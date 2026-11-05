#!/usr/bin/env bash
#
# A private CA for moneystack, and server certificates signed by it.
#
#   scripts/certs.sh ca [name]                  create the CA (once; it refuses to replace one)
#   scripts/certs.sh sign <name> <host>...      sign a server certificate for DNS names or IP addresses
#   scripts/certs.sh truststore                 build a JVM trust store holding the CA, for DB_SSL=system
#
# For example:
#
#   scripts/certs.sh ca "moneystack internal CA"
#   scripts/certs.sh sign postgres moneystack-db.internal.example.com
#   scripts/certs.sh sign gateway "*.example.com"
#   scripts/certs.sh truststore
#
# Files go to ./certs (or $CERTS_DIR), which git ignores. Keep ca.key offline: whoever holds it can issue a
# certificate for any name your app trusts. Browsers don't trust this CA, so public hosts need a public one.
#
# Settings: CERTS_DIR, CA_DAYS (3650), CERT_DAYS (397), TRUSTSTORE_PASSWORD (changeit: a trust store holds
# nothing secret, so this only satisfies the format).

set -euo pipefail
umask 077

CERTS_DIR="${CERTS_DIR:-certs}"
CA_DAYS="${CA_DAYS:-3650}"
CERT_DAYS="${CERT_DAYS:-397}"
TRUSTSTORE_PASSWORD="${TRUSTSTORE_PASSWORD:-changeit}"

CA_CERT="$CERTS_DIR/ca.crt"
CA_KEY="$CERTS_DIR/ca.key"

die() {
  echo "error: $*" >&2
  exit 1
}

usage() {
  sed -n '3,8p' "$0" | sed 's/^# \{0,1\}//' >&2
  exit 2
}

require_ca() {
  [[ -f "$CA_CERT" && -f "$CA_KEY" ]] || die "no CA in $CERTS_DIR: run '$0 ca' first"
}

new_key() {
  openssl ecparam -name prime256v1 -genkey -noout -out "$1"
}

create_ca() {
  local name="${1:-moneystack internal CA}"
  [[ ! -e "$CA_KEY" && ! -e "$CA_CERT" ]] ||
    die "$CERTS_DIR already holds a CA: replacing it would invalidate every certificate it signed"

  mkdir -p "$CERTS_DIR"
  new_key "$CA_KEY"
  local ext
  ext="$(mktemp)"
  trap 'rm -f "$ext"' RETURN
  cat >"$ext" <<EOF
[req]
distinguished_name = dn
x509_extensions = ca
prompt = no
[dn]
CN = $name
[ca]
basicConstraints = critical, CA:TRUE, pathlen:0
keyUsage = critical, keyCertSign, cRLSign
subjectKeyIdentifier = hash
EOF
  openssl req -x509 -new -sha256 -days "$CA_DAYS" -key "$CA_KEY" -config "$ext" -out "$CA_CERT"
  chmod 644 "$CA_CERT"
  echo "created $CA_CERT and $CA_KEY, valid for $CA_DAYS days"
}

san_list() {
  local host sans=""
  for host in "$@"; do
    # An address needs an IP entry: a client won't match it against a DNS one.
    if [[ "$host" =~ ^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$ || "$host" == *:* ]]; then
      sans+="IP:$host,"
    else
      sans+="DNS:$host,"
    fi
  done
  echo "${sans%,}"
}

sign() {
  [[ $# -ge 2 ]] || usage
  local name="$1"
  shift
  [[ "$name" =~ ^[A-Za-z0-9._-]+$ ]] || die "name must be letters, digits, '.', '_' or '-': $name"
  require_ca

  local key="$CERTS_DIR/$name.key" crt="$CERTS_DIR/$name.crt"
  local sans
  sans="$(san_list "$@")"

  [[ -f "$key" ]] || new_key "$key" # re-signing keeps the key, so a renewal changes only the certificate

  local dir
  dir="$(mktemp -d)"
  trap 'rm -rf "$dir"' RETURN
  cat >"$dir/ext" <<EOF
basicConstraints = critical, CA:FALSE
keyUsage = critical, digitalSignature
extendedKeyUsage = serverAuth
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid, issuer
subjectAltName = $sans
EOF
  openssl req -new -sha256 -key "$key" -subj "/CN=$1" -out "$dir/csr"
  openssl x509 -req -sha256 -days "$CERT_DAYS" -in "$dir/csr" -CA "$CA_CERT" -CAkey "$CA_KEY" \
    -set_serial "0x$(openssl rand -hex 16)" -extfile "$dir/ext" -out "$crt" 2>/dev/null
  chmod 644 "$crt"

  openssl verify -CAfile "$CA_CERT" "$crt" >/dev/null || die "$crt does not verify against $CA_CERT"
  echo "signed $crt for $sans, valid for $CERT_DAYS days"
}

truststore() {
  require_ca
  command -v keytool >/dev/null || die "keytool not found: it comes with the JDK"
  local store="$CERTS_DIR/truststore.p12"
  rm -f "$store"
  keytool -importcert -noprompt -alias moneystack-ca -file "$CA_CERT" -keystore "$store" \
    -storetype PKCS12 -storepass "$TRUSTSTORE_PASSWORD" 2>/dev/null
  chmod 644 "$store"
  echo "created $store"
  echo "run the app with JAVA_OPTS=\"-Djavax.net.ssl.trustStore=<mounted path>" \
    "-Djavax.net.ssl.trustStorePassword=$TRUSTSTORE_PASSWORD -Djavax.net.ssl.trustStoreType=PKCS12\""
}

command -v openssl >/dev/null || die "openssl not found"

case "${1:-}" in
  ca)
    shift
    create_ca "$@"
    ;;
  sign)
    shift
    sign "$@"
    ;;
  truststore) truststore ;;
  *) usage ;;
esac
