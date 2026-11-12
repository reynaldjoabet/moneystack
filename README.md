<img src="docs/brand/logo-mark.svg" alt="" width="64">

# moneystack

Accounting, investments and budgeting on Scala 3, http4s, skunk and Postgres. The [proposed UI](docs/README.md)
is in `docs/`.

## Running

Needs Postgres and a signing secret for access tokens. Migrations run on startup.

```sh
DB_USER=moneystack DB_NAME=moneystack AUTH_JWT_SECRET=$(openssl rand -base64 48) sbt --client run
```

Or build the image and run it next to Postgres:

```sh
sbt --client Docker/publishLocal   # builds moneystack:latest (runs as a non-root user)
docker compose up                  # Postgres + the app on :8080; POSTGRES_PORT=55432 if 5432 is taken
```

### Configuration

Every setting is an environment variable. All problems are reported at once on startup.

| Variable | Default | |
|---|---|---|
| `DB_HOST` / `DB_PORT` | `localhost` / `5432` | |
| `DB_USER`, `DB_NAME` | required | |
| `DB_PASSWORD` | none | |
| `DB_SSL` | `disable` | `trusted` encrypts without checking the certificate; `system` also verifies it and its hostname against `DB_HOST`, using the JVM trust store |
| `DB_POOL_MAX` | `8` | session pool size |
| `DB_STATEMENT_TIMEOUT` | `30 seconds` | Postgres cancels longer statements (migrations are exempt) |
| `DB_IDLE_IN_TRANSACTION_TIMEOUT` | `60 seconds` | Postgres ends sessions left idle inside a transaction |
| `DB_MIGRATE_ON_START` | `true` | apply pending Flyway migrations before serving |
| `AUTH_JWT_SECRET` | required | HS256 signing key, at least 32 bytes; rotating it signs everyone out |
| `AUTH_TOKEN_TTL` | `1 hour` | how long an access token is valid |
| `AUTH_ISSUER` | `moneystack` | `iss` claim, checked on every token |
| `AUTH_PASSWORD_RESET_TTL` | `30 minutes` | how long a password reset token works |
| `HTTP_HOST` / `HTTP_PORT` | `0.0.0.0` / `8080` | |
| `HTTP_REQUEST_TIMEOUT` | `30 seconds` | answered 503 past this |
| `HTTP_IDLE_TIMEOUT` | `60 seconds` | idle keep-alive connections are closed |
| `HTTP_SHUTDOWN_TIMEOUT` | `30 seconds` | time in-flight requests get to finish on SIGTERM |
| `HTTP_MAX_BODY_BYTES` | `1048576` | larger request bodies are answered 413 |
| `HTTP_CORS_ORIGINS` | none | comma-separated origins allowed to call the API from a browser |
| `HTTP_DRAIN_DELAY` | `5 seconds` | on SIGTERM, readiness reports 503 this long before connections stop being accepted |
| `TELEMETRY_ENABLED` | `false` | export traces and metrics over OTLP (see below) |
| `LOG_FORMAT` | `text` | `json` for one JSON object per line |
| `LOG_LEVEL` | `INFO` | root log level |
| `LEDGER_BASE_CURRENCY` | `USD` | reporting currency |

Health probes: `GET /health/live` (process up) and `GET /health/ready` (503 until Postgres answers).

### Authentication

`POST /auth/signup` and `POST /auth/login` answer with a bearer token:

```json
{ "token": "eyJ…", "tokenType": "Bearer", "expiresAt": "2026-11-05T06:41:12Z", "user": { "id": "…", "role": "investor" } }
```

Send it as `Authorization: Bearer <token>` on every other call. A missing, expired or forged token is
401; a valid token for the wrong role is 403. Roles are read from the database on each request, so a role
change applies at once. Portfolios are visible to their owner only; anyone else gets 404.

Every new account is an investor. An admin changes roles with `PUT /admin/users/{id}/role` and
`{"role": "analyst" | "accountant" | "admin" | "investor"}`. The first admin is promoted in SQL:

```sql
UPDATE users SET role = 'admin' WHERE email = 'you@example.com';
```

Passwords need 12 to 72 bytes (bcrypt reads no further). `POST /auth/password` with
`{"currentPassword", "newPassword"}` changes it and `POST /auth/logout-all` signs out every device: both
revoke every token issued before. The password change answers a new token for the caller.

For a forgotten password, `POST /auth/password/forgot` with `{"email"}` always answers 202 and, if the account
exists, emails it a one-time token. `POST /auth/password/reset` with `{"token", "newPassword"}` sets the password,
revokes earlier tokens and answers a new one. A token works once; asking again replaces it.

### Observability

With `TELEMETRY_ENABLED=true` the app exports traces (one span per request and per SQL statement) and
metrics (HTTP server, cats-effect runtime) over OTLP. The exporter reads the standard OpenTelemetry
variables, for example:

```sh
TELEMETRY_ENABLED=true OTEL_EXPORTER_OTLP_ENDPOINT=http://collector:4317 OTEL_SERVICE_NAME=moneystack
```

Every response carries an `X-Request-ID`. A 500's body gives only that id, and the log line with the
cause carries it too.

### In production

- Terminate TLS in front of the app (load balancer or ingress), and set `DB_SSL=system` when Postgres is remote.
- Inject `AUTH_JWT_SECRET` and `DB_PASSWORD` from a secret store, never from the image or the repo.
- Email, push, Telegram, payments and market data are sandboxes that only log. Replace `Mailer.sandbox` with a
  real mail client before going live: it writes working password reset tokens to the log.
- The app does not limit login or password reset attempts: rate-limit `/auth/login` and `/auth/password/*` at the load balancer or gateway
  (`deploy/ratelimit.yaml` does this on Envoy Gateway).
- Point liveness at `/health/live` and readiness at `/health/ready`. Give the pod a termination grace
  period longer than `HTTP_DRAIN_DELAY` plus `HTTP_SHUTDOWN_TIMEOUT`. Probes are left out of the access log.
- The session timeouts are sent as Postgres startup parameters. PgBouncer refuses connections carrying
  them unless they are in its `ignore_startup_parameters`: list them there and set the timeouts on the
  database role instead (`ALTER ROLE … SET statement_timeout = '30s'`).
- Replicas that start together are safe: Flyway holds a Postgres advisory lock, so one of them migrates and
  the others wait. For long migrations, run the app once as a job, then deploy with `DB_MIGRATE_ON_START=false`.
- The image applies Ubuntu's security updates on top of the Temurin base when it is built, and CI fails
  on any fixable high or critical vulnerability in it (Trivy). Dependabot opens weekly PRs for the Scala
  dependencies, the Actions and the compose images; Scala itself stays on the LTS line and is bumped by hand.
- The JVM sizes its heap to 60% of the container's memory limit and exits on `OutOfMemoryError`, so the
  orchestrator restarts it. The rest is for memory outside the heap: about 215 MiB under load, plus short peaks.

### Deploying to Kubernetes

[deploy/](deploy/) holds a Kustomize app for a cluster with a shared Gateway. Use one gateway, not both:
Envoy Gateway (`kubectl apply -k deploy`, expects Gateway `infra/public`) or HAProxy Unified Gateway
(`kubectl apply -k deploy/haproxy`; [deploy/haproxy/platform/](deploy/haproxy/platform/) installs the gateway).
Everything else is shared, in [deploy/base/](deploy/base/). Before applying:

1. Build and push an image, then set it in `deploy/base/kustomization.yaml`. CI doesn't push one yet.
2. Set `DB_HOST` in `deploy/base/deployment.yaml` to the database's real hostname. With `DB_SSL=system` the
   certificate must name that host and chain to the JVM trust store, so a private CA needs a trust store in
   the image, or in `JAVA_OPTS` with a mounted file.
3. Set the host in the `httproute.yaml` of the gateway you use (both routes).
4. Create the Secret: `kubectl create namespace moneystack`, then the command in that gateway's `kustomization.yaml`.
5. Make sure the gateway sees real client IPs, or all users behind the load balancer share one login rate
   limit. On Envoy, set `clientIPDetection` or PROXY protocol in a `ClientTrafficPolicy` on the Gateway. On
   HAProxy, see `deploy/haproxy/platform/values.yaml`.

`scripts/certs.sh` creates a private CA, signs server certificates and builds that trust store; run it with
no arguments for usage. Keys go to `certs/`, which git ignores. Browsers don't trust this CA, so use it for
internal connections such as Postgres, and a public CA for `money.example.com`.

The database role must be able to run the migrations, or use `DB_MIGRATE_ON_START=false` and run them separately.
Each Pod needs 1 GiB of memory and opens up to `DB_POOL_MAX` Postgres connections: 8 per Pod by default.

The rate limit is 10 requests a minute per client IP, counted per gateway replica, so 3 replicas allow 30.
On HAProxy a blocked client recovers about 2 minutes after it stops retrying.

On HAProxy, keep the chart's 2560Mi memory limit: below it HAProxy can exit at startup. After adding a
`gateway-access` label to an existing namespace, restart the controller: it doesn't notice the change. The
chart's `stat` Service port is 31024, but HAProxy serves stats and metrics on 1024, so scrape the Pod's port 1024.

## Developing

```sh
sbt --client test              # unit tests
sbt --client integrationTest   # Testcontainers suites (needs Docker)
sbt --client lint              # scalafmt + scalafix checks, as CI runs them
sbt --client fmt               # fix formatting and unused code
sbt --client cov               # coverage report
```

Compiler warnings fail the build in CI (`-Werror` when `CI=true`), not locally. `git blame` skips formatting-only
commits listed in `.git-blame-ignore-revs`; run `git config blame.ignoreRevsFile .git-blame-ignore-revs` once.

See [CLAUDE.md](CLAUDE.md) for module layout and conventions.
