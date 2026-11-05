package moneystack

import moneystack.config.DbSsl

import javax.net.ssl.SNIHostName

class DbSslSuite extends munit.FunSuite {

  test("DB_SSL=system checks the certificate's hostname, not only its issuer") {
    val params = Main.sslFor(DbSsl.System, "db.example.com").tlsParameters
    assertEquals(params.endpointIdentificationAlgorithm, Some("HTTPS"))
    assertEquals(params.serverNames.toList.flatten.map(_.getEncoded.toList), List(new SNIHostName("db.example.com").getEncoded.toList))
  }
}
