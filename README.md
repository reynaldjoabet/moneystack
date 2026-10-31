# moneystack

Accounting, investments and budgeting on Scala 3, http4s, skunk and Postgres.

## Running

Needs Postgres. Migrations run on startup.

```sh
DB_USER=moneystack DB_NAME=moneystack sbt --client run
```

| Variable | Default | |
|---|---|---|
| `DB_HOST` / `DB_PORT` | `localhost` / `5432` | |
| `DB_USER`, `DB_NAME` | required | |
| `DB_PASSWORD` | none | |
| `DB_POOL_MAX` | `8` | session pool size |
| `DB_MIGRATE_ON_START` | `true` | apply pending Flyway migrations before serving |
| `HTTP_HOST` / `HTTP_PORT` | `0.0.0.0` / `8080` | |
| `LEDGER_BASE_CURRENCY` | `USD` | reporting currency |

Health probes: `GET /health/live` (process up) and `GET /health/ready` (503 until Postgres answers).

## Developing

```sh
sbt --client test              # unit tests
sbt --client integrationTest   # Testcontainers suites (needs Docker)
sbt --client lint              # scalafmt + scalafix checks, as CI runs them
sbt --client fmt               # fix formatting and unused code
sbt --client cov               # coverage report
```

See [CLAUDE.md](CLAUDE.md) for module layout and conventions.
