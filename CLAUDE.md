# CLAUDE.md

Guidance for Claude Code (and other agents) working in this repository.

## Project

moneystack: personal and small-business finance on Scala 3 + Typelevel. One http4s app (root project)
mounts three feature modules, each with its own Postgres schema and routes.

| Module | Package | What it holds |
|---|---|---|
| `modules/core` | `moneystack.domain` | iron refined scalar types shared by everything |
| `modules/platform` | `moneystack.platform` | skunk codecs, `Db.txE` transactions, `HttpSupport`, FX, CSV, Flyway `Migrations` |
| `modules/testkit` | `moneystack.testkit` | Testcontainers Postgres + `PostgresSuite` (test-only) |
| `modules/accounting` | `moneystack.accounting` | double-entry ledger, documents, inventory, banking, reconciliation |
| `modules/portfolio` | `moneystack.portfolio` | investments, performance, goals, FIRE, net worth (schema `portfolio`) |
| `modules/budget` | `moneystack.budget` | wallets, transactions, budgets, schedules, rules (schema `budget`) |
| root | `moneystack` | users/auth, model portfolios, subscriptions; `Main` is the only composition root |

Feature modules depend on `platform` only, never on each other or on the root app. Each exposes a
`XxxModule.make(pool, currency)` and `routes(auth)`; `Main` wires them.

## Commands

Always use the sbt thin client, `sbt --client "<command>"` — never plain `sbt` or `sbt -batch`.
Restart it (`sbt --client shutdown`) or `reload` after editing `build.sbt` or `project/`.
For a clean slate use `cleanFull`, not `clean`: sbt 2's disk cache survives `clean`.

| Alias | Runs |
|---|---|
| `test` | unit suites only (no Docker) |
| `integrationTest` / `Integration/test` | Testcontainers suites (any suite in an `it` package); needs Docker |
| `testAll` | both tiers |
| `fmt` | scalafix `RemoveUnused`, then scalafmt on sources and `.sbt` files |
| `fmtCheck` / `lint` | CI checks: formatting / formatting + `scalafixAll --check` |
| `cov` | scoverage over both tiers; report at `target/out/jvm/scala-3.9.0/moneystack/scoverage-report/` |
| `dependencyUpdates` | lists newer versions of dependencies (sbt-updates; changes nothing) |

Single suite: `sbt --client "accounting/Integration/testOnly *LedgerIntegrationSuite"`.

## Conventions

- Compiler flags include `-Wunused:all`, `-Wvalue-discard`, `-Wnonunit-statement`: no new warnings.
- Style is scalafmt with `newlines.source = keep` and braces kept; run `fmt` before committing.
- Refined types live in `core`; construct with `.either` for untrusted input, `.assume`/`applyUnsafe` only for trusted data.
- Domain failures are `Either[E, A]` values, not exceptions; `Db.txE` rolls a transaction back on `Left`.
- Money is `BigDecimal` (accounting, portfolio) or minor units (budget); never `Double`.
- New Postgres-backed tests go in an `it` package and extend `moneystack.testkit.PostgresSuite`.
- Pure logic gets a unit suite outside `it` so `test` stays fast.

## Database migrations

- Flyway, run by `Main` at startup (`DB_MIGRATE_ON_START=false` to skip), and by `PostgresSuite` in tests.
- Scripts: `db/migration/<module>/V<n>__<title>.sql` in that module's resources (`app` for the root).
  Each module has its own history table, `flyway_history_<module>`, so versions are per module.
- Never edit a migration once it has been deployed — not even a comment (Flyway checksums the file).
  Fix mistakes with a new version.
- Keep `V1` scripts idempotent: databases created before Flyway adopt them by re-running V1.

## Configuration

`AppConfig` reads environment variables with ciris and reports every problem at once. Name boolean
flags positively (`enableXxx`, `migrateOnStart`), never `disableXxx`. Secrets are `ciris.Secret`.

## Versions

- Scala: LTS line only (currently 3.9.0). Java: 21 and 25 in CI.
- Dependency versions live in `project/Dependencies.scala` (`Versions.*`), not inline in `build.sbt`.

## Git

Never create branches or open pull requests; commit only when asked, on the current branch.
