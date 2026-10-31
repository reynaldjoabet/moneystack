import Dependencies.Versions

ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "io.moneystack"
ThisBuild / version      := "0.1.0"

// scalafix's RemoveUnused reads the compiler's unused warnings from SemanticDB.
ThisBuild / semanticdbEnabled := true

addCommandAlias("fmt", "scalafixAll; scalafmtSbt; scalafmtAll")
addCommandAlias("fmtCheck", "scalafmtSbtCheck; scalafmtCheckAll")
addCommandAlias("lint", "fmtCheck; scalafixAll --check")
addCommandAlias("integrationTest", "Integration/test")
addCommandAlias("testAll", "test; Integration/test")
addCommandAlias("cov", "coverage; test; Integration/test; coverageAggregate; coverageOff")

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-encoding",
    "UTF-8",
    "-no-indent",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Wunused:all",
    "-Wvalue-discard",
    "-Wnonunit-statement",
    "-Xkind-projector",
    "-Xmax-inlines",
    "64"
  ),
  // sbt 2 writes SemanticDB to a separate `meta/` directory, but sbt-scalafix only reads `classDirectory`.
  Compile / semanticdbTargetRoot := (Compile / classDirectory).value,
  Test / semanticdbTargetRoot    := (Test / classDirectory).value
)

/**
 * Suites in an `it` package need Docker (Testcontainers): `test` skips them and
 * `Integration/test` runs only them, from the same test sources.
 */
lazy val Integration = config("integration").extend(Test)

def isIntegration(suite: String): Boolean = suite.contains(".it.")

lazy val integrationSettings = inConfig(Integration)(Defaults.testTasks) ++ Seq(
  Test / testOptions += Tests.Filter(suite => !isIntegration(suite)),
  // Integration's options start from Test's, so drop Test's unit-only filter rather than add to it.
  Integration / testOptions := (Test / testOptions).value.filterNot(_.isInstanceOf[Tests.Filter]) :+ Tests.Filter(isIntegration)
)

lazy val testDeps = Seq(
  "org.scalameta" %% "munit"             % Versions.munit   % Test,
  "org.typelevel" %% "munit-cats-effect" % Versions.munitCE % Test
)

/** Refined scalar types shared by every module. */
lazy val core = (project in file("modules/core"))
  .settings(
    commonSettings,
    name := "moneystack-core",
    libraryDependencies ++= Seq(
      "io.github.iltotore" %% "iron" % Versions.iron
    )
  )

/** Skunk codecs for core types, transactions, and route plumbing shared by the feature modules. */
lazy val platform = (project in file("modules/platform"))
  .dependsOn(core)
  .settings(
    commonSettings,
    name := "moneystack-platform",
    libraryDependencies ++= Seq(
      "io.github.iltotore" %% "iron-skunk"                 % Versions.iron,
      "io.github.iltotore" %% "iron-circe"                 % Versions.iron,
      "org.tpolecat"       %% "skunk-core"                 % Versions.skunk,
      "org.typelevel"      %% "cats-effect"                % Versions.ce,
      "org.http4s"         %% "http4s-dsl"                 % Versions.http4s,
      "org.http4s"         %% "http4s-circe"               % Versions.http4s,
      "io.circe"           %% "circe-core"                 % Versions.circe,
      "org.flywaydb"        % "flyway-database-postgresql" % Versions.flyway,
      "org.postgresql"      % "postgresql"                 % Versions.postgresJdbc
    )
  )

/** Test-only: Testcontainers Postgres fixture and assertions shared by the modules' integration suites. */
lazy val testkit = (project in file("modules/testkit"))
  .dependsOn(platform)
  .settings(
    commonSettings,
    name            := "moneystack-testkit",
    coverageEnabled := false,
    libraryDependencies ++= Seq(
      "org.tpolecat"      %% "skunk-core"                % Versions.skunk,
      "org.typelevel"     %% "munit-cats-effect"         % Versions.munitCE,
      "org.testcontainers" % "testcontainers-postgresql" % Versions.testcontainers,
      "ch.qos.logback"     % "logback-classic"           % Versions.logback
    )
  )

lazy val featureModule = Seq(
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-generic" % Versions.circe,
    "io.circe" %% "circe-parser"  % Versions.circe
  ) ++ testDeps,
  // Testcontainers' containers are only removed when the JVM exits: forking gives each run its own
  // JVM, so they don't pile up in the long-running sbt server. Integration inherits this from Test.
  Test / fork := true
) ++ integrationSettings

/** General ledger, sales & purchases, inventory, banking and statements (after bigcapital and Akaunting). */
lazy val accounting = (project in file("modules/accounting"))
  .dependsOn(platform, testkit % Test)
  .configs(Integration)
  .settings(commonSettings, featureModule, name := "moneystack-accounting")

/** Investment tracking, net worth and planning (after Wealthfolio, Picsou and MoneyMatter). */
lazy val portfolio = (project in file("modules/portfolio"))
  .dependsOn(platform, testkit % Test)
  .configs(Integration)
  .settings(commonSettings, featureModule, name := "moneystack-portfolio")

/** Personal budgeting: wallets, transactions, budgets, schedules, goals, rules (after Cashew, Expense Manager and MoneyMatter). */
lazy val budget = (project in file("modules/budget"))
  .dependsOn(platform, testkit % Test)
  .configs(Integration)
  .settings(commonSettings, featureModule, name := "moneystack-budget")

lazy val root = (project in file("."))
  .dependsOn(core, accounting, portfolio, budget, testkit % Test)
  .aggregate(core, platform, testkit, accounting, portfolio, budget)
  .configs(Integration)
  .settings(
    commonSettings,
    integrationSettings,
    name := "moneystack",
    // Fork for the same reason as featureModule: Testcontainers cleans up only on JVM exit.
    Test / fork := true,
    libraryDependencies ++= Seq(
      "io.github.iltotore" %% "iron"                % Versions.iron,
      "io.github.iltotore" %% "iron-skunk"          % Versions.iron,
      "io.github.iltotore" %% "iron-circe"          % Versions.iron,
      "org.tpolecat"       %% "skunk-core"          % Versions.skunk,
      "org.http4s"         %% "http4s-ember-server" % Versions.http4s,
      "org.http4s"         %% "http4s-ember-client" % Versions.http4s,
      "org.http4s"         %% "http4s-circe"        % Versions.http4s,
      "org.http4s"         %% "http4s-dsl"          % Versions.http4s,
      "io.circe"           %% "circe-core"          % Versions.circe,
      "io.circe"           %% "circe-generic"       % Versions.circe,
      "io.circe"           %% "circe-parser"        % Versions.circe,
      "org.typelevel"      %% "cats-effect"         % Versions.ce,
      "is.cir"             %% "ciris"               % Versions.ciris,
      "org.typelevel"      %% "log4cats-slf4j"      % Versions.log4cats,
      "ch.qos.logback"      % "logback-classic"     % Versions.logback,
      ("com.github.t3hnar" %% "scala-bcrypt"        % Versions.bcrypt).cross(CrossVersion.for3Use2_13)
    ) ++ testDeps
  )
