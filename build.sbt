ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "io.wealth"
ThisBuild / version      := "0.1.0"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-no-indent",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Wunused:all",
    "-Wvalue-discard",
    "-Wnonunit-statement",
    "-Xkind-projector",
    "-Xmax-inlines", "64"
  )
)

lazy val testDeps = Seq(
  "org.scalameta" %% "munit"             % V.munit   % Test,
  "org.typelevel" %% "munit-cats-effect" % V.munitCE % Test
)

/** Refined scalar types shared by every module. */
lazy val core = (project in file("modules/core"))
  .settings(
    commonSettings,
    name := "wealth-core",
    libraryDependencies ++= Seq(
      "io.github.iltotore" %% "iron" % V.iron
    )
  )

/** Skunk codecs for core types, transactions, and route plumbing shared by the feature modules. */
lazy val platform = (project in file("modules/platform"))
  .dependsOn(core)
  .settings(
    commonSettings,
    name := "wealth-platform",
    libraryDependencies ++= Seq(
      "io.github.iltotore" %% "iron-skunk"   % V.iron,
      "io.github.iltotore" %% "iron-circe"   % V.iron,
      "org.tpolecat"       %% "skunk-core"   % V.skunk,
      "org.typelevel"      %% "cats-effect"  % V.ce,
      "org.http4s"         %% "http4s-dsl"   % V.http4s,
      "org.http4s"         %% "http4s-circe" % V.http4s,
      "io.circe"           %% "circe-core"   % V.circe
    )
  )

/** Test-only: Testcontainers Postgres fixture and assertions shared by the modules' integration suites. */
lazy val testkit = (project in file("modules/testkit"))
  .settings(
    commonSettings,
    name := "wealth-testkit",
    libraryDependencies ++= Seq(
      "org.tpolecat"       %% "skunk-core"                % V.skunk,
      "org.typelevel"      %% "munit-cats-effect"         % V.munitCE,
      "org.testcontainers" %  "testcontainers-postgresql" % V.testcontainers,
      "ch.qos.logback"     %  "logback-classic"           % V.logback
    )
  )

lazy val featureModule = Seq(
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-generic" % V.circe,
    "io.circe" %% "circe-parser"  % V.circe
  ) ++ testDeps,
  Test / fork := true
)

/** General ledger, sales & purchases, inventory, banking and statements (after bigcapital and Akaunting). */
lazy val accounting = (project in file("modules/accounting"))
  .dependsOn(platform, testkit % Test)
  .settings(commonSettings, featureModule, name := "wealth-accounting")

/** Investment tracking, net worth and planning (after Wealthfolio, Picsou and MoneyMatter). */
lazy val portfolio = (project in file("modules/portfolio"))
  .dependsOn(platform, testkit % Test)
  .settings(commonSettings, featureModule, name := "wealth-portfolio")

/** Personal budgeting: wallets, transactions, budgets, schedules, goals, rules (after Cashew, Expense Manager and MoneyMatter). */
lazy val budget = (project in file("modules/budget"))
  .dependsOn(platform, testkit % Test)
  .settings(commonSettings, featureModule, name := "wealth-budget")

lazy val root = (project in file("."))
  .dependsOn(core, accounting, portfolio, budget)
  .aggregate(core, platform, testkit, accounting, portfolio, budget)
  .settings(
    commonSettings,
    name := "wealth",
    libraryDependencies ++= Seq(
      "io.github.iltotore"    %% "iron"                % V.iron,
      "io.github.iltotore"    %% "iron-skunk"          % V.iron,
      "io.github.iltotore"    %% "iron-circe"          % V.iron,
      "org.tpolecat"          %% "skunk-core"          % V.skunk,
      "org.http4s"            %% "http4s-ember-server" % V.http4s,
      "org.http4s"            %% "http4s-ember-client" % V.http4s,
      "org.http4s"            %% "http4s-circe"        % V.http4s,
      "org.http4s"            %% "http4s-dsl"          % V.http4s,
      "io.circe"              %% "circe-core"          % V.circe,
      "io.circe"              %% "circe-generic"       % V.circe,
      "io.circe"              %% "circe-parser"        % V.circe,
      "org.typelevel"         %% "cats-effect"         % V.ce,
      "org.typelevel"         %% "log4cats-slf4j"      % V.log4cats,
      "ch.qos.logback"        %  "logback-classic"     % V.logback,
      ("com.github.t3hnar"    %% "scala-bcrypt"        % V.bcrypt).cross(CrossVersion.for3Use2_13)
    ) ++ testDeps
  )
