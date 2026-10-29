ThisBuild / scalaVersion := "3.10.0"
ThisBuild / organization := "io.wealth"
ThisBuild / version      := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "wealth",
    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-no-indent",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Wunused:all",
      "-Wvalue-discard",
      "-Wnonunit-statement",
      "-Ykind-projector",
      "-Xmax-inlines", "64"
    ),
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
      ("com.github.t3hnar"    %% "scala-bcrypt"        % V.bcrypt).cross(CrossVersion.for3Use2_13),
      "org.scalameta"         %% "munit"               % V.munit   % Test,
      "org.typelevel"         %% "munit-cats-effect"   % V.munitCE % Test
    )
  )
