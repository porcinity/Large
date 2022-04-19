lazy val scala3Version  = "3.1.1"
lazy val DoobieVersion  = "1.0.0-RC1"
lazy val CirceVersion   = "0.14.1"
lazy val Http4sVersion  = "0.23.11"
lazy val JUnitVersion   = "0.13.3"
lazy val LogbackVersion = "1.2.11"

lazy val root = project
  .in(file("."))
  .settings(
    name                 := "Large",
    version              := "0.1.0-SNAPSHOT",
    cancelable in Global := true,
    scalaVersion         := scala3Version,
    libraryDependencies ++= Seq(
      "com.github.sbt"         % "junit-interface"        % JUnitVersion % Test,
      "org.tpolecat"          %% "doobie-core"            % DoobieVersion,
      "org.tpolecat"          %% "doobie-postgres"        % DoobieVersion,
      "org.tpolecat"          %% "doobie-hikari"          % DoobieVersion,
      "org.http4s"            %% "http4s-ember-server"    % Http4sVersion,
      "org.http4s"            %% "http4s-circe"           % Http4sVersion,
      "org.http4s"            %% "http4s-dsl"             % Http4sVersion,
      "io.circe"              %% "circe-core"             % CirceVersion,
      "io.circe"              %% "circe-generic"          % CirceVersion,
      "com.github.daddykotex" %% "courier"                % "3.1.0",
      "org.latestbit"         %% "circe-tagged-adt-codec" % "0.10.1",
      "com.aventrix.jnanoid"   % "jnanoid"                % "2.0.0",
      "dev.optics"            %% "monocle-core"           % "3.1.0",
      "dev.optics"            %% "monocle-macro"          % "3.1.0",
      "dev.optics"            %% "monocle-refined"        % "3.1.0",
      "org.tpolecat"          %% "skunk-core"             % "0.3.1",
      "eu.timepit"            %% "refined"                % "0.9.28",
      "eu.timepit"            %% "refined-cats"           % "0.9.28",
      "io.circe"              %% "circe-refined"          % CirceVersion,
//      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
      "ch.qos.logback" % "logback-classic" % LogbackVersion % Runtime
    )
  )
