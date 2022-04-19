lazy val scala3Version  = "3.1.1"
lazy val DoobieVersion  = "1.0.0-RC1"
lazy val CirceVersion   = "0.14.1"
lazy val Http4sVersion  = "0.23.11"
lazy val JUnitVersion   = "0.13.3"
lazy val LogbackVersion = "1.2.11"
lazy val MonocleVersion = "3.1.0"
lazy val RefinedVersion = "0.9.28"
lazy val SkunkVersion   = "0.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name                 := "Large",
    version              := "0.1.0-SNAPSHOT",
    cancelable in Global := true,
    scalaVersion         := scala3Version,
    libraryDependencies ++= Seq(
      "com.github.sbt"         % "junit-interface"        % JUnitVersion % Test,
      "org.http4s"            %% "http4s-ember-server"    % Http4sVersion,
      "org.http4s"            %% "http4s-circe"           % Http4sVersion,
      "org.http4s"            %% "http4s-dsl"             % Http4sVersion,
      "io.circe"              %% "circe-core"             % CirceVersion,
      "io.circe"              %% "circe-generic"          % CirceVersion,
      "io.circe"              %% "circe-refined"          % CirceVersion,
      "com.github.daddykotex" %% "courier"                % "3.1.0",
      "org.latestbit"         %% "circe-tagged-adt-codec" % "0.10.1",
      "com.aventrix.jnanoid"   % "jnanoid"                % "2.0.0",
      "dev.optics"            %% "monocle-core"           % MonocleVersion,
      "dev.optics"            %% "monocle-macro"          % MonocleVersion,
      "dev.optics"            %% "monocle-refined"        % MonocleVersion,
      "org.tpolecat"          %% "skunk-core"             % SkunkVersion,
      "eu.timepit"            %% "refined"                % RefinedVersion,
      "eu.timepit"            %% "refined-cats"           % RefinedVersion,
      "ch.qos.logback" % "logback-classic" % LogbackVersion % Runtime
    )
  )
