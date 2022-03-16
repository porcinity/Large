lazy val scala3Version = "3.1.1"
lazy val DoobieVersion = "1.0.0-RC1"
lazy val CirceVersion = "0.14.1"
lazy val Http4sVersion = "0.23.10"
lazy val JUnitVersion = "0.13.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "doobie-http4s",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.github.sbt" % "junit-interface"      % JUnitVersion      % Test,
      "org.tpolecat"   %% "doobie-core"         % DoobieVersion,
      "org.tpolecat"   %% "doobie-postgres"     % DoobieVersion,
      "org.tpolecat"   %% "doobie-hikari"       % DoobieVersion,

      "org.http4s"     %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"     %% "http4s-circe"        % Http4sVersion,
      "org.http4s"     %% "http4s-dsl"          % Http4sVersion,

      "io.circe"       %% "circe-generic"       % CirceVersion,

      "com.lorandszakacs" %% "sprout" % "0.0.5",
    )
)
