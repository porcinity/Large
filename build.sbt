val scala3Version = "3.1.1"
val DoobieVersion = "1.0.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "doobie-http4s",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

  libraryDependencies ++= Seq(
    "org.tpolecat" %% "doobie-core"     % DoobieVersion,
    "org.tpolecat" %% "doobie-postgres" % DoobieVersion,
    "org.tpolecat" %% "doobie-hikari"   % DoobieVersion,
  )
)
