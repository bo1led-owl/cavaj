val scala3Version = "3.7.3"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "cavaj",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.ow2.asm"    % "asm"   % "9.7",
    ),
  )
