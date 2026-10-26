scalaVersion := "3.9.0"

lazy val root = rootProject
  .settings(
    name := "LordOfTheWings_2026",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.20" % Test,
    )
  )
