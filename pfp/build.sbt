name := "pfp"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "dev.profunktor" %% "console4cats" % "0.8.0",
  "io.estatico" %% "newtype" % "0.4.3",
  "eu.timepit" %% "refined" % "0.9.10"
)

scalacOptions += "-Ymacro-annotations"
