name := "EffLiftApp"

scalaVersion := "2.12.6"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.1-SNAPSHOT"

scalacOptions += "-Ypartial-unification"
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
