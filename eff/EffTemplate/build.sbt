name := "EffLiftApp"

scalaVersion := "2.12.0"

libraryDependencies += "org.atnos" % "eff_2.12" % "5.2.0"

scalacOptions += "-Ypartial-unification"
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
