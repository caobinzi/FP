name := "EffLiftApp"

scalaVersion := "2.12.0"

libraryDependencies += "org.atnos" % "eff-scalaz_2.12" % "5.0.0-20170929100326-b526890"
libraryDependencies += "org.atnos" % "eff_2.12" % "5.0.0-20170929100326-b526890"
//libraryDependencies += "org.atnos" % "eff_2.12" % "5.0.0-20170929100326-b526890"


scalacOptions += "-Ypartial-unification"
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
