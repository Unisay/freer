name := "freer"

version in ThisBuild := "0.0.1"

scalaVersion in ThisBuild := "2.12.1"

libraryDependencies += "org.typelevel" %% "cats-core" % "0.9.0"
libraryDependencies += "org.typelevel" %% "cats-laws" % "0.9.0"
libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.9" % "test"
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.8.9" % "test"
libraryDependencies += "org.typelevel" %% "discipline" % "0.7.3" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

scalaOrganization in ThisBuild := "org.typelevel"

scalacOptions ++= Seq(
  "-unchecked",
  "-feature",
  "-deprecation:false",
  "-Xlint",
  "-Xcheckinit",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-inaccessible",
  "-Ywarn-unused-import",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code",
  "-Yno-adapted-args",
//  "-Yinduction-heuristics",
  "-Ypartial-unification",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding", "UTF-8"
)
