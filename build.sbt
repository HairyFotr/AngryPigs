scalaVersion := "2.10.1"

name := "AngryPigs"

organization := "AngryPigs"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")

scalacOptions ++= Seq("-Ywarn-adapted-args", "-Ywarn-all", "-Ywarn-dead-code", "-Ywarn-inaccessible", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-value-discard")

resolvers += "linter" at "http://hairyfotr.github.com/linteRepo/releases"

addCompilerPlugin("com.foursquare.lint" %% "linter" % "0.1-SNAPSHOT")

seq(lwjglSettings: _*)
