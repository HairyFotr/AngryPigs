scalaVersion := "2.10.1"

name := "AngryPigs"

organization := "AngryPigs"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")

scalacOptions ++= Seq("-Ywarn-adapted-args", "-Ywarn-all", "-Ywarn-dead-code", "-Ywarn-inaccessible", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-value-discard")

scalacOptions ++= Seq("-Xplugin:/home/hairy/dev/linter/target/scala-2.10/linter_2.10-0.1-SNAPSHOT.jar")

seq(lwjglSettings: _*)
