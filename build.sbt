name := "notification-server"

version := "1.1.0-SNAPSHOT"

organization := "me.ycy"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

seq(sbtassembly.Plugin.assemblySettings: _*)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.mashupbots.socko" % "socko-webserver_2.9.2" % "0.2.3",
  "com.typesafe.akka" % "akka-actor"  % "2.0.3",
  "net.databinder" % "dispatch-json_2.9.1" % "0.8.8"
)
