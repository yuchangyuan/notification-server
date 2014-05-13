name := "notification-server"

version := "1.1.3"

organization := "me.ycy"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

seq(sbtassembly.Plugin.assemblySettings: _*)

resolvers += "Typesafe Repository" at
  "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "me.ycy" %% "notification-api" % "1.0",
  "org.mashupbots.socko" %% "socko-webserver" % "0.2.4",
  "com.typesafe.akka" %% "akka-actor"  % "2.1.2",
  "net.databinder" %% "dispatch-json" % "0.8.9"
)
