name := "notification-server"

version := "1.2.0"

organization := "me.ycy"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

seq(sbtassembly.Plugin.assemblySettings: _*)

resolvers += "Typesafe Repository" at
  "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "me.ycy" %% "notification-api" % "1.0",
  "me.ycy" %% "desktop-util" % "1.0.1",
  "org.mashupbots.socko" %% "socko-webserver" % "0.2.4",
  "com.typesafe.akka" %% "akka-actor"  % "2.1.2",
  "net.databinder" %% "dispatch-json" % "0.8.9",
  "commons-codec" % "commons-codec" % "1.5"
)
