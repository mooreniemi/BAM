name := """quickselect"""

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies ++= Seq(
  "de.h2b.java.lib.pa-toolbox" % "pa-toolbox-extern-javaplot" % "1.1.0",
  "com.storm-enroute" %% "scalameter-core" % "0.7",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)

fork in run := true
