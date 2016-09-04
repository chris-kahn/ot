name := """ot"""

version := "1.0-SNAPSHOT"

organization := "ws.kahn"

scalaVersion := "2.11.4"

crossScalaVersions := Seq("2.10.4", "2.11.4")

// -- Scala compiler options --------
scalacOptions in ThisBuild ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-language:higherKinds", // enable higher kinded types
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint", // recommended additional warnings
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
  "-Ywarn-inaccessible",
  "-Ywarn-dead-code"
)

lazy val root = (project in file("."))

// Set additional resolvers from where sbt should download dependencies
resolvers ++= Seq(
  "Typesafe" at "http://repo.typesafe.com/typesafe/releases",
  "ShiftFocus" at "https://maven.shiftfocus.ca/repositories/releases"
)

// -- External dependencies --------
libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.3.6",
  "joda-time" % "joda-time" % "2.1",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5",
  "org.clapper" %% "grizzled-slf4j" % "1.0.2"
)



// -- SBT Publish settings --------

publishMavenStyle := true

publishTo := {
  val privateKeyFile = new java.io.File(sys.env("HOME") + "/.ssh/id_rsa")
  Some(Resolver.sftp(
    "Kahn.ws Maven Repository",
    "repo.kahn.ws",
    50022,
    "/var/www/repo.kahn.ws/maven/" + {
      if (isSnapshot.value) "snapshots" else "releases"
    }
  ) as ("maven", privateKeyFile))
}
