name := "Scalastat"
version := "0.0.2"
scalaVersion := "2.11.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
logBuffered in Test := false
