name := "Scalastat"
version := "0.0.2"
scalaVersion := "2.12.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
logBuffered in Test := false
