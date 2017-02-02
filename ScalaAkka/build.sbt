name := """AccountContext"""

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.16"
libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % "2.4.16"
libraryDependencies += "com.github.dnvriend" %% "akka-persistence-inmemory" % "1.3.18"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += Resolver.jcenterRepo
