name := "parser"

version := "1.0"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6" % "test")

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")
