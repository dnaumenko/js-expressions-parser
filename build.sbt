name := "js-expressions-parser"

version := "0.1"
scalaVersion := "2.12.6"

// compile
libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4"

// test
libraryDependencies +=  "org.specs2"  %% "specs2-core"  % "4.3.3" % "test"