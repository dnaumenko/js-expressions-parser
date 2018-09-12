import sbt.Keys.scalaVersion
import com.github.jsexpr.Dependencies._

name := "js-expressions-parser"

val settings = Seq(
  version := "0.1",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.6", "2.13.0-M4"),
  licenses := Seq("Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
)


lazy val core = project
  .settings(settings)
  .settings(libraryDependencies ++= Seq(
    C.parboiled2, T.specs2
  ))

lazy val coreExtra = Project("core-extra", file("core-extra"))
  .settings(settings)
  .settings(libraryDependencies ++= Seq(T.specs2))
  .dependsOn(core)
                                    
lazy val root = project.in(file("."))
  .aggregate(core, coreExtra)
