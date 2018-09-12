package com.github.jsexpr

import sbt._

object Dependencies {
  object C {
    val parboiled2 = "org.parboiled" %% "parboiled" % "2.1.4"
  }

  object T {
    val specs2 = "org.specs2"  %% "specs2-core"  % "4.3.3" % "test"
  }
}