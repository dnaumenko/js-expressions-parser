package com.github.jsexpr.extra

import com.github.jsexpr.{FormulaEval, FormulaParser}
import com.github.jsexpr.FormulaValue._
import org.parboiled2.{ErrorFormatter, ParseError}
import org.specs2.mutable.Specification

import scala.util.{Failure, Success}

class StringFunctionsTest extends Specification {
  "Formula Eval With String Functions" should {
    "support StartsWith() funciton" in {
      eval("StartsWith(\"abcde\", \"ab\")") === TrueValue()
    }

    "support EndsWith() funciton" in {
      eval("EndsWith(\"abcde\", \"de\")") === TrueValue()
    }

    "support Contains() funciton" in {
      eval("Contains(\"abcde\", \"cd\")") === TrueValue()
    }

    "support Lower() funciton" in {
      eval("Lower(\"ABC\")") === FString("abc")
    }

    "support Upper() funciton" in {
      eval("Upper(\"abc\")") === FString("ABC")
    }
  }

  def eval(s: String, env: Map[String, Value] = Map.empty): Value = {
    val eval = new FormulaEval with StringFunctions
    val parser = FormulaParser(s)
    parser.Line.run() match {
      case Success(result) => eval.eval(result)
      case Failure(e: ParseError) => sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }
}