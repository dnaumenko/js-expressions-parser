package com.github.jsexpr

import com.github.jsexpr.FormulaValue._
import org.parboiled2.{ErrorFormatter, ParseError}
import org.specs2.mutable.Specification

import scala.util.{Failure, Success}

class FormulaEvalTest extends Specification {
  "Formula Eval" should {
    "evaluate boolean to boolean" in {
      eval("true") === TrueValue()
      eval("false") === FalseValue()
    }

    "evaluate number to number" in {
      eval("1") === FNumber(1)
    }

    "evaluate string to string" in {
      eval("\"xyz\"") === FString("xyz")
    }

    "evaluate simple arithmetic exp to its result value" in {
      eval("-(2 + 3)") === FNumber(-5)
      eval("1 + 1") === FNumber(2)
      eval("2 - 2") === FNumber(0)
      eval("2 * 2") === FNumber(4)
      eval("4 / 2") === FNumber(2)
    }

    "evaluate complex arithmetic to its result value" in {
      eval("1 + 2 * 3") === FNumber(7)
      eval("(1 + 2) * 3") === FNumber(9)
      eval("(1 + 2 * 5) * 3") === FNumber(33)
    }

    "evaluate boolean negotiations" in {
      eval("!true") === FalseValue()
      eval("!false") === TrueValue()
    }

    "evaluate expressions with given identifiers" in {
      val env = Map("user.age" -> FNumber(42))
      eval("user.age + 10", env) === FNumber(52)
    }

    "throw exception if tries to eval unknown identifier" in {
      eval("user.age + 10") must throwAn[IllegalArgumentException]
    }
  }

  def eval(s: String, env: Map[String, Value] = Map.empty): Value = {
    val parser = FormulaParser(s)
    val eval = FormulaEval(env)
    parser.InputLine.run() match {
      case Success(result) => eval.eval(result)
      case Failure(e: ParseError) => sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }
}
