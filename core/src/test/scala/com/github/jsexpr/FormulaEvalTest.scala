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

    "evaluate if statement with true/false predicate" in {
      eval("if (true) 10") === FNumber(10)
      eval("if (false) 10") === VoidValue()
    }

    "throw exception if not-boolean predicate given for if statement" in {
      eval("if (10) 10") must throwAn[IllegalArgumentException]
    }

    "evaluate if/else statement" in {
      eval("if (true) 10 else 20") === FNumber(10)
      eval("if (false) 10 else 20") === FNumber(20)
      eval("if (10) 20 else 30") must throwAn[IllegalArgumentException]
    }

    "evaluate ?: operator" in {
      eval("true ? 10 : 20") === FNumber(10)
      eval("false ? 10 : 20") === FNumber(20)
      eval("10 ? 20 : 30") must throwAn[IllegalArgumentException]
    }

    "eval >,>=,<,<=,==,!= operator expressions" in {
      eval("2 > 3") === FalseValue()
      eval("2 < 3") === TrueValue()
      eval("2 >= 3") === FalseValue()
      eval("2 <= 3") === TrueValue()
      eval("2 == 3") === FalseValue()
      eval("2 != 3") === TrueValue()
    }

    "eval >,>=,<,<= operator expressions preserving priority of multiplication/addition operations" in {
      eval("4 > 3 + 2") === FalseValue()
      eval("4 >= 2 * 2 + 1") === FalseValue()
      eval("2 * 2 + 1 <= 4") === FalseValue()
    }

    "eval &&,|| statements" in {
      eval("true && false") == FalseValue()
      eval("true || false") == TrueValue()
    }

    "parse &&,|| statements with correct priority" in {
      eval("2 > 3 && true") == FalseValue()
      eval("true || 2 > 3") == TrueValue()
    }
  }

  def eval(s: String, env: Map[String, Value] = Map.empty): Value = {
    val parser = FormulaParser(s)
    val eval = FormulaEval(env)
    parser.Line.run() match {
      case Success(result) => eval.eval(result)
      case Failure(e: ParseError) => sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }
}
