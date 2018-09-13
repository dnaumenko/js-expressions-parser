package com.github.jsexpr.extra

import java.time.ZonedDateTime

import com.github.jsexpr.FormulaValue.{FDateTime, FNumber, Value}
import com.github.jsexpr.{FormulaEval, FormulaParser}
import org.parboiled2.{ErrorFormatter, ParseError}
import org.specs2.mutable.Specification

import scala.util.{Failure, Success}

class TimeFunctionsTest extends Specification {
  "Formula Eval With Time Functions" should {
    "return datetime for Now() function" in {
      eval("Now()") must haveClass[FDateTime]
    }

    "parse valid string to datetime for ParseTime() funciton" in {
      eval("ParseTime(\"2017-01-01T10:00:23Z\")") === FDateTime(ZonedDateTime.parse("2017-01-01T10:00:23Z"))
    }

    "return day of month number for Day() function" in {
      eval("DayOfWeek(ParseTime(\"2017-01-02T10:00:23Z\"))") === FNumber(1)
    }

    "return day of week number for DayOfWeek() function" in {
      eval("Day(ParseTime(\"2017-01-02T10:00:23Z\"))") === FNumber(2)
    }

    "return month number for Month() function" in {
      eval("Month(ParseTime(\"2017-01-02T10:00:23Z\"))") === FNumber(1)
    }

    "return year number for Year() function" in {
      eval("Year(ParseTime(\"2017-01-02T10:00:23Z\"))") === FNumber(2017)
    }
  }

  def eval(s: String, env: Map[String, Value] = Map.empty): Value = {
    val eval = new FormulaEval with TimeFunctions
    val parser = FormulaParser(s)
    parser.Line.run() match {
      case Success(result) => eval.eval(result)
      case Failure(e: ParseError) => sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }
}
