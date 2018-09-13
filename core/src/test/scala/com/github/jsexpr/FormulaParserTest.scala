package com.github.jsexpr

import com.github.jsexpr.FormulaAST._
import com.github.jsexpr.FormulaValue._
import org.parboiled2.{ErrorFormatter, ParseError}
import org.specs2.mutable.Specification

import scala.util.{Failure, Success}

class FormulaParserTest extends Specification {
  "Formula Parser" should {
    "parse 'null' to null constant" in {
      parse("null") === Constant(NullValue())
    }

    "parse 'true' to true constant" in {
      parse("true") === Constant(TrueValue())
    }

    "parse 'false' to false constant" in {
      parse("false") === Constant(FalseValue())
    }

    "parse '2' to number constant" in {
      parse("2") === Constant(FNumber(BigDecimal(2)))
    }

    "parse '-1' to number constant" in {
      parse("-1") === Constant(FNumber(BigDecimal(-1)))
    }

    "parse '1.2' to number constant" in {
      parse("1.2") === Constant(FNumber(BigDecimal(1.2)))
    }

    "parse '-1E10' to number constant" in {
      parse("-1E10") === Constant(FNumber(BigDecimal(-1E+10)))
    }

    "parse '12.34e-10' to number constant" in {
      parse("12.34e-10") === Constant(FNumber(BigDecimal(12.34e-10)))
    }

    "parse \"'xyz\"' to string constant" in {
      parse("\"xyz\"") === Constant(FString("xyz"))
    }

    "parse escapes in string to string constant" in {
      parse(""""\"\\/\b\f\n\r\t"""") === Constant(FString("\"\\/\b\f\n\r\t"))
      parse("\"L\\" + "u00e4nder\"") === Constant(FString("Länder"))
    }

    "parse arithemetic expressions with addition" in {
      parse("1 + 1") === AdditionOperation(Constant(FNumber(BigDecimal(1))), Constant(FNumber(BigDecimal(1))))
    }

    "parse arithemetic expressions with substraction" in {
      parse("1 - 1") === SubtractionOperation(Constant(FNumber(BigDecimal(1))), Constant(FNumber(BigDecimal(1))))
    }

    "parse arithemetic expressions with multiplication" in {
      parse("1 * 1") === MultiplicationOperation(Constant(FNumber(BigDecimal(1))), Constant(FNumber(BigDecimal(1))))
    }

    "parse arithemetic expressions with substraction" in {
      parse("1 / 1") === DivisionOperation(Constant(FNumber(BigDecimal(1))), Constant(FNumber(BigDecimal(1))))
    }

    "parse arithemetic expressions preserving substraction/multiplication priority" in {
      parse("1 + 2 * 3") === AdditionOperation(
        Constant(FNumber(BigDecimal(1))),
        MultiplicationOperation(Constant(FNumber(BigDecimal(2))), Constant(FNumber(BigDecimal(3))))
      )
    }

    "parse arithemetic expressions preserving parents priority" in {
      parse("(1 + 2) * 3") === MultiplicationOperation(
        AdditionOperation(Constant(FNumber(BigDecimal(1))), Constant(FNumber(BigDecimal(2)))),
        Constant(FNumber(BigDecimal(3)))
      )
    }

    "parse '-(1)' to unary minus operation" in {
      parse("-(1)") === MinusOperation(Constant(FNumber(BigDecimal(1))))
    }

    "parse '!true' to unary invert operation" in {
      parse("!true") === InvertOperation(Constant(TrueValue()))
    }

    "parse dot-separated identifiers to identifier" in {
      parse("user.name") === Identifier("user.name")
    }

    "parse if statement" in {
      parse("if (true) 10") === IfOperation(Constant(TrueValue()), Constant(FNumber(10)))
    }

    "parse if/else statement" in {
      parse("if (true) 10 else 2") === IfElseOperation(Constant(TrueValue()), Constant(FNumber(10)),  Constant(FNumber(2)))
    }

    "parse '?:' operarator" in {
      parse("true ? 10 : 2") === IfElseOperation(Constant(TrueValue()), Constant(FNumber(10)),  Constant(FNumber(2)))
    }
  }

  def parse(s: String): Formula = {
    val parser = FormulaParser(s)
    parser.InputLine.run() match {
      case Success(result) => result
      case Failure(e: ParseError) => sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }
}
