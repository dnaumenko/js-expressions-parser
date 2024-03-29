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
      parse("2") === Constant(FNumber(2))
    }

    "parse '-1' to number constant" in {
      parse("-1") === Constant(FNumber(-1))
    }

    "parse '1.2' to number constant" in {
      parse("1.2") === Constant(FNumber(1.2))
    }

    "parse '-1E10' to number constant" in {
      parse("-1E10") === Constant(FNumber(-1E+10))
    }

    "parse '12.34e-10' to number constant" in {
      parse("12.34e-10") === Constant(FNumber(12.34e-10))
    }

    "parse \"'xyz\"' to string constant" in {
      parse("\"xyz\"") === Constant(FString("xyz"))
    }

    "parse escapes in string to string constant" in {
      parse(""""\"\\/\b\f\n\r\t"""") === Constant(FString("\"\\/\b\f\n\r\t"))
      parse("\"L\\" + "u00e4nder\"") === Constant(FString("Länder"))
    }

    "parse arithemetic expressions with addition" in {
      parse("1 + 1") === AdditionOperation(Constant(FNumber(1)), Constant(FNumber(1)))
    }

    "parse arithemetic expressions with substraction" in {
      parse("1 - 1") === SubtractionOperation(Constant(FNumber(1)), Constant(FNumber(1)))
    }

    "parse arithemetic expressions with multiplication" in {
      parse("1 * 1") === MultiplicationOperation(Constant(FNumber(1)), Constant(FNumber(1)))
    }

    "parse arithemetic expressions with substraction" in {
      parse("1 / 1") === DivisionOperation(Constant(FNumber(1)), Constant(FNumber(1)))
    }

    "parse arithemetic expressions preserving substraction/multiplication priority" in {
      parse("1 + 2 * 3") === AdditionOperation(
        Constant(FNumber(BigDecimal(1))),
        MultiplicationOperation(Constant(FNumber(2)), Constant(FNumber(3)))
      )
    }

    "parse arithemetic expressions preserving parents priority" in {
      parse("(1 + 2) * 3") === MultiplicationOperation(
        AdditionOperation(Constant(FNumber(1)), Constant(FNumber(2))),
        Constant(FNumber(BigDecimal(3)))
      )
    }

    "parse >,>=,<,<=,==,!= operator expressions" in {
      parse("2 > 3") === GreaterThanOperation(Constant(FNumber(2)), Constant(FNumber(3)))
      parse("2 < 3") === LessThanOperation(Constant(FNumber(2)), Constant(FNumber(3)))
      parse("2 >= 3") === GreaterOrEqualThanOperation(Constant(FNumber(2)), Constant(FNumber(3)))
      parse("2 <= 3") === LessOrEqualThanOperation(Constant(FNumber(2)), Constant(FNumber(3)))
      parse("2 == 3") === EqualOperation(Constant(FNumber(2)), Constant(FNumber(3)))
      parse("2 != 3") === NotEqualOperation(Constant(FNumber(2)), Constant(FNumber(3)))
    }

    "parse >,>=,<,<= operator expressions preserving priority of multiplication/addition operations" in {
      parse("4 > 3 + 2") === GreaterThanOperation(
        Constant(FNumber(4)),
        AdditionOperation(Constant(FNumber(3)), Constant(FNumber(2)))
      )
      parse("4 >= 2 * 2 + 1") === GreaterOrEqualThanOperation(
        Constant(FNumber(4)),
        AdditionOperation(
          MultiplicationOperation(Constant(FNumber(2)), Constant(FNumber(2))),
          Constant(FNumber(1))
        )
      )
      parse("2 * 2 + 1 <= 4") === LessOrEqualThanOperation(
        AdditionOperation(
          MultiplicationOperation(Constant(FNumber(2)), Constant(FNumber(2))),
          Constant(FNumber(1))
        ),
        Constant(FNumber(4))
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

    "parse &&,|| statements" in {
      parse("true && false") == AndOperation(Constant(TrueValue()), Constant(FalseValue()))
      parse("true || false") == OrOperation(Constant(TrueValue()), Constant(FalseValue()))
    }

    "parse &&,|| statements with correct priority" in {
      parse("2 > 3 && true") == AndOperation(
        GreaterThanOperation(Constant(FNumber(2)), Constant(FNumber(3))),
        Constant(TrueValue())
      )

      parse("true || 2 > 3") == OrOperation(
        Constant(TrueValue()),
        GreaterThanOperation(Constant(FNumber(2)), Constant(FNumber(3)))
      )
    }
  }

  def parse(s: String): Formula = {
    val parser = FormulaParser(s)
    parser.Line.run() match {
      case Success(result) => result
      case Failure(e: ParseError) => sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }
}
