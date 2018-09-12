package com.github.jsexpr

import com.github.jsexpr.FormulaAST._
import com.github.jsexpr.FormulaValue._
import org.parboiled2.CharPredicate.{Digit, Digit19, HexDigit}
import org.parboiled2._

case class FormulaParser(input: ParserInput) extends Parser
 with StringBuilding with WhiteSpace with Numbers with EscapedStrings with UnescapedString {

  def InputLine: Rule1[Formula] = rule {
    FormulaRule ~ EOI
  }

  def FormulaRule: Rule1[Formula] = rule {
    Term ~ zeroOrMore(
      ws('+') ~ Term ~> AdditionOperation
      | ws('-') ~ Term ~> SubtractionOperation)
  }

  def Term: Rule1[Formula] = rule {
    SingleExpression ~ zeroOrMore(
      ws('*') ~ SingleExpression ~> MultiplicationOperation
      | ws('/') ~ SingleExpression ~> DivisionOperation)
  }

  def SingleExpression: Rule1[Formula] = rule { WhiteSpace ~ (ConstExpression | UnaryExpression | Parens) ~ WhiteSpace }

  def ConstExpression: Rule1[Formula] = rule {
    Literal ~> Constant
  }

  def Parens: Rule1[Formula] = rule {
    '(' ~ FormulaRule ~ ')'
  }

  def UnaryExpression: Rule1[Formula] = rule {
    run {
      (ws('!') ~ SingleExpression ~> InvertOperation
      | ws('-') ~ SingleExpression ~> MinusOperation)
    }
  }

  def Literal: Rule1[Value] = rule { WhiteSpace ~ Value }

  def Value: Rule1[Value] = rule {
    run {
      (String | StringNoEscapes | Number | True | False | Null) ~ WhiteSpace
    }
  }

  def String: Rule1[Value] = rule { StringUnwrapped ~> FString }

  def StringNoEscapes: Rule1[Value] = rule { StringUnwrappedNoEscapes ~> FString }

  def Number: Rule1[Value] = rule { capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> (FNumber(_)) ~ WhiteSpace }

  def True: Rule1[Value] = rule { "true" ~ WhiteSpace ~ push(TrueValue()) }

  def False: Rule1[Value] = rule { "false" ~ WhiteSpace ~ push(FalseValue()) }

  def Null: Rule1[Value] = rule { "null" ~ WhiteSpace ~ push(NullValue()) }
}

trait WhiteSpace { this: Parser =>
  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  def ws(c: Char) = rule { c ~ WhiteSpace }

  def ws(s: String) = rule { s ~ WhiteSpace }
}

trait Numbers { this: Parser =>
  def Integer = rule { optional('-') ~ (Digit19 ~ Digits | Digit) }

  def Digits = rule { oneOrMore(Digit) }

  def Frac = rule { "." ~ Digits }

  def Exp = rule { ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits }
}

trait UnescapedString extends StringBuilding with WhiteSpace { this: Parser =>
  def StringUnwrappedNoEscapes = rule { '\'' ~ clearSB() ~ zeroOrMore(CharExceptApostrophe) ~ ws('\'') ~ push(sb.toString) }

  def CharExceptApostrophe = rule { !'\'' ~ ANY ~ appendSB() }
}

trait EscapedStrings extends StringBuilding with WhiteSpace { this: Parser =>
  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"

  def StringUnwrapped = rule { '"' ~ clearSB() ~ Characters ~ ws('"') ~ push(sb.toString) }

  def Characters = rule { zeroOrMore(NormalChar | '\\' ~ EscapedChar) }

  def NormalChar = rule { !QuoteBackslash ~ ANY ~ appendSB() }

  def EscapedChar = rule (
    QuoteSlashBackSlash ~ appendSB()
      | 'b' ~ appendSB('\b')
      | 'f' ~ appendSB('\f')
      | 'n' ~ appendSB('\n')
      | 'r' ~ appendSB('\r')
      | 't' ~ appendSB('\t')
      | Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
  )

  def Unicode = rule { 'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16)) }
}