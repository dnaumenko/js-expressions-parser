package com.github.jsexpr

import com.github.jsexpr.FormulaAST.{Formula, _}
import com.github.jsexpr.FormulaValue._
import org.parboiled2.CharPredicate._
import org.parboiled2._

case class FormulaParser(input: ParserInput) extends Parser
 with StringBuilding with WhiteSpace with Numbers with EscapedStrings with UnescapedString {

  def Line: Rule1[Formula] = rule {
    FormulaRule ~ EOI
  }

  def FormulaRule: Rule1[Formula] = rule {
    Term ~ zeroOrMore(
      ws('>') ~ Term ~> GreaterThanOperation
      | ws('<') ~ Term ~> LessThanOperation
      | ws(">=") ~ Term ~> GreaterOrEqualThanOperation
      | ws("<=") ~ Term ~> LessOrEqualThanOperation
      | ws("==") ~ Term ~> EqualOperation
      | ws("!=") ~ Term ~> NotEqualOperation
    )
  }

  def Term: Rule1[Formula] = rule {
    FactorTerm ~ zeroOrMore(
      ws('+') ~ FactorTerm ~> AdditionOperation
      | ws('-') ~ FactorTerm ~> SubtractionOperation
    )
  }

  def FactorTerm: Rule1[Formula] = rule {
    StatementExpression ~ zeroOrMore(
      ws('*') ~ StatementExpression ~> MultiplicationOperation
      | ws('/') ~ StatementExpression ~> DivisionOperation)
  }

  def StatementExpression: Rule1[Formula] = rule {
    ConditionalExpression | SingleExpression
  }

  def SingleExpression: Rule1[Formula] = rule {
    WhiteSpace ~ (ConstExpression | UnaryExpression | FunctionExpression | IdentifierExpression | Parens) ~ WhiteSpace
  }

  def ConstExpression: Rule1[Formula] = rule {
    Literal ~> Constant
  }

  def IdentifierExpression: Rule1[Identifier] = rule {
    capture(oneOrMore(CharPredicate.AlphaNum + CharPredicate('.'))) ~> Identifier
  }

  def FunctionExpression: Rule1[Formula] = rule {
    (IdentifierExpression ~ ws('(') ~ ArgumentsRule ~ ws(')')) ~> ((name: Identifier, arguments: Seq[Formula]) =>
      FunctionOperation(name, arguments)) ~ WhiteSpace
  }

  def ArgumentsRule: Rule1[Seq[Formula]] = rule {
    zeroOrMore(FormulaRule).separatedBy(ws(','))
  }

  def ConditionalExpression: Rule1[Formula] = rule {
    run {
      ( SingleExpression ~ ws('?') ~ FormulaRule ~ ws(':') ~ FormulaRule ~> IfElseOperation
      | ws("if") ~ ws('(') ~ FormulaRule ~ ws(')') ~ FormulaRule ~ ws("else") ~ FormulaRule ~> IfElseOperation
      | ws("if") ~ ws('(') ~ FormulaRule ~ ws(')') ~ FormulaRule ~> IfOperation)
    }
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

  def WhiteSpace: Rule0 = rule { zeroOrMore(WhiteSpaceChar) }

  def ws(c: Char): Rule0 = rule { c ~ WhiteSpace }

  def ws(s: String): Rule0 = rule { s ~ WhiteSpace }
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