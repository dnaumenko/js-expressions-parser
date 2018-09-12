package com.github.jsexpr

import com.github.jsexpr.FormulaValue.Value


object FormulaAST {
  sealed trait Formula

  case class Identifier(name: String) extends Formula

  case class Constant(value: Value) extends Formula

  abstract class UnaryOperation(op: Identifier, argument: Formula) extends Formula

  case class MinusOperation(argument: Formula) extends UnaryOperation(Identifier("-"), argument)

  case class InvertOperation(argument: Formula) extends UnaryOperation(Identifier("!"), argument)

  abstract class BinaryOperation(op: Identifier, lhs: Formula, rhs: Formula) extends Formula

  case class AdditionOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier("+"), lhs, rhs)

  case class SubtractionOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier("-"), lhs, rhs)

  case class MultiplicationOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier("*"), lhs, rhs)

  case class DivisionOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier("/"), lhs, rhs)

  case class AndOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier("&&"), lhs, rhs)

  case class OrOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier("&&"), lhs, rhs)

  case class GreaterThanOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier(">"), lhs, rhs)
  case class GreaterOrEqualThanOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier(">="), lhs, rhs)
  case class LessThanOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier("<"), lhs, rhs)
  case class LessOrEqualThanOperation(lhs: Formula, rhs: Formula) extends BinaryOperation(Identifier("<="), lhs, rhs)
}

object FormulaValue {
  abstract class Value

  case class TrueValue() extends Value

  case class FalseValue() extends Value

  case class NullValue() extends Value

  case class FNumber(number: BigDecimal) extends Value
  object FNumber {
    def apply(n: String) = new FNumber(BigDecimal(n))
  }

  case class FString(string: String) extends Value
}
