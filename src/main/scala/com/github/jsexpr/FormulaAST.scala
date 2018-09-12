package com.github.jsexpr

import com.github.jsexpr.FormulaValue.Value


object FormulaAST {
  sealed trait Formula

  case class Identifier(name: String) extends Formula

  case class Constant(value: Value) extends Formula

  abstract class UnaryOperation(op: Identifier, argument: Formula) extends Formula

  case class MinusOperation(argument: Formula) extends UnaryOperation(Identifier("-"), argument)

  case class InvertOperation(argument: Formula) extends UnaryOperation(Identifier("!"), argument)

  abstract class BinaryOperation(val op: Identifier, val lhs: Formula, val rhs: Formula) extends Formula

  case class AdditionOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("+"), lhs, rhs)

  case class SubtractionOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("-"), lhs, rhs)

  case class MultiplicationOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("*"), lhs, rhs)

  case class DivisionOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("/"), lhs, rhs)

  case class AndOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("&&"), lhs, rhs)

  case class OrOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("&&"), lhs, rhs)

  case class GreaterThanOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier(">"), lhs, rhs)

  case class GreaterOrEqualThanOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier(">="), lhs, rhs)

  case class LessThanOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("<"), lhs, rhs)

  case class LessOrEqualThanOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("<="), lhs, rhs)
}

object FormulaValue {
  sealed trait Value

  case class TrueValue() extends Value
  case class FalseValue() extends Value
  case class NullValue() extends Value
  case class FNumber(number: BigDecimal) extends Value
  object FNumber {
    def apply(n: Int) = new FNumber(BigDecimal(n))
    def apply(n: Double) = new FNumber(BigDecimal(n))
    def apply(n: String) = new FNumber(BigDecimal(n))
  }

  case class FString(string: String) extends Value
}
