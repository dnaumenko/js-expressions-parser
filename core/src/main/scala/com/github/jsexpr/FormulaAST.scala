package com.github.jsexpr

import java.time.ZonedDateTime

import com.github.jsexpr.FormulaValue.Value


object FormulaAST {
  sealed trait Formula

  case class Identifier(name: String) extends Formula

  case class Constant(value: Value) extends Formula

  abstract class UnaryOperation(op: Identifier, val argument: Formula) extends Formula

  case class MinusOperation(override val argument: Formula) extends UnaryOperation(Identifier("-"), argument)

  case class InvertOperation(override val argument: Formula) extends UnaryOperation(Identifier("!"), argument)

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
  
  case class EqualOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("=="), lhs, rhs)

  case class NotEqualOperation(override val lhs: Formula, override val rhs: Formula) extends BinaryOperation(Identifier("!="), lhs, rhs)

  case class FunctionOperation(op: Identifier, arguments: Seq[Formula]) extends Formula

  case class IfOperation(predicate: Formula, expressionOnTrue: Formula) extends Formula

  case class IfElseOperation(predicate: Formula, expressionOnTrue: Formula, expressionOnFalse: Formula) extends Formula
}

object FormulaValue {
  sealed trait Value
  case class VoidValue() extends Value
  case class TrueValue() extends Value
  case class FalseValue() extends Value
  case class NullValue() extends Value
  case class FNumber(number: BigDecimal) extends Value
  object FNumber {
    def apply(n: Int) = new FNumber(BigDecimal(n))
    def apply(n: Long) = new FNumber(BigDecimal(n))
    def apply(n: Double) = new FNumber(BigDecimal(n))
    def apply(n: String) = new FNumber(BigDecimal(n))
  }
  case class FString(string: String) extends Value
  case class FDateTime(date: ZonedDateTime) extends Value
}
