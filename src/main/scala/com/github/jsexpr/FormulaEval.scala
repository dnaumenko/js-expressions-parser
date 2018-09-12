package com.github.jsexpr

import com.github.jsexpr.FormulaAST.UnaryOperation
import com.github.jsexpr.FormulaAST._
import com.github.jsexpr.FormulaValue.{FNumber, FalseValue, TrueValue, Value}

case class FormulaEval(env: Map[String, Value] = Map.empty) {
  def eval(formula: Formula): Value = {
    formula match {
      case Constant(value) => value
      case Identifier(n) => env.getOrElse(n, throw new IllegalArgumentException(s"Unknown value for $n"))

      case e: UnaryOperation =>
        val value = eval(e.argument)

        e match {
          case MinusOperation(_) => value match {
            case FNumber(n) => FNumber(n.unary_-)
            case v => throw new IllegalArgumentException(s"Can't apply unary minus to not-number: $v")
          }
          case InvertOperation(_) => value match {
            case TrueValue() => FalseValue()
            case FalseValue() => TrueValue()
            case v => throw new IllegalArgumentException(s"Can't apply unary invert op to not-boolean: $v")
          }
        }

      case e: BinaryOperation =>
        val lhs = eval(e.lhs)
        val rhs = eval(e.rhs)

        e match {
          case _: AdditionOperation => numOp(e.op, lhs, rhs, _ + _)
          case _: SubtractionOperation => numOp(e.op, lhs, rhs, _ - _)
          case _: MultiplicationOperation => numOp(e.op, lhs, rhs, _ * _)
          case _: DivisionOperation => numOp(e.op, lhs, rhs, _ / _)
        }
    }
  }

  private def numOp(op: Identifier, lhs: Value, rhs: Value, f: (BigDecimal, BigDecimal) => BigDecimal) = (lhs, rhs) match {
      case (FNumber(a1), FNumber(a2)) => FNumber(f(a1, a2))
      case (n1, n2) => throw new IllegalArgumentException(s"Can't apply ${op.name} operation for non-numbers: $n1, $n2")
    }
}
