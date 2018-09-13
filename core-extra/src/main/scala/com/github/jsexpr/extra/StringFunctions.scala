package com.github.jsexpr.extra

import com.github.jsexpr.FormulaValue._
import com.github.jsexpr.FunctionsAware

object StringFunctions {
  lazy val functions: Map[String, Seq[Value] => Value] = Map(
    "StartsWith" -> startsWith,
    "EndsWith" -> endsWith,
    "Contains" -> contains,
    "Lower" -> lower,
    "Upper" -> upper,
    "ParseInt" -> parseInt,
    "ParseBool" -> parseBool,
    "ParseDouble" -> parseDouble,
  )

  def startsWith(args: Seq[Value]): Value = stringsToVal(args, (first, second) => bool(first.startsWith(second)))

  def endsWith(args: Seq[Value]): Value = stringsToVal(args, (first, second) => bool(first.endsWith(second)))

  def contains(args: Seq[Value]): Value = stringsToVal(args, (first, second) => bool(first.contains(second)))

  def lower(args: Seq[Value]): Value = stringToVal(args, str => FString(str.toLowerCase))

  def upper(args: Seq[Value]): Value = stringToVal(args, str => FString(str.toUpperCase))

  def parseInt(args: Seq[Value]): Value = stringToVal(args, str => FNumber(str.toInt))

  def parseBool(args: Seq[Value]): Value = stringToVal(args, str => bool(str.toBoolean))

  def parseDouble(args: Seq[Value]): Value = stringToVal(args, str => FNumber(str.toDouble))

  private def stringsToVal(args: Seq[Value], f: (String, String) => Value) = {
    if (args.size != 2)
      throw new IllegalArgumentException(s"Wrong number of arguments arguments. Expect 2, got $args")

    (args.head, args(1)) match {
      case (FString(first), FString(second)) => f(first, second)
      case _ => throw new IllegalArgumentException(s"Expect both arguments to be strings, got $args")
    }
  }

  private def stringToVal(args: Seq[Value], f: String => Value) = {
    if (args.size != 1)
      throw new IllegalArgumentException(s"Wrong number of arguments arguments. Expect 1, got $args")

    args.head match {
      case FString(d) => f(d)
      case v => throw new IllegalArgumentException(s"Expect string as argument, got $v")
    }
  }

  private def bool(boolean: Boolean) = if (boolean) TrueValue() else FalseValue()
}


trait StringFunctions extends FunctionsAware {
  override abstract def functions(): Map[String, Seq[Value] => Value] = super.functions() ++ StringFunctions.functions
}
