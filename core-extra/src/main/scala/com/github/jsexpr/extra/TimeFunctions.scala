package com.github.jsexpr.extra

import java.time.ZonedDateTime

import com.github.jsexpr.FormulaValue.{FDateTime, FNumber, FString, Value}
import com.github.jsexpr.FunctionsAware

object TimeFunctions {
  lazy val functions: Map[String, Seq[Value] => Value] = Map(
    "Now" -> now,
    "Day" -> day,
    "DayOfWeek" -> dayOfWeek,
    "Month" -> month,
    "Year" -> year,
    "ParseTime" -> parseTime
  )

  def now(args: Seq[Value]): Value = {
    if (args.nonEmpty)
      throw new IllegalArgumentException(s"Now doesn't accept any arguments, got $args")

    FDateTime(ZonedDateTime.now())
  }

  def parseTime(args: Seq[Value]): Value = {
    if (args.size != 1)
      throw new IllegalArgumentException(s"Wrong number of arguments arguments. Expect 1, got $args")

    args.head match {
      case FString(d) => FDateTime(ZonedDateTime.parse(d))
      case v => throw new IllegalArgumentException(s"Expect string as argument, got $v")
    }
  }

  def day(args: Seq[Value]): Value = dateTimeFunction(args, d => FNumber(d.getDayOfMonth))

  def dayOfWeek(args: Seq[Value]): Value = dateTimeFunction(args, d => FNumber(d.getDayOfWeek.getValue))

  def month(args: Seq[Value]): Value = dateTimeFunction(args, d => FNumber(d.getMonth.getValue))

  def year(args: Seq[Value]): Value = dateTimeFunction(args, d => FNumber(d.getYear))

  def dateTimeFunction(args: Seq[Value], f: ZonedDateTime => Value): Value = {
    if (args.size != 1)
      throw new IllegalArgumentException(s"Wrong number of arguments arguments. Expect 1, got $args")

    args.head match {
      case FDateTime(d) => f(d)
      case v => throw new IllegalArgumentException(s"Expect datetime as argument, got $v")
    }
  }
}

trait TimeFunctions extends FunctionsAware {
  override abstract def functions(): Map[String, Seq[Value] => Value] = super.functions() ++ TimeFunctions.functions
}
