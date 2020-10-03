package com.evolution.bootcamp.homework.basics

import scala.io.Source

object ControlStructuresHomework {
  import com.evolution.bootcamp.homework.basics.ControlStructuresHomework.Command._

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  import java.text.DecimalFormat

  val format = new DecimalFormat("0.###")

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result {
    def renderResult(): String
  }

  final case class RenderResult(command: Command, result: Double) extends Result {
    def renderResult(): String = {
      command match {
        case Divide(dividend, divisor) => s"${format.format(dividend)} divided by ${format.format(divisor)} is ${format.format(result)}"
        case Sum(numbers) => s"the sum of ${numbers.map(format.format).mkString(" ")} is ${format.format(result)}"
        case Average(numbers) => s"the average of ${numbers.map(format.format).mkString(" ")} is ${format.format(result)}"
        case Min(numbers) => s"the minimum of ${numbers.map(format.format).mkString(" ")} is ${format.format(result)}"
        case Max(numbers) => s"the maximum of ${numbers.map(format.format).mkString(" ")} is ${format.format(result)}"
      }
    }
  } // adjust Result as required to match requirements

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    if (x.length == 0) Left(ErrorMessage("Error: Input command is empty"))
    else {
      val parsedCommand = x.split("\\s+").toList
      val command: Either[ErrorMessage, Command] = parsedCommand match {
        case x :: xs => x match {
          case "divide"  => Right(Divide(xs.head.toDouble, xs(1).toDouble))
          case "sum"     => Right(Sum(xs.map(_.toDouble)))
          case "average" => Right(Average(xs.map(_.toDouble)))
          case "min"     => Right(Min(xs.map(_.toDouble)))
          case "max"     => Right(Max(xs.map(_.toDouble)))
          case _         => Left(ErrorMessage(s"Error: Unknown command '$x'"))
        }
      }
      command
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(dividend, divisor) => if (divisor == 0) Left(ErrorMessage("Error: Division by zero")) else
        Right(RenderResult(x, dividend/divisor))
      case Sum(numbers)              => Right(RenderResult(x, numbers.sum))
      case Average(numbers)          => Right(RenderResult(x, numbers.sum / numbers.size))
      case Min(numbers)              => Right(RenderResult(x, numbers.min))
      case Max(numbers)              => Right(RenderResult(x, numbers.max))
      case _                         => Left(ErrorMessage("Error: Unknown operation"))
    }
  }

  def process(x: String): String = {
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    val result = for {
      command <- parseCommand(x)
      calculation <- calculate(command)
    } yield calculation

    result match {
      case Right(result) => result.renderResult()
      case Left(error) => error.value
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
