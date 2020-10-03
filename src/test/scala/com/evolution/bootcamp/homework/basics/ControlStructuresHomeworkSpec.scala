package com.evolution.bootcamp.homework.basics

import com.evolution.bootcamp.homework.basics.ControlStructuresHomework.Command.{Divide, Min}
import com.evolution.bootcamp.homework.basics.ControlStructuresHomework.ErrorMessage
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ControlStructuresHomeworkSpec extends AnyFlatSpec {

  "parse command" should "return right Command" in {
    ControlStructuresHomework.parseCommand("divide 4 5") shouldEqual Right(Divide(4, 5))
    ControlStructuresHomework.parseCommand("min 4 5 6.4") shouldEqual Right(Min(List(4, 5, 6.4)))
    ControlStructuresHomework.parseCommand("boom 4 5") shouldEqual Left(ErrorMessage("Error: Unknown command 'boom'"))
    ControlStructuresHomework.parseCommand("") shouldEqual Left(ErrorMessage("Error: Input command is empty"))
  }

  "parse input" should "return right result string" in {
    ControlStructuresHomework.process("") shouldEqual "Error: Input command is empty"
    ControlStructuresHomework.process("boom 4 0") shouldEqual "Error: Unknown command 'boom'"
    ControlStructuresHomework.process("divide 4 0") shouldEqual "Error: Division by zero"

    ControlStructuresHomework.process("divide 4 5") shouldEqual "4 divided by 5 is 0.8"
    ControlStructuresHomework.process("sum 5 5 6 8.5") shouldEqual "the sum of 5 5 6 8.5 is 24.5"
    ControlStructuresHomework.process("average 4 3 8.5 4") shouldEqual "the average of 4 3 8.5 4 is 4.875"
    ControlStructuresHomework.process("min 4 -3 -17") shouldEqual "the minimum of 4 -3 -17 is -17"
    ControlStructuresHomework.process("max 4 -3 -17") shouldEqual "the maximum of 4 -3 -17 is 4"
  }

}
