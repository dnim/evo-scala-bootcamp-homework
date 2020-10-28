package com.evolution.bootcamp.homework.error_handling

import java.time.{Month, Year}

import cats.data.Validated.{Invalid, Valid}
import com.evolution.bootcamp.homework.error_handling.Homework.CardValidationTypes._
import com.evolution.bootcamp.homework.error_handling.Homework.PaymentCardValidator.{AllErrorsOr, validate}
import com.evolution.bootcamp.homework.error_handling.Homework.{PaymentCard, PaymentCardValidator}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class HomeworkSpec extends AnyFlatSpec {

  "PaymentCard" should "is valid" in {
    validate("BOB KOOK", "1234123412341234", "12/22", "2323") shouldEqual
        Valid(PaymentCard(NameOnCard("BOB KOOK"), CardNumber("1234123412341234"),
          ExpirationDate(Month.DECEMBER, Year.of(22)), SecurityCode("2323")))
  }

  it should "be invalid" in {
    validate("BOBKOOK", "1234124", "99/22", "12345").isInvalid shouldBe true
    validate("BOB KOOK", "1234123412341234", "99/22", "1234").isInvalid shouldBe true
    validate("BOB KOOK", "1234123412341234", "12/22", "1234k").isInvalid shouldBe true
  }

}
