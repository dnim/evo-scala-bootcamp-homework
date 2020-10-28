package com.evolution.bootcamp.homework.error_handling

import java.time.{Month, Year}

import cats.data.ValidatedNec
import com.evolution.bootcamp.homework.error_handling.Homework.CardValidationTypes.{CardNumber, ExpirationDate, NameOnCard, SecurityCode}

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework {

  object CardValidationTypes {
    final case class NameOnCard(name: String) extends AnyVal
    final case class CardNumber(cardNumber: String) extends AnyVal
    final case class ExpirationDate(month: Month, year: Year)
    final case class SecurityCode(code: String) extends AnyVal
  }

  case class PaymentCard(name: NameOnCard, number: CardNumber, expirationDate: ExpirationDate, securityCode: SecurityCode)

  sealed trait ValidationError
  object ValidationError {

    final case object NameHasWrongFormat extends ValidationError {
      override def toString: String = "Name on card should contain only characters from Latin alphabet (A-Z) in upper case"
    }
    final case object NameIsEmpty extends ValidationError {
      override def toString: String = "Please, provide name on card"
    }

    final case object CardNumberContainsWrongCharacters extends ValidationError {
      override def toString: String = "Card number can contains only digits (0-9)"
    }
    final case object CardNumberHasWrongLength extends ValidationError {
      override def toString: String = "Card number should be 16 digits length"
    }

    final case object ExpirationDateWrongFormat extends ValidationError {
      override def toString: String = "Wrong Expiration Date format"
    }

    final case object SecurityCodeWrongFormat extends ValidationError {
      override def toString: String = "Wrong Security Code format. Should be 4 digits"
    }
  }

  object PaymentCardValidator {

    import ValidationError._
    import cats.implicits._
    import CardValidationTypes._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validateName(name: String): AllErrorsOr[NameOnCard] = {
      def validateOnEmpty: AllErrorsOr[NameOnCard] =
        if (name.isBlank) NameIsEmpty.invalidNec
        else NameOnCard(name).validNec

      def validateCardCharacters: AllErrorsOr[NameOnCard] =
        if (name.matches("^[A-Z]+ [A-Z]+$")) NameOnCard(name).validNec
        else NameHasWrongFormat.invalidNec

      validateOnEmpty *> validateCardCharacters
    }

    def validateCardNumber(number: String): AllErrorsOr[CardNumber] = {
      def validateLength: AllErrorsOr[CardNumber] =
        if (number.length == 16) CardNumber(number).validNec
        else CardNumberHasWrongLength.invalidNec

      def validateDigits(number: CardNumber): AllErrorsOr[CardNumber] =
        if (number.cardNumber.forall(_.isDigit)) number.validNec
        else CardNumberContainsWrongCharacters.invalidNec

      validateLength andThen validateDigits
    }

    def validateExpirationDate(date: String): AllErrorsOr[ExpirationDate] = {
      def convertStringToExpirationDate(date: String): AllErrorsOr[ExpirationDate] = {
        val splittedDate = date.split("/").toList
        val month: Int = splittedDate.head.toInt
        val year: Int = splittedDate(1).toInt
        if (month > 12) {
          ExpirationDateWrongFormat.invalidNec
        } else {
          ExpirationDate(Month.of(month), Year.of(year)).validNec
        }
      }

      if (date.matches("^[0-1][0-9]\\/[0-9]{2}$")) convertStringToExpirationDate(date)
      else ExpirationDateWrongFormat.invalidNec
    }

    def validateSecurityCode(code: String): AllErrorsOr[SecurityCode] =
      if (code.matches("^[0-9]{4}$")) SecurityCode(code).validNec
      else SecurityCodeWrongFormat.invalidNec

    def validate(
        name: String,
        number: String,
        expirationDate: String,
        securityCode: String,
    ): AllErrorsOr[PaymentCard] =
      (validateName(name),
          validateCardNumber(number),
          validateExpirationDate(expirationDate),
          validateSecurityCode(securityCode)).mapN(PaymentCard)
  }
}

// Attributions and useful links:
// https://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html#error-handling
// https://www.geeksforgeeks.org/scala-exception-handling/
// https://typelevel.org/cats/datatypes/validated.html
// https://blog.ssanj.net/posts/2019-08-18-using-validated-for-error-accumulation-in-scala-with-cats.html
