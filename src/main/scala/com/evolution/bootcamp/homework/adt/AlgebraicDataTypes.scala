package com.evolution.bootcamp.homework.adt

object AlgebraicDataTypes {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // Attributions and useful links:
  // https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
  // https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
  // https://en.wikipedia.org/wiki/Algebraic_data_type

  /**
   * Copy-paste from Texas Hold'em task
   * TODO: not really clear how it's better to use with 'classic' ADT approach
   */

  sealed abstract class Suit(val suitChar : Char)
  case object Hearts   extends Suit('h')
  case object Diamonds extends Suit('d')
  case object Clubs    extends Suit('c')
  case object Spades   extends Suit('s')

  //"A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"
  sealed abstract class Rank(val rankChar : Char, val rankValue: Int)
  case object Two   extends Rank('2', 1)
  case object Three extends Rank('3', 2)
  case object Four  extends Rank('4', 3)
  case object Five  extends Rank('5', 4)
  case object Six   extends Rank('6', 5)
  case object Seven extends Rank('7', 6)
  case object Eight extends Rank('8', 7)
  case object Nine  extends Rank('9', 8)
  case object Ten   extends Rank('T', 9)
  case object Jack  extends Rank('J', 10)
  case object Queen extends Rank('Q', 11)
  case object King  extends Rank('K', 12)
  case object Ace   extends Rank('A', 13)

  case class Card(rank: Rank, suit: Suit)

  sealed trait Hand
  case class Texas(hand: List[Card]) extends Hand
  case class Omaha(hand: List[Card]) extends Hand

  case class Board(board: List[Card])

  sealed abstract class Combination(val value : Int)
  case object HighCard      extends Combination(1)
  case object Pair          extends Combination(2)
  case object TwoPairs      extends Combination(3)
  case object ThreeOfKind   extends Combination(4)
  case object Straight      extends Combination(5)
  case object Flush         extends Combination(6)
  case object FullHouse     extends Combination(7)
  case object FourOfKind    extends Combination(8)
  case object StraightFlush extends Combination(9)

}
