package com.evolution.bootcamp.homework.basics

import scala.annotation.tailrec

object Basics extends App {
  // Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
  // https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

  def lcm(a: Int, b: Int): Int =
    if (a == 0 || b == 0) 0
    else a / gcd(a, b) * b

  def gcd(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) 0
    else {
      val minValue: Int = Math.min(a.abs, b.abs)
      @tailrec
      def findGcd(result: Int): Int = {
        if (result == 1) result
        else if (a % result == 0 && b % result == 0) result
        else findGcd(result - 1)
      }
      findGcd(minValue)
    }
  }
}
