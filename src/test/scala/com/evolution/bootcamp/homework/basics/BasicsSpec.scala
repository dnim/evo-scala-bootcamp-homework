package com.evolution.bootcamp.homework.basics

import com.evolution.bootcamp.homework.basics.Basics._
import org.scalatest.flatspec.AnyFlatSpec

class BasicsSpec extends AnyFlatSpec {

  "GCD" should "return right values" in {
    assert(gcd(0, 0) == 0)
    assert(gcd(0, 2) == 0)
    assert(gcd(-20, 0) == 0)
    assert(gcd(10, 20) == 10)
    assert(gcd(54, 24) == 6)
    assert(gcd(-54, -24) == 6)
    assert(gcd(7, 2) == 1)
  }

  "LCM" should "return right values" in {
    assert(lcm(0, 0) == 0)
    assert(lcm(0, 2) == 0)
    assert(lcm(-20, 0) == 0)
    assert(lcm(16, 20) == 80)
    assert(lcm(10, 20) == 20)
    assert(lcm(54, 24) == 216)
    assert(lcm(-54, -24) == 216)
    assert(lcm(7, 2) == 14)
    assert(lcm(101, 1) == 101)
  }
}
