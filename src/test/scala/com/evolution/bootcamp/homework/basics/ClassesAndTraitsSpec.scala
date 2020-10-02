package com.evolution.bootcamp.homework.basics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ClassesAndTraitsSpec extends AnyFlatSpec {

  "2D shapes" should "return right areas" in {
    val circle: Circle = Circle(10, 5, 3.3)
    circle.area() shouldEqual 34.21 +- 0.1
  }

  "3D shapes" should "return right surface areas" in {
    val sphere: Sphere = Sphere(3, 4, 5, 10)
    sphere.surfaceArea() shouldEqual 1256.63 +- 0.1
  }

  "3D shapes" should "return right volumes" in {
    val sphere: Sphere = Sphere(3, 4, 5, 10)
    sphere.volume() shouldEqual 3141.59 +- 0.1
  }

}
