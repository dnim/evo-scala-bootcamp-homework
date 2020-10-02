package com.evolution.bootcamp.homework.basics

import java.lang.Math._

object ClassesAndTraits extends App {

  val triangle: Triangle = new Triangle((11, 22), (27, 41), (53,20))

}

sealed trait Functions2D {
  def area(): Double
}

sealed trait Functions3D {
  def surfaceArea(): Double
  def volume(): Double
}

sealed trait Movable[A] {
  def move(dx: Double, dy: Double): Shape[A]
}

sealed trait Movable3D[A] {
  def move(dx: Double, dy: Double, dz: Double): Shape3D[A]
}

sealed trait Located {
  def x: Double
  def y: Double
}

sealed trait Located3D extends Located {
  def z: Double
}

sealed trait Bounded {
  def minX: Double
  def maxX: Double
  def minY: Double
  def maxY: Double
}

sealed trait Bounded3D extends Bounded {
  def minZ: Double
  def maxZ: Double
}

sealed trait Shape[A] extends Located with Bounded with Movable[A]

sealed trait Shape2D[A] extends Shape[A] with Functions2D

sealed trait Shape3D[A] extends Located3D with Bounded3D with Movable3D[A] with Functions3D

final case class Point(x: Double, y: Double) extends Shape2D[Point] {
  override def minX: Double = x
  override def maxX: Double = x
  override def minY: Double = y
  override def maxY: Double = y

  override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)

  override def area(): Double = 0
}

final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D[Circle] {
  override def x: Double = centerX
  override def y: Double = centerY
  override def minX: Double = x - radius
  override def maxX: Double = x + radius
  override def minY: Double = y - radius
  override def maxY: Double = y + radius

  override def move(dx: Double, dy: Double): Circle = Circle(x + dx, y + dy, radius)

  override def area(): Double = Math.PI * pow(radius, 2)
}

final case class Rectangle(bottomLeftX: Double, bottomLeftY: Double, height: Double, length: Double) extends Shape[Rectangle] {
  override def x: Double = bottomLeftX + height/2
  override def y: Double = bottomLeftY + length/2
  override def minX: Double = x
  override def maxX: Double = x + height
  override def minY: Double = y
  override def maxY: Double = y + length

  override def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, height, length)
}

final case class Triangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) extends Shape[Triangle] {

  def this(a: (Double, Double), b: (Double, Double), c: (Double, Double)) {
    this(a._1, a._2, b._1, b._2, c._1, c._2)
  }

  // Incenter of a triangle https://en.wikipedia.org/wiki/Incenter
  override def x: Double = (x1 * sideB + x2 * sideC + x3 * sideA) / perimeter
  override def y: Double = (y1 * sideB + y2 * sideC + y3 * sideA) / perimeter

  override def minX: Double = List[Double](x1, x2, x3).min
  override def maxX: Double = List[Double](x1, x2, x3).max
  override def minY: Double = List[Double](y1, y2, y3).min
  override def maxY: Double = List[Double](y1, y2, y3).max

  private val sideA: Double = sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2))
  private val sideB: Double = sqrt(pow(x2 - x3, 2) + pow(y2 - y3, 2))
  private val sideC: Double = sqrt(pow(x3 - x1, 2) + pow(y3 - y1, 2))
  private val perimeter: Double = sideA + sideB + sideC

  override def move(dx: Double, dy: Double): Shape[Triangle] = Triangle(x1 + dx, y1 + dy, x2 + dx, y2 + dy, x3 + dx, y3 + dy)
}

final case class Square(bottomLeftX: Double, bottomLeftY: Double, size: Double) extends Shape2D[Square] {
  override def move(dx: Double, dy: Double): Shape[Square] = Square(bottomLeftX + dx, bottomLeftY + dy, size)
  override def x: Double = bottomLeftX + size/2
  override def y: Double = bottomLeftY + size/2
  override def minX: Double = bottomLeftX
  override def maxX: Double = bottomLeftX + size
  override def minY: Double = bottomLeftY
  override def maxY: Double = bottomLeftY + size

  override def area(): Double = Math.pow(size, 2)
}

// 3D
final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D[Sphere] {

  override def minX: Double = x - radius
  override def maxX: Double = x + radius
  override def minY: Double = y - radius
  override def maxY: Double = y + radius
  override def minZ: Double = z - radius
  override def maxZ: Double = z + radius
  override def move(dx: Double, dy: Double, dz: Double): Shape3D[Sphere] = Sphere(x + dx, y + dy, z + dz, radius)

  override def surfaceArea(): Double = 4 * Math.PI * Math.pow(radius, 2)

  override def volume(): Double = 4/3 * Math.PI * Math.pow(radius, 3)
}

