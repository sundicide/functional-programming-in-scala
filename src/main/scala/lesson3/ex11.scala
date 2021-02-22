package lesson3

import fpinscala.datastructures._
import lesson3.ex10._

object ex11 {
  def sum(x: List[Int], z: Int): Int = foldLeft(x, z)(_ + _)

  def product(x: List[Double], z: Double): Double = foldLeft(x, z)(_ * _)

  def length[A](x: List[A]): Int = foldLeft(x, 0)((x: Int, _) => 1 + x)

  def main(args:Array[String]): Unit = {
    val l = makeList((1 to 4).toList)
    println(sum(l, 0))
    val ld = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Cons(5.0 ,Nil)))))
    println(product(ld, 1.0))

    println(length(l))
    println(length(ld))
  }
}
