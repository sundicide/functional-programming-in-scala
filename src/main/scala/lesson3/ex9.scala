package lesson3

import fpinscala.datastructures._
import fpinscala.datastructures.List._

object ex9 {
  def length[A](as: List[A]): Int =
    foldRight(as , 0)((_, x) => 1 + x)

  def main(args: Array[String]): Unit = {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    println(length(a))
  }
}

