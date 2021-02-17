package lesson3

import fpinscala.datastructures._

import scala.annotation.tailrec

object ex4 {
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0)
      l
    else
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }

  def main(args: Array[String]): Unit = {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    val b = 1
    println(drop(a, b))

    val c = Nil
    println(drop(c, b))
  }
}

