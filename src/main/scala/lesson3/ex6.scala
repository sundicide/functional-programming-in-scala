package lesson3

import fpinscala.datastructures._

import scala.annotation.tailrec

object ex6 {
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (xs == Nil) Nil
      else Cons(x, init(xs))
  }

  def main(args: Array[String]): Unit = {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    println(init(a))
  }
}

