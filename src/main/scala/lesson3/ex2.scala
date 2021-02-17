package lesson3

import fpinscala.datastructures._

object ex2 {
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def main(args: Array[String]): Unit = {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    println(tail(a))

    val b = Nil
    println(tail(b))
  }
}

