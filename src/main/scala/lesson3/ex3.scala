package lesson3

import fpinscala.datastructures._

object ex3 {
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(x, xs)
  }

  def main(args: Array[String]): Unit = {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    val b = 9
    println(setHead(a, b))

    val c = Nil
    println(setHead(c, b))
  }
}

