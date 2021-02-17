package lesson3

import fpinscala.datastructures._

import scala.annotation.tailrec

object ex5 {
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) xs
      else dropWhile(xs, f)
  }

  def main(args: Array[String]): Unit = {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    val b = 1
    println(dropWhile(a, (x:Int) => x == 2))
  }
}

