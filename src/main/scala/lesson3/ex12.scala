package lesson3

import fpinscala.datastructures._
import lesson3.ex10._

import scala.annotation.tailrec

object ex12 {
  def reverse[A](as: List[A]): List[A] = {

    @tailrec
    def loop(bs: List[A], accu: List[A]): List[A] = bs match {
      case Nil => accu
      case Cons(x, xs) => loop(xs, Cons(x, accu))
    }

    loop(as, Nil)
  }

  def main(args:Array[String]): Unit = {
    val l = makeList((1 to 4).toList)
    val ld = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Cons(5.0 ,Nil)))))

    val s = List(1,2,3)

    println(reverse(l))
  }
}
