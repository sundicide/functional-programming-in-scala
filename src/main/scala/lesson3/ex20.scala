package lesson3

import fpinscala.datastructures._
import lesson3.ex10.makeList

// TBD
object ex20 {
  //  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
  //    def loop(remain: List[A], accu: List[B]): List[B] = remain match {
  //      case Nil => accu
  //      case Cons(x, xs) => loop(xs, f(x))
  //    }
  //}

  def main(args:Array[String]): Unit = {
    val l = makeList((1 to 4).toList)
//    val ld = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Cons(5.0 ,Nil)))))

  }
}
