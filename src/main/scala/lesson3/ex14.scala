package lesson3

import fpinscala.datastructures.List.foldRight
import fpinscala.datastructures._
import lesson3.ex10._

import scala.annotation.tailrec

object ex14 {
  def append[A](as: List[A], bs: List[A]): List[A] = {
//    def loop(remain: List[A], accu: List[A]): List[A] = remain match {
//      case Nil => bs
//      case Cons(x, xs) => Cons(x, loop(xs)
//    }

//    foldRight(as, bs)((x, y) => x match {
//      case Nil => bs
//      case (_, _) => Cons(_, _)
//    })
    Nil
  }

  def main(args:Array[String]): Unit = {
//    val l = makeList((1 to 4).toList)
//    val ld = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Cons(5.0 ,Nil)))))
//
//    val s = List(1,2,3)
//
//    println(reverse(l))
  }
}
