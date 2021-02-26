package lesson3

import fpinscala.datastructures._
import lesson3.ex10.makeList

object ex19 {
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x: A, xs: List[A]) =>
      if ((f(x))) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  def main(args:Array[String]): Unit = {
    val l = makeList((1 to 4).toList)
//    val ld = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Cons(5.0 ,Nil)))))

    println(filter(l)((x: Int) => x == 2 || x == 1))
  }
}
