package lesson3

import fpinscala.datastructures._
import lesson3.ex10._

object ex17 {
  def doubleToString(as: List[Double]): List[String] = as match {
    case Nil => Nil
    case Cons(x: Double, xs: List[Double]) => Cons(x.toString, doubleToString(xs))
  }

  def main(args:Array[String]): Unit = {
//    val l = makeList((1 to 4).toList)
    val ld = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Cons(5.0 ,Nil)))))

    println(doubleToString(ld))
  }
}
