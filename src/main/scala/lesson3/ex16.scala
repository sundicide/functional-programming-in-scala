package lesson3

import fpinscala.datastructures.List.foldRight
import fpinscala.datastructures._
import lesson3.ex10._

import scala.annotation.tailrec

object ex16 {
  def addOneToAllMembers(as: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(x: Int, xs: List[Int]) => Cons(x+1, addOneToAllMembers(xs))
  }

  def main(args:Array[String]): Unit = {
    val l = makeList((1 to 4).toList)

    println(addOneToAllMembers(l))
  }
}
