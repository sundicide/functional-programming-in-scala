package lesson3

import fpinscala.datastructures._

import scala.annotation.tailrec
import scala.collection.immutable.{List => Llist}

object ex10 {
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def loop(bs: List[A], ff: B): B = bs match {
      case Nil => ff
      case Cons(x, xs) => loop(xs, f(ff, x))
    }

    loop(as, z)
  }

  def makeList(remain: Llist[Int]): List[Int] = {
    if (remain.isEmpty) Nil
    else Cons(remain.head, makeList(remain.tail))
  }

  def main(args: Array[String]): Unit = {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    println(foldLeft(a, 0)(_ + _))

    val maxVal = 1000
    val s: Llist[Int] = (1 to maxVal by 1).toList


    val d = makeList(s)
//    println(foldRight(d, 0)(_ + _))
    println(foldLeft(d, 0)(_ + _))
  }
}

