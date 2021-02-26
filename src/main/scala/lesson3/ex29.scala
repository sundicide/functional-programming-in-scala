package lesson3

import fpinscala.datastructures._

object ex29 {
  def fold[A](xs: Tree[A])(f: (A, A) => A): A = xs match {
    case Leaf(x: A) => x
    case Branch(left: Tree[A], right: Tree[A]) => f(fold(left)(f), fold(right)(f))
  }

  def maximum(xs: Tree[Int]): Int = fold(xs)((x: Int, y: Int) => x max y)

  // TBD:
//  def depth
//  def size
//  def map

  def main(args:Array[String]): Unit = {
    val t1 = Branch(Leaf(1), Leaf(2))
    val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val t3 = Branch(Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(2), Branch(Leaf(6), Leaf(7)))), Branch(Leaf(3), Leaf(4)))

    println(fold(t1)(_ + _))
    println(fold(t2)(_ + _))

    println(maximum(t2))
  }
}
