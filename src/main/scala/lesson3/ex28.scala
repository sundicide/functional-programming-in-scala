package lesson3

import fpinscala.datastructures._

object ex28 {
  def map[A, B](xs: Tree[A])(f: A => B): Tree[B] = xs match {
      case Leaf(x: A) => Leaf(f(x))
      case Branch(left: Tree[A], right: Tree[A]) => Branch(map(left)(f), map(right)(f))
  }

  def main(args:Array[String]): Unit = {
    val t1 = Branch(Leaf(1), Leaf(2))
    val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val t3 = Branch(Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(2), Branch(Leaf(6), Leaf(7)))), Branch(Leaf(3), Leaf(4)))

    println(map(t1)((x: Int) => x + 1))
    println(map(t2)((x: Int) => x + 1))
    println(map(t3)((x: Int) => x + 1))
  }
}
