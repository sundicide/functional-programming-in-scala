package lesson3

import fpinscala.datastructures._
import lesson3.ex10.makeList

object ex25 {
  def size[A](xs: Tree[A]): (Int, Int) = {
    def loopLeaf(remain: Tree[A]): Int = remain match {
      case Leaf(_) => 1
      case Branch(left: Tree[A], right: Tree[A]) => loopLeaf(left) + loopLeaf(right)
    }
    def loopBranch(remain: Tree[A]): Int = remain match {
      case Leaf(_) => 0
      case Branch(left: Tree[A], right: Tree[A]) => 1 + loopBranch(left) + loopBranch(right)
    }
    (loopLeaf(xs), loopBranch(xs))
  }

  def main(args:Array[String]): Unit = {
    val t1 = Branch(Leaf("a"), Leaf("b"))
    val t2 = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))

    println(size(t1))
    println(size(t2))
  }
}
