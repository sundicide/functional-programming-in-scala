package lesson3

import fpinscala.datastructures._

object ex27 {
  def depth[A](xs: Tree[A]): Int = {
    def loop(remain: Tree[A], accu: Int): Int = remain match {
      case Leaf(_) => accu
      case Branch(left: Tree[Int], right: Tree[Int]) => loop(left , accu+1) max loop(right, accu+1)
    }
    loop(xs, 0)
  }

  def main(args:Array[String]): Unit = {
    val t1 = Branch(Leaf(1), Leaf(2))
    val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val t3 = Branch(Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(2), Branch(Leaf(6), Leaf(7)))), Branch(Leaf(3), Leaf(4)))

    println(depth(t1))
    println(depth(t2))
    println(depth(t3))
  }
}
