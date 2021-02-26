package lesson3

import fpinscala.datastructures._

object ex26 {
  def maximum(xs: Tree[Int]): Int = {
    def loop(remain: Tree[Int]): Int = remain match {
      case Leaf(value: Int) => value
      case Branch(left: Tree[Int], right: Tree[Int]) => maximum(left) max maximum(right)
    }
    loop(xs)
  }

  def main(args:Array[String]): Unit = {
    val t1 = Branch(Leaf(1), Leaf(2))
    val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    println(maximum(t1))
    println(maximum(t2))
  }
}
