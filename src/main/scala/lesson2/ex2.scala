package lesson2

object ex2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n < as.length - 1)
        if (ordered(as(n), as(n+1))) loop(n + 1)
        else false
      else true
    loop (0)
  }

  def main(args: Array[String]): Unit =
    println(isSorted(Array(1, 2, 3, 4), (a: Int, b: Int) => (b - a) > 0))
}

