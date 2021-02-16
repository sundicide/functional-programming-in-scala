package lesson2

object ex1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(acc:Int, curr: Int): Int =
      if (curr == 1) acc
      else loop(acc * curr, curr - 1)

    loop (1, n)
  }

  def main(args: Array[String]): Unit =
    println(fib(3))
}

