package lesson2

object ex5 {
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit =
    println("Hello World")
}

