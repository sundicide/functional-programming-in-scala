package lesson2

object ex4 {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def main(args: Array[String]): Unit =
    println("Hello World")
}

