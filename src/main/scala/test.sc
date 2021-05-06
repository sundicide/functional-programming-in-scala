import scala.annotation.tailrec

val a = List(1,2,3)


@tailrec
def loop(xs: List[Int], i: Int, acc: List[Int]): List[Int] = {
  if (xs.isEmpty) acc
  else
    if (i % 2 == 0) loop(xs.tail, i+1, acc :+ xs.head)
    else loop(xs.tail, i+1, acc)
}
println(loop(a, 0, Nil))


println((0 to 10 by 1).toList)

val s = a updated (2, 1)
println(s)

println(List.range(1,1) map (j => (2, j)))

println(List.range(1,1))

val line25 = for {
  i <- 1 to 5
} yield i
println(line25)

def flatMapTest[A](a: List[Option[A]]): Unit = a match {
  case h :: t => println(h); println(t)
}

flatMapTest(List(Some(1), Some(2)))

val xs = Map("a" -> List(List(11),111), "b" -> List(List(22),222)).flatMap((a => a._2))
val ys = Map("a" -> List(1 -> 11,4 -> 111), "b" -> List(2 -> 22,9 -> 222)).flatMap(_._2)
println(ys)

val ones: Stream[Int] = Stream.cons(1, ones)

ones.take(5).toList

ones.map(_ + 1).exists(_ % 2 == 0)

ones.takeWhile(_ == 1)

