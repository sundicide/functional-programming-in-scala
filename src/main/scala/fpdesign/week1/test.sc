
val f: PartialFunction[String, String] = { case "ping" => "pong" }

f("ping")
f.isDefinedAt("abc")

val g: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: rest =>
    rest match {
      case Nil => "two"
    }
}

g.isDefinedAt(List(1,2,3))
try {
  g(List(1,2,3)) // match error
} catch {
  case e: Exception => println(e)
}


val l = List(1,2,3)
val l2 = List(3,4,5)
val l3 = List(6,2,3)

l == l2
List("Bloch, Joshua", "Gafter, Neal") == List("Bloch, Joshua")

val k = 2
val k2 = 3
math.min(k, k2)

3 :: l


l3 == l3.sorted

l3 == l3


