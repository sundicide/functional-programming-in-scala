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
g(List(1,2,3)) // match error