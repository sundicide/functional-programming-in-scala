val s: Seq[Double] = List(1,2,3,4)

println(s)

println(s.sum)

for (i <- 0 to 10) {
  println(i + 1)
}

println(math.pow(4, 2))

def factorial(n: Int): Int = n match {
  case 1 => 1
  case s => s * factorial(s-1)
}
println(factorial(4))

println(Some(2) flatMap (dd =>Some(dd+1)))