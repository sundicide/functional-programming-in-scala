def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0

val x = maybeTwice(true, { println("hi"); 1 + 41 })

def maybeTwice2(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j+j else 0
}
val x = maybeTwice2(true, { println("hi"); 1 + 41 })

