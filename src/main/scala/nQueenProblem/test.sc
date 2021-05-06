val n = 5
val m = 4
val s = for {
  i <- 0 to n
  j <- 0 to m
} yield (i,j)
s


val q = List(List(0,0),List(3,1),List(1,0),List(4,1))
q flatMap(qq =>
  0 until 2 map (d => d :: qq ))