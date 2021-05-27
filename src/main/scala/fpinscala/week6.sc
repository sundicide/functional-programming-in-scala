val xs = Array(1,2,3,44)
xs map (x => x * 2)

val s = "Hello World"
s filter (c => c.isUpper) // HW
s exists (c => c.isUpper) // true
s forall (c => c.isUpper) // false

val pairs = List(1,2,3) zip s // List((1, H), (2, e), (3, l))
pairs.unzip                   // (List(1,2,3), List(H,e,l))

s flatMap (c => List('.', c)) // .H.e.l.l.o. .W.o.r.l.d

xs.sum // 50
xs.max // 44


def combinations(m: Int, n: Int) =
  (1 to m) flatMap (x => (1 to n) map (y => (x, y)))

combinations(4,4)

def scalaProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum

// 위 식은 pattern matching function value로 아래처럼 쓸 수 있다.
def scalaProductUsingPattern(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{ case (x, y) => x * y}.sum

def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)