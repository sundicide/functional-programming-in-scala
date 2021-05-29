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

object pairs {
  val n = 7
  val xss = (1 until n) map (i =>
    (1 until i) map (j => (i, j)))

  (xss foldRight Seq[Int]())(_ ++ _)

  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

  (1 until n) flatMap(i =>
    (1 until i) map (j => (i, j))) filter (pair =>
    isPrime(pair._1 + pair._2))

  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

  def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
    (for {
      x <- xs
      y <- ys
    } yield x * y).sum
  }
}

val capitalOfCountry = Map("US" -> "Washington")
object maps {

  capitalOfCountry get "andorra"
  capitalOfCountry get "US"

  def showCapital(country: String) = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "missing data"
  }

  showCapital("US")
  showCapital("Andorra")
}

val fruit = List("apple", "pear", "orange", "pineapple")
fruit sortWith (_.length < _.length) // List("pear", "apple", "orange", "pineapple")
fruit.sorted // List("apple", "orange", "pear", "pineapple")

fruit groupBy (_.head) // Map(p -> List(pear, pineapple), a -> List(apple), o -> List(orange))

object polynomials {
  class Poly(val terms: Map[Int, Double]) {
    def + (other: Poly) = new Poly(terms ++ other.terms map adjust)
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      terms get exp match {
        case Some(coeff1) => exp -> (coeff + coeff1)
        case None => exp -> coeff
      }
    }

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }

  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
  p1 + p2
}

val cap1 = capitalOfCountry withDefaultValue "unknown"
cap1("Andorra") // "Unknown"

object polynomialsWithDefaults {
  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
    def addTerm(terms: Map[Int, Double],  term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }

  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
  p1 + p2
}

object x {
  import scala.io.Source

  val in = Source.fromURL("./linuxwords.txt")

  val mnemonics = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  def run(): Unit = {
    println(in)
  }
}
x.run()

