import scala.annotation.tailrec

object higherOrderFunctions {
  def sumInts(a: Int, b: Int): Int =
    if (a > b) 0 else a + sumInts(a + 1, b)

  def cube(x: Int): Int = x * x * x

  def sumCubes(a: Int, b: Int): Int =
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)


  def fact(n: Int): Int =
    if (n == 0) 1 else n * fact(n - 1)

  def sumFactorials(a: Int, b: Int): Int =
    if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)
}

/**
 * 1차 HOF 로 변경
 */
object higherOrderFunctions1 {
  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)

  def id(x: Int): Int = x
  def sumInts(a: Int, b: Int) = sum(id, a, b)

  def cube(x: Int): Int = x * x * x
  def sumCubes(a: Int, b: Int) = sum(cube, a, b)

  def fact(n: Int): Int = if (n == 0) 1 else n * fact(n - 1)
  def sumFactorials(a: Int, b: Int) = sum(fact, a, b)
}

/**
 * 위에서는 모든 내부함수들에 name 을 지어줘야 했다.
 * Anonymous function 으로 이를 해결한다.
 */
object higherOrderFunctions2 {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }
  def sumInts(a: Int, b: Int) = sum(x => x, a, b)

  def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)
}

object currying {
  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  def fact(n: Int) = product(x => x)(1, n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  product2(x => x * x)(3, 4)
}

object fixedPoint {
  import math.abs

  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double): Boolean =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    @tailrec
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  fixedPoint(x => 1 + x/2)(1) // 1.9975

  def sqrt(x: Double) = fixedPoint(y => x / y)(1)

  /**
   * infinity computation!!!!
   * 2와 1일 계속 요동 치기 떄문이다.
   */
  //  sqrt(2)

  /**
   * 위의 무한 계산을 피하기 위해 두 개의 값을 구한 뒤 평균 값을 구하도록 한다.
   * 첫 번째 계산 값은 y가 되고 두 번째 값은 x / y가 되니 이 둘을 더한 값에 2를 나누면 된다.
   * @param x
   * @return
   */
  def sqrt2(x: Double) = fixedPoint(y => (y + x / y) / 2)(1)
  sqrt(2) // 1.4142

  /**
   * averageDamp로 추상화한 버전
   */
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
  def sqrt3(x: Double) = fixedPoint(averageDamp(y => x / y))(1)
}

object functionsAndData {
  object rationals {
    val x = new Rational(1, 3)
    val y = new Rational(5, 7)
    val z = new Rational(3, 2)

    x.max(y)
    new Rational(2)
  }
  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be nonezero")

    def this(x: Int) = this(x, 1) // 여기에서의 this는 constructor 의미로 쓰인다.

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y) // val로 선언했기에 바로 평가되서 다음 부턴 계산을 안하고 재사용한다.
    def numer = x / g
//    def numer = x / gcd(x,y) // 만약 이와 같이 선언 하면 매번 gcd를 계산해야 한다. 계산 리소스가 크고 가끔 호출될 때 사용하면 좋다.
    def denom = y / g

//    def less(that: Rational) = numer * that.denom < that.numer * denom
    def < (that: Rational) = numer * that.denom < that.numer * denom
    def max(that: Rational) = if (this.<(that)) that else this

    def +(r: Rational) =
      new Rational(numer * r.denom + r.numer * denom,
        denom * r.denom)

    def mul(r: Rational) =
      new Rational(numer * r.numer,
        denom * r.denom)

    def unary_- : Rational = new Rational(-numer, denom)

    def -(that: Rational) = this + -that

    override def toString = s"$numer/$denom"
  }
}