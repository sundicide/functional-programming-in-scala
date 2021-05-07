import fpdesign.week2.primes.from

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  print(lo + " ")
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

streamRange(1, 10).tail(1)


def expr = {
  val x = { print("x"); 1 }
  lazy val y = { print("y"); 2 }
  def z = { print("z"); 3 }

  z + y + x + z + y + x
}
expr

def from(n: Int): Stream[Int] = n #:: from(n+1)

val nats = from(0)

val m4s = nats map (_ * 4)

(m4s take 100).toList


// Sieve of Eratosthenes
// ancient technique to calculate prime number
// 1. 2부터 시작
// 2. 소수가 있으면 그의 배수가 되는 항목들은 제외한다.
// 3. 다음 숫자로 넘어간다.
// 4. 반복
def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))
primes.take(100).toList


def sqrtStream(x: Double): Stream[Double] = {
  def imporve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map imporve)
  guesses
}
sqrtStream(4).take(10).toList

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.0001

sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList
