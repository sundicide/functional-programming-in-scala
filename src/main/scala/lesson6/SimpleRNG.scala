package lesson6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object SimpleRNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

    // ex6.1) RNG.nextInt를 이용해서 0 이상, Int.maxValue 이하의 난수 정수를 생성하는 함수를 작성
    // nextInt가 Int.MinValue를 돌려주는 구석진 경우도 확실하게 처리해야 한다.
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i1, rng1) = rng.nextInt
      (if (i1 < 0) (i1 >>> 16) else i1, rng1)
    }

    // ex6.2) 0 이상, 1미만의 Double 난수를 발생하는 함수.
    def double(rng: RNG): (Double, RNG) = {
      val (i1, rng1) = nonNegativeInt(rng)
      (i1 / (Int.MaxValue.toDouble + 1), rng1)
    }

    // ex6.3) 각각 난수쌍 하나를 발생하는 함수들을 작성해라.
    // 앞에서 작성한 함수들을 재사용할 수 있어야 한다.
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i1, rng1) = nonNegativeInt(rng)
      val (d1, rng2) = double(rng1)
      ((i1, d1), rng2)
    }

    // ex6.3) 각각 난수쌍 하나를 발생하는 함수들을 작성해라.
    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (i1, rng1) = nonNegativeInt(rng)
      val (d1, rng2) = double(rng1)
      ((d1, i1), rng2)
    }

    // ex6.3) 각각 난수쌍 하나를 발생하는 함수들을 작성해라.
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng1) = double(rng)
      val (d2, rng2) = double(rng1)
      val (d3, rng3) = double(rng2)
      ((d1, d2, d3), rng3)
    }

    // ex6.4) 정수 난수들의 목록
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def loop(n: Int, rng: RNG, accu: List[Int]): (List[Int], RNG) = {
        if (n > count) (accu, rng)
        else {
          val (i1, rng1) = rng.nextInt
          loop(n + 1, rng1, i1 :: accu)
        }
      }

      loop(0, rng, List())
    }



  // p106~p107 codes
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 0보다 크거나 같고 2로 나누어지는 Int를 발생하는 함수
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // end codes

  // ex6.5) double을 map을 이해서 구현
  // double: 0 이상, 1미만의 Double 난수를 발생하는 함수.
  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // ex6.6) 두 상태 동작 ra 및 rb와 이들의 결과를 조합하는 함수 f를 받고
  // 두 동작을 조합한 새 동작을 돌려준다.
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  // p108 codes
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)
  // end codes

  // p110 codes
  def noneNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else
      noneNegativeLessThan(n)(rng2)
  }
  // end codes

  // ex6.8) flatMap을 구현하고 이를 이용해 nonNegativeLessThan을 구현해라.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a1, rng1) = f(rng)
      val (g1, rng2) = g(a1)(rng1)
      (g1, rng2)
    }
  }
  def noneNegativeLessThan2(i: Int): Rand[Int] = {
    rng => {
//      val (i, rng1) = nonNegativeInt(rng)

      flatMap(r => (i, nonNegativeInt(r)._2))(a => if (a >= 0) (rng2 => (1, rng2)) else noneNegativeLessThan2(a))(rng)
    }
  }
}
