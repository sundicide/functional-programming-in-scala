package lesson6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  // ex)6.1 RNG.nextInt를 이용해서 0 이상, Int.maxValue 이하의 난수 정수를 생성하는 함수를 작성
  // nextInt가 Int.MinValue를 돌려주는 구석진 경우도 확실하게 처리해야 한다.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng1) = rng.nextInt
    (if (i1 < 0) (i1 >>> 16) else i1, rng1)
  }
}
