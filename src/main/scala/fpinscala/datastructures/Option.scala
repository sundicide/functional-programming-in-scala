package fpinscala.datastructures
import scala.{ List => Llist}

// 패턴 부합을 사용해도 좋으나 map과 getOrElse를 제외한 모든 함수는 패턴 부합 없이도 구현할 수 있어야 한다.
sealed trait Option[+A] {
  // 만일 Option이 None이 아니면 f를 적용한다.
  def map[B](f: A => B): Option[B] = this match {
    case Some(x: A) => Some(f(x))
    case None =>  None
  }

  // Option의 Some안의 결과를 돌려준다. 단, Options이 none이면 주어진 기본값을 돌려준다.
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // 만일 Option이 None이 아니면 f(실패할 수 있음)를 적용한다.
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  /*
    Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  // ob는 필요한 경우에만 평가한다.
  // 첫 Option이 정의되어 있으면 그것을 돌려주고 그렇지 않다면 둘째 Option을 돌려준다.
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    if (this == None) ob
    else this

  /*
    Again, we can implement this with explicit pattern matching.
  */
  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  /*
    This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  /* Or via explicit pattern matching. */
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  // 순차열의 평균이 m이라 할 때, 분산(variance)는 순차열의 각 요소에 대한 math.pow(x - m, 2)들의 평균이다.
  def variance(xs: Seq[Double]): Option[Double] = {
    val m: Double = (xs.sum) / xs.length
    val s = xs map (x=> math.pow(x - m, 2) / xs.length) reduceLeft (_ + _)
    Some(s)
  }
}
