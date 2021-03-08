package lesson5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (n == 1) Cons(h, () => Empty)
      else Cons(h, () => t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) =>
      if (n == 1) t()
      else t().drop(n-1)
  }

  // ex5.3) Stream에서 주어진 술어를 만족하는 선행 요소들을 모두 돌려주는 함수
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) Cons(h, () => Empty)  else Cons(h, () => t().takeWhile(p))
  }

  // p90
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)
  //

  // ex5.4) 모든 요소가 주어진 술어를 만족하는지 점검하는 함수
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) => p(h()) && t().forAll(p)
  }
  def forAll2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) && b)

  // ex5.5 takeWhile using foldRight
  def takeWhile2(p: A => Boolean): Stream[A] = ???
//    foldRight(Empty)((a, b) => if (p(a)) b else Cons(() => a, () => Empty))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = { // 비지 않은 스트림을 위한 똑똑한 생성자
    lazy val head = hd // 평가 반복을 피하기 위해 head와 tail을 게으른 값으로서 캐싱한다.
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty // 특정 형식의 빈 스트림을 생성하기 위한 똑똑한 생성자

  def apply[A](as: A*): Stream[A] = // 여러 요소로 이루어진 Stream의 생성을 위한 편의용 가변 인수 메서드
   if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

