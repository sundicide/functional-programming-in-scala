def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge (xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}
val nums = List(2, -4, 5, 7, 1)
msort(nums)

object generalizedMSort {
  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (lt(x, y)) x :: merge (xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  // x와 y에 타입을 지정 안해도 scala compiler가 Int를 인지 한다.
  msort(nums)((x, y) => x < y)

  val fruits = List("apple", "pineapple", "orange", "banana")
  // compareTo는 JAVA String 메서드이다.
  // first string 이 second string 보다 작으면 -1 같으면 0 크면 1을 리턴한다.
  msort(fruits)((x: String, y: String) => x.compareTo(y) < 0)
}

object usingOrdering {
  import math.Ordering
  def msort[T](xs: List[T])(ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge (xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst)(ord), msort(snd)(ord))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  // x와 y에 타입을 지정 안해도 scala compiler가 Int를 인지 한다.
  msort(nums)((Ordering.Int))

  val fruits = List("apple", "pineapple", "orange", "banana")
  // compareTo는 JAVA String 메서드이다.
  // first string 이 second string 보다 작으면 -1 같으면 0 크면 1을 리턴한다.
  msort(fruits)(Ordering.String)
}
object usingImplicit {
  import math.Ordering
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge (xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  // x와 y에 타입을 지정 안해도 scala compiler가 Int를 인지 한다.
  msort(nums)

  val fruits = List("apple", "pineapple", "orange", "banana")
  // compareTo는 JAVA String 메서드이다.
  // first string 이 second string 보다 작으면 -1 같으면 0 크면 1을 리턴한다.
  msort(fruits)
}

object listfun {
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  nums filter (x => x > 0)
  nums filterNot (x => x > 0)
  nums partition (x => x > 0)

  nums takeWhile (x => x > 0) // 2
  nums dropWhile (x => x > 0) // -4, 5, 7, 1
  nums span (x => x > 0) // (List(2), List(-4, 5, 7, 1))

  val data = List("a", "a", "a" , "b", "c" , "c", "a")
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span(y => y == x)
      first :: pack(rest)
  }
  pack(data) // List(List(a,a,a), List(b)...

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))

  encode(data) // List((a, 3), (b, 1), (c, 2), (a, 1))
}