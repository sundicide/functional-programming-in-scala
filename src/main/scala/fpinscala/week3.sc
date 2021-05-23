import java.util.NoSuchElementException

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  override def toString = "{" + left + elem + right + "}"

  override def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
}

class Empty extends IntSet {
  override def incl(x: Int) = new NonEmpty(x, new Empty, new Empty)
  override def contains(x: Int) = false
  override def toString = "."
  override def union(other: IntSet): IntSet = other
}


val t1 = new NonEmpty(3, new Empty, new Empty)
val t2 = t1 incl 4

object overrides {
  abstract class Base {
    def foo = 1
    def bar: Int
  }

  class Sub extends Base {
    override def foo = 2
    def bar = 3
  }
}

object Empty extends IntSet {
  def incl(x: Int) = new NonEmpty(x, new Empty, new Empty)
  def contains(x: Int) = false
  def union(other: IntSet): IntSet = other
}

object polymorphism {
  trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
  }

  /**
   * val 은 객체가 초기화될때 평가되지만 def는 참조할 때마다 평가된다.
   * @param head val로서 이미 구현했기에 함수 body에서 따로 구현 안해도 된다.
   * @param tail head와 동일
   * @tparam T
   */
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    override def isEmpty: Boolean = false
  }

  class Nil[T] extends List[T] {
    override def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  }

  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)
  }

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  nth(2, list)
  nth(-1, list) // Out of bounds
}
