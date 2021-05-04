package fpdesign.week1

case class Book(title: String, authors: List[String])

object QueriesWithFor {
  val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Ordersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  def queryBooks() =
    for (b <- books; a <- b.authors if a startsWith "Bird,") yield b.title
  def queryBooks2() =
    for (b <- books if b.title.indexOf("Program") >= 0) yield b.title
  def queryBooks3() =
    for {
      b1 <- books
      b2 <- books
      if b1 != b2
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1

  /**
   * 중복을 해결한 버전1.
   * 하지만 저자가 3권 이상의 책을 출판했을 경우 여전히 중복이 생기는 문제가 있다.
   */
  def queryBooks4() =
    for {
      b1 <- books
      b2 <- books
      if b1.title < b2.title
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1

  /**
   * 중복을 해결한 버전2.
   * 한 저자가 여러 개의 책을 출판했어도 중복을 제거할 수 있다.
   */
  def queryBooks5() =
    { for {
      b1 <- books
      b2 <- books
      if b1.title < b2.title
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1 }.distinct



  def main(args:Array[String]): Unit = {
    println(queryBooks())
    println(queryBooks2())
    println(queryBooks3())
    println(queryBooks4())
    println(queryBooks5())
  }
}
