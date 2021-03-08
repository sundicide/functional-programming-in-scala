package fpinscala.datastructures

import org.junit._
import org.junit.Assert.assertEquals

class OptionSuite {
 trait TestSets {
  val a = lesson4.Some(2)
  val aa = lesson4.Some(2.0)
  val s = lesson4.None
  val b = lesson4.Some(3)
  val bb = lesson4.Some(3.0)
 }

 @Test def `map`: Unit = {
  new TestSets {
   assertEquals(a map (_ + 1), b)
  }
 }

 @Test def `filter`: Unit = {
   new TestSets {
    assertEquals(a filter(x => x == 1), lesson4.None)
    assertEquals(a filter(x => x == 2), lesson4.None)
   }
 }

 @Test def `orElse`: Unit = {
  new TestSets {
   assertEquals(a orElse b, a)
   assertEquals(s orElse b, b)
  }
 }
 @Test def `varaince`: Unit = {
  new TestSets {
    println(lesson4.Option.variance(Seq(2.0, 3.0)))
  }
 }
 def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age + numberOfSpeedingTickets

 def Try[A](a: => A): lesson4.Option[A] =
  try lesson4.Some(a)
  catch { case e: Exception => lesson4.None }


// def parseInsuranceRateQuote (
//  age: String,
//  numberOfSpeedingTickets: String): lesson4.Option[Double] = {
//   val optAge: lesson4.Option[Int] = Try(age.toInt) // 모든 string에 대해 toInt 메서드를 사용할 수 있다.
//   val optTickets: lesson4.Option[Int] = Try(numberOfSpeedingTickets.toInt)
//   insuranceRateQuote(optAge, optTickets) // 형식을 점검하지 않는다.
// }

 @Test def `map2`: Unit = {
  new TestSets {
   println(lesson4.Option.variance(Seq(2.0, 3.0)))
  }
 }
}
