package fpinscala.datastructures

import org.junit._
import org.junit.Assert.assertEquals

class OptionSuite {
 trait TestSets {
  val a = Some(2)
  val aa = Some(2.0)
  val s = None
  val b = Some(3)
  val bb = Some(3.0)
 }

 @Test def `map`: Unit = {
  new TestSets {
   assertEquals(a map (_ + 1), b)
  }
 }

 @Test def `filter`: Unit = {
   new TestSets {
    assertEquals(a filter(x => x == 1), None)
    assertEquals(a filter(x => x == 2), None)
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
    println(Option.variance(Seq(2.0, 3.0)))
  }
 }
}
