package lesson6

import lesson6.SimpleRNG._
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit._

class SimpleRNGSuite {
  trait TestSets {
//    val n3: Stream[Int] = Stream(1,2,3)
//    val n5: Stream[Int] = Stream(1,2,3,4,5)
  }

  @Test def `RNG test`(): Unit = {
    val rng = Simple(42)
    assertEquals(rng, Simple(42))

    val (n1, rng2) = rng.nextInt
    assertEquals(n1, 16159453)
    assertEquals(rng2, Simple(1059025964525L))

    val (n2, rng3) = rng2.nextInt
    assertEquals(n2, -1281479697)
    assertEquals(rng3, Simple(197491923327988L))
  }

  @Test def `ex6.2)`(): Unit = {
    val rng = Simple(42)
    println(double(rng))
    assertEquals(Int.MaxValue, 2147483647)
    assertTrue(Int.MaxValue.toDouble == 2.147483647E9)
    assertTrue(Int.MaxValue.toDouble + 1 == 2.147483648E9)
  }

  @Test def `ex6.3)`(): Unit = {
    val rng = Simple(42)
    println(intDouble(rng))
  }

  @Test def `ex6.4)`(): Unit = {
    val rng = Simple(42)
    println(ints(4)(rng))
  }

  @Test def `ex6.5)`(): Unit = {
    val rng = Simple(42)
    val (d, rng1) = double2(rng)
    println(d)
    println(rng1)
  }
}