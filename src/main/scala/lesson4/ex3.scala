package lesson4

import fpinscala.datastructures._

object ex3 {
  def Try[A](a: => A): Option[A] = // a를 평가하는 도중에 에러가 발생하면 None을 돌려주기 위해 엄격하지 않은(=lazy 인수) 방식으로 받는다.
    try Some(a)
    catch { case e: Exception => None }

//  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
//    if (a == None || b == None) None
//    else f(Try(a), Try(b))
//  }

  def main(args:Array[String]): Unit = {
    val a = Some(2)
    val s = None
    val b = None
    println(a orElse b)
    println(s orElse a)
  }
}
