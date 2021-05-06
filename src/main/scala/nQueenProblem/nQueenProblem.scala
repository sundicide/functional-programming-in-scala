package nQueenProblem

import scala.annotation.tailrec

object nQueenProblem {
  def printListInList[T](l: List[List[T]]): Unit = {
    println("======")
    for {
      ll <- l
    } println(ll)
    println("======")
  }

  def setOneToIndexInList(list: List[List[Int]], j: Int, k: Int):List[List[Int]] = {
    list.zipWithIndex.map(d => {
      val front = d._1.slice(0, k)
      val back = d._1.slice(k+1, d._1.length)
      if (d._2 == j) front ++ List(1) ++ back
      else d._1
    })
  }
  def makeChessTable(n: Int): List[List[Int]] = {
    (1 to n).map(_ => (1 to n).map(_ => 0).toList).toList
  }
  def solve(n: Int): Unit = {
    makeChessTable(n)

    def isCollision(currentChessTable: List[List[Int]], i: Int, j: Int, k: Int): Boolean = {
      val isUpOk = !currentChessTable.zipWithIndex.exists(p => p._2 < j && p._1(k) == 1)
      val isUpLeftOk = !(0 until j).exists(d => {
        val newJ = j - d - 1
        val newK = k - d - 1
        newJ >= 0 && newK >= 0 && currentChessTable(newJ)(newK) == 1
      })

      val isUpRightOk = !(0 until j).exists(d => {
        val newJ = j - d - 1
        val newK = k + d + 1
        newJ < n && newK < n && currentChessTable(newJ)(newK) == 1
      })

      !isUpOk || !isUpLeftOk || !isUpRightOk
    }
    def loop(): Unit = {
      (0 until n).foreach(d => {
        val ct = makeChessTable(n)
        val l = setOneToIndexInList(ct, 0, d)

        def innerLoop(ll:List[List[Int]], j: Int, k: Int): List[List[Int]] = {
          if (j == n) ll
          else if (k == n) Nil
          else {
            if (isCollision(ll, d, j, k))
              innerLoop(ll, j, k + 1)
            else {
              val newL = setOneToIndexInList(ll, j, k)
              innerLoop(newL, j+1, 0)
              innerLoop(newL, j, k+1)
            }
          }
        }
        printListInList(innerLoop(l, 1, 0))
      })
    }
    loop()
  }

  def main(args:Array[String]):Unit = {
    solve(5)
  }
}
