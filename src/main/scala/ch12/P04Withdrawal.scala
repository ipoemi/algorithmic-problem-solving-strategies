package ch12

import scala.io._

object P04Withdrawal {

  val in: String =
    """3
      |3 2
      |1 4 6 10 10 17
      |4 2
      |4 8 9 12 3 10 2 5
      |10 5
      |70 180 192 192 1 20 10 200 6 102 60 1000 4 9 1 12 8 127 100 700
      |""".stripMargin

  case class City(position: Int, startNotice: Int, interval: Int)

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    //val startTime = System.nanoTime()
    (1 to testCount).foreach { testNo =>
      val Array(subjectCnt, minCnt) = source.next().split(" ").map(_.toInt)
      val rankAndCnts = source.next().split(" ").map(_.toInt).grouped(2).toVector.map(l => (l(0), l(1)))

      //println(s"-- testCase $testNo --")
      //println(s"Rank And Cnt:")
      //rankAndCnts.foreach(println)
      //println(s"CityCnt=$cityCnt, k=$k")
      //println(s"Cities:")
      //cities.foreach(a => println(a))
      println(solve(rankAndCnts, minCnt))
    }
    //println(s"Spent Time: ${(System.nanoTime() - startTime) / 1000.0}")
  }

  def solve(rankAndCnts: Seq[(Int, Int)], minCnt: Int): String = {
    def aux(avg: Double): Boolean = {
      val v = rankAndCnts.map(x => avg * x._2 - x._1).sorted
      v.takeRight(minCnt).sum >= 0
    }

    val loopCnt = 100

    val result = (0 until loopCnt).foldLeft((-1e-9, 1.0)) { (result, _) =>
      val mid = (result._1 + result._2) / 2
      if (aux(mid)) (result._1, mid)
      else (mid, result._2)
    }._2

    result.formatted("%.7f")
  }

}

