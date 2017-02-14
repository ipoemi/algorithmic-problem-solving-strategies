package ch12

import scala.io._

object P03CanadaTrip {

  val in: String =
    """2
      |3 15
      |500 100 10
      |504 16 4
      |510 60 6
      |2 1234567
      |8030000 8030000 1
      |2 2 1
      |""".stripMargin

  case class City(position: Int, startNotice: Int, interval: Int)

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    //val startTime = System.nanoTime()
    (1 to testCount).foreach { testNo =>
      val Array(cityCnt, k) = source.next().split(" ").map(_.toInt)
      val cities = (0 until cityCnt).map { _ =>
        val line = source.next().split(" ").map(_.toInt)
        City(line(0), line(1), line(2))
      }

      //println(s"-- testCase $testNo --")
      //println(s"CityCnt=$cityCnt, k=$k")
      //println(s"Cities:")
      //cities.foreach(a => println(a))
      println(solve(cities: Seq[City], k: Int))
    }
    //println(s"Spent Time: ${(System.nanoTime() - startTime) / 1000.0}")
  }

  def solve(cities: Seq[City], k: Int): String = {
    def aux(dist: Int): Boolean = {
      cities.map { city =>
        (city.position.min(dist) - (city.position - city.startNotice)).max(0) / city.interval + 1
      }.sum >= k
    }

    val loopCnt = 100

    val result = (0 until loopCnt).foldLeft((-1, 8030001)) { (result, _) =>
      val mid = (result._1 + result._2) / 2
      if (aux(mid)) (result._1, mid)
      else (mid, result._2)
    }._2

    result.toString
  }

}

