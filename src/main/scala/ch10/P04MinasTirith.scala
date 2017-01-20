package ch10

import scala.annotation.tailrec

object P04MinasTirith {

  import scala.io._

  val in: String =
    """3
      |10
      |7.02066050 -3.83540431 4.0
      |-7.23257714 -3.41903904 2.0
      |0.00000000 -8.00000000 8.0
      |-8.00000000 -0.00000000 4.8
      |-6.47213595 4.70228202 3.2
      |-4.70228202 6.47213595 4.8
      |7.60845213 -2.47213595 1.6
      |-2.47213595 -7.60845213 8.8
      |6.47213595 4.70228202 7.6
      |-0.00000000 8.00000000 4.8
      |4
      |8.00000000 0.00000000 8.00
      |0.00000000 -8.00000000 8.00
      |-8.00000000 -0.00000000 8.00
      |1.25147572 7.90150672 5.40
      |1
      |8 0 15.99
      |""".stripMargin

  private val MaxValue = 987654321

  def main(args: Array[String]): Unit = {
    //println(s"PI = ${2 * Math.PI}")
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    (1 to testCount).foreach { testNo =>
      val cntPoints = source.next().toInt
      val watchBoxColl = (0 until cntPoints).map { _ =>
        val Array(y, x, r) = source.next().split(" ").map(_.toDouble)
        WatchBox(y, x, r)
      }

      //println(s"-- testCase $testNo --")
      //println(s"WatchBoxes: ")
      //watchBoxColl.foreach(println)
      println(solve(watchBoxColl))
    }
  }

  def solve(watchBoxColl: Seq[WatchBox]): String = {
    val angleRangeColl = watchBoxColl.map(convertToAngleRange).sortBy(_.begin)
    //angleRangeColl.foreach(println)

    @tailrec
    def auxLinear(begin: Double, end: Double, auxAngleRangeColl: Seq[AngleRange], result: Int): Int = {
      //println(s"begin=$begin, end=$end, result=$result")
      if (begin >= end) result
      else {
        val coll = auxAngleRangeColl.filter(_.begin <= begin)
        if (coll.isEmpty) MaxValue
        else {
          val maxValue = coll.maxBy(_.end)
          auxLinear(maxValue.end, end, auxAngleRangeColl.dropWhile(_ != maxValue).tail, result + 1)
        }
      }
    }

    val result = angleRangeColl.filter(range => range.begin <= 0 || range.end >= 2 * Math.PI).map { range =>
      val begin = range.end % (2 * Math.PI)
      val end = (range.begin + 2 * Math.PI) % (2 * Math.PI)
      1 + auxLinear(begin, end, angleRangeColl, 0)
    }.min
    if (result >= MaxValue) "IMPOSSIBLE"
    else result.toString
  }

  def convertToAngleRange(point: WatchBox): AngleRange = {
    val loc = (2 * Math.PI + Math.atan2(point.y, point.x)) % (2 * Math.PI)
    val range = 2 * Math.asin(point.r / 2 / 8)
    AngleRange(loc - range, loc + range)
  }

  case class WatchBox(y: Double, x: Double, r: Double)

  case class AngleRange(begin: Double, end: Double)

}

