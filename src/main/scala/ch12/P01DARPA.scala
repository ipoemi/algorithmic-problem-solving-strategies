package ch12


object P01DARPA {

  import scala.collection.mutable
  import scala.io._

  val in: String =
    """3
      |2 4
      |80 100 120 140
      |4 4
      |80 100 120 140.00
      |4 7
      |0 70 90 120 200 210 220
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    val startTime = System.nanoTime()
    (1 to testCount).foreach { testNo =>
      val Array(cameraCnt, stationCnt) = source.next().split(" ").map(_.toInt)
      val stationPosition = source.next().split(" ").map(_.toDouble)

      println(s"-- testCase $testNo --")
      println(s"CameraCnt=$cameraCnt, StationCnt=$stationCnt ")
      println(s"Station Position: ${stationPosition.toSeq}")
      //println(s"Hit List: ")
      //hints.foreach(println)
      println(solve(stationPosition, cameraCnt))
    }
    println(s"Spent Time: ${(System.nanoTime() - startTime) / 1000.0}")
  }

  def solve(stationPosition: Seq[Double], cameraCnt: Int): String = {
    val cache = mutable.Map[Int, Option[Double]]()

    def aux(installed: Int, cur: Int): Option[Double] = {
      cache.getOrElseUpdate(installed, {
        if (Integer.bitCount(installed) == cameraCnt) {
          val indices = stationPosition.indices.filter(i => ((1 << i) & installed) != 0)
          if (indices.size > 1)
            Some(indices.tail.foldLeft((Double.MaxValue, indices.head)) {
              case ((min, last), i) => (min.min(stationPosition(i) - stationPosition(last)), i)
            }._1)
          else None
        } else if (cur >= stationPosition.size) {
          None
        } else {
          (aux(installed | (1 << cur), cur + 1), aux(installed, cur + 1)) match {
            case (None, None) => None
            case (Some(v), None) => Some(v)
            case (None, Some(v)) => Some(v)
            case (Some(v1), Some(v2)) => Some(v1.max(v2))
          }
        }
      })
    }

    /*
    lazy val aux: (Int, Int) => Option[Double] = Function.untupled(memoize {
      case (installed, _) if Integer.bitCount(installed) == cameraCnt =>
        val indices = stationPosition.indices.filter(i => ((1 << i) & installed) != 0)
        if (indices.size > 1)
          Some(indices.tail.foldLeft((Double.MaxValue, indices.head)) {
            case ((min, last), cur) => (min.min(stationPosition(cur) - stationPosition(last)), cur)
          }._1)
        else None
      case (_, cur) if cur >= stationPosition.size => None
      case (installed, cur) =>
        (aux(installed | (1 << cur), cur + 1), aux(installed, cur + 1)) match {
          case (None, None) => None
          case (Some(v), None) => Some(v)
          case (None, Some(v)) => Some(v)
          case (Some(v1), Some(v2)) => Some(v1.max(v2))
        }
    })
    */

    aux(0, 0).getOrElse(0.0).toString
  }

}

