package ch08

import scala.collection.mutable

object P03TrianglePath {

  import scala.io._

  val in: String =
    """2
      |5
      |6
      |1  2
      |3  7  4
      |9  4  1  7
      |2  7  5  9  4
      |5
      |1
      |2 4
      |8 16 8
      |32 64 32 64
      |128 256 128 256 128
      |""".stripMargin

  def solve(triagle: Seq[Seq[Int]], y: Int, x: Int, cache: mutable.HashMap[(Int, Int), Int] = mutable.HashMap()): Int = {
    if (y == triagle.size - 1) return triagle(y)(x)
    cache.getOrElseUpdate((y, x), {
      triagle(y)(x) + solve(triagle, y + 1, x).max(solve(triagle, y + 1, x + 1))
    })
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    (1 to testCount).foreach {
      testNo =>
        val lineCnt = source.next().toInt

        val triagle = (0 until lineCnt).map(_ => source.next().split("\\s+").toVector.map(_.trim.toInt))

        //println(s"-- testCase $testNo --")
        //println(s"Triagle:")
        //for (row <- triagle) {
        //	for (x <- row) print(s"$x ")
        //	println()
        //}
        println(solve(triagle, 0, 0))
    }
  }

}
