package ch08

import scala.collection.mutable

object P09TrianglePathCount {

  import scala.io._

  val in: String =
    """2
      |4
      |1
      |1 1
      |1 1 1
      |1 1 1 1
      |4
      |9
      |5 7
      |1 3 2
      |3 5 5 6
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    (1 to testCount).foreach { testNo =>
      val cnt = source.next().toInt
      val triangle = (0 until cnt).map(_ => source.next().split(" ").toVector.map(_.toInt)).toVector

      //println(s"-- testCase $testNo --")
      //println(s"Cnt: $cnt")
      //println(s"Triangle:")
      //triangle.foreach { line => line.foreach(n => print(s"$n ")); println() }
      println(solve(triangle))
    }
  }

  def solve(triangle: Seq[Seq[Int]]): Int = {
    val pathCache: mutable.HashMap[(Int, Int), Int] = mutable.HashMap()
    val countCache: mutable.HashMap[(Int, Int), Int] = mutable.HashMap()

    def pathAux(y: Int, x: Int): Int = {
      if (y == triangle.size - 1) return triangle(y)(x)
      val cachedValue = pathCache.get((y, x))
      if (cachedValue.nonEmpty) return cachedValue.get
      pathCache((y, x)) = triangle(y)(x) + pathAux(y + 1, x).max(pathAux(y + 1, x + 1))
      pathCache((y, x))
    }

    def countAux(y: Int, x: Int): Int = {
      if (y == triangle.size - 1) return 1
      val cachedValue = countCache.get((y, x))
      if (cachedValue.nonEmpty) return cachedValue.get

      countCache((y, x)) =
          (if (pathAux(y + 1, x) >= pathAux(y + 1, x + 1)) countAux(y + 1, x) else 0) +
              (if (pathAux(y + 1, x + 1) >= pathAux(y + 1, x)) countAux(y + 1, x + 1) else 0)
      countCache((y, x))
    }

    countAux(0, 0)

  }

}
