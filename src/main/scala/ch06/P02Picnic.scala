package ch06

object P02Picnic {

  import scala.io._

  val in: String =
    """3
      |2 1
      |0 1
      |4 6
      |0 1 1 2 2 3 3 0 0 2 1 3
      |6 10
      |0 1 0 2 1 2 1 3 1 4 2 3 2 4 3 4 3 5 4 5
      |""".stripMargin

  def aux(remain: Seq[Int], friendList: Seq[(Int, Int)], cur: Seq[(Int, Int)]): Int = {
    if (remain.isEmpty) {
      println(cur)
      return 1
    }
    val nextPairs = remain.tail.map((remain.head, _)).filter(friendList.contains(_))
    if (nextPairs.isEmpty) return 0
    nextPairs.map(pair => aux(remain.filter(x => x != pair._1 && x != pair._2), friendList, cur :+ pair)).sum
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next.toInt
    (1 to testCount).foreach { testNo =>
      val Array(studentCount, friendCount) = source.next.split(" ").map(_.toInt)
      val tmpPairList = source.next.split(" ").map(_.toInt).toVector
      val friendList = tmpPairList.sliding(2, 2).toVector.map(_.sorted).map { case Seq(n1, n2) => (n1, n2) }


      //println(s"-- testCase $testNo --")
      println(s"StudentCount: $studentCount, PairCount: $friendCount")
      println(s"PairList: $friendList")
      println(aux(0 until studentCount, friendList, Vector()))
    }
  }
}
