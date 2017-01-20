package ch08

object P05JLIS {

  import scala.io._

  val in: String =
    """3
      |3 3
      |1 2 4
      |3 4 7
      |3 3
      |1 2 3
      |4 5 6
      |5 3
      |10 20 30 1 2
      |10 20 30
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    (1 to testCount).foreach {
      testNo =>
        val _ = source.next()
        val sequence1 = source.next().split(" ").toVector.map(_.toInt)
        val sequence2 = source.next().split(" ").toVector.map(_.toInt)

        //println(s"-- testCase $testNo --")
        println(s"Sequence1: $sequence1")
        println(s"Sequence2: $sequence2")
        println(solve(sequence1, sequence2))
    }
  }

  def solve(seq1: Seq[Int], seq2: Seq[Int]): Int = {
    if (seq1.isEmpty || seq2.isEmpty) return 0

    val cache: Array[Array[Option[Int]]] = Array.fill(seq1.size + 1)(Array.fill(seq2.size + 1)(None))

    def aux(idx1: Int, idx2: Int): Int = {
      val cachedValue = cache(idx1 + 1)(idx2 + 1)
      if (cachedValue.nonEmpty) return cachedValue.get
      var ret = 2
      val maxValue =
        (if (idx1 < 0) Long.MinValue else seq1(idx1)) max
            (if (idx2 < 0) Long.MinValue else seq2(idx2))
      for (i <- seq1.indices if i > idx1 && seq1(i) > maxValue) {
        ret = ret max (aux(i, idx2) + 1)
      }
      for (i <- seq2.indices if i > idx2 && seq2(i) > maxValue) {
        ret = ret max (aux(idx1, i) + 1)
      }
      cache(idx1 + 1)(idx2 + 1) = Some(ret)
      ret
    }

    aux(-1, -1) - 2
  }

}
