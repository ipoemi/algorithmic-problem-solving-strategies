package ch09

import com.util.memoize

import scala.annotation.tailrec

object P04KLIS {

  import scala.io._

  val in: String =
    """4
      |8 6
      |5 1 6 4 3 2 8 7
      |8 4
      |2 1 4 3 6 5 8 7
      |8 2
      |5 6 7 8 1 2 3 4
      |9 2
      |1 9 7 4 2 6 3 11 10
      |""".stripMargin

  val MaxValue: Int = 2000000000 + 1

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    (1 to testCount).foreach { testNo =>
      val Array(_, k) = source.next().split(" ").map(_.toInt)
      val seq = source.next().split(" ").map(_.toInt).toSeq

      //println(s"-- testCase $testNo --")
      //println(s"n = $n, k = $k")
      //println(s"sequence: $seq")
      println(kthLis(seq, k))
    }
  }

  def kthLis(coll: Seq[Int], k: Int): String = {
    lazy val lisAux: (Int) => Int = memoize {
      case (start) =>
        val candColl = for (next <- (start + 1) until coll.size if start == -1 || coll(next) > coll(start)) yield {
          1 + lisAux(next)
        }
        (candColl :+ 1).max
    }

    lazy val countAux: (Int) => (Int) = memoize {
      case (start) if lisAux(start) == 1 => 1
      case (start) =>
        val candColl = for (next <- (start + 1) until coll.size) yield {
          if ((start == -1 || coll(start) < coll(next)) && lisAux(start) == lisAux(next) + 1) countAux(next) else 0
        }
        candColl.foldLeft(0)((accu, item) => (accu + item).min(MaxValue.toInt))
    }

    @tailrec
    def kthAux(start: Int, skip: Int, lis: Seq[Int]): Seq[Int] = {
      val nextLis = if (start != -1) lis :+ coll(start) else lis
      val candColl = for {
        next <- (start + 1) until coll.size
        if (start == -1 || coll(start) < coll(next)) && lisAux(start) == lisAux(next) + 1
      } yield {
        (coll(next), next)
      }
      if (candColl.isEmpty) return nextLis
      val candStream = candColl.sorted.toStream.map { case (_, tIdx) => (tIdx, countAux(tIdx)) }
      val (idx, acc) = candStream.tail.scanLeft(candStream.head) {
        case ((_, tAcc), (tIdx, tCount)) => (tIdx, tAcc + tCount)
      }.dropWhile {
        case (_, tAcc) => tAcc < skip
      }.head
      kthAux(idx, skip - acc + countAux(idx), nextLis)
    }

    s"${lisAux(-1) - 1}\n${kthAux(-1, k, Vector()).mkString(" ")}"
  }

}
