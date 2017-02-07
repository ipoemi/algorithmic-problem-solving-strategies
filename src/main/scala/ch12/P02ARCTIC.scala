package ch12

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io._

object P02ARCTIC {

  val in: String =
    """2
      |5
      |0 0
      |1 0
      |1 1
      |1 2
      |0 2
      |6
      |1.0 1.0
      |30.91 8
      |4.0 7.64
      |21.12 6.0
      |11.39 3.0
      |5.31 11.0
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    //val startTime = System.nanoTime()
    (1 to testCount).foreach { testNo =>
      val baseCnt = source.next().toInt
      val basePoses = (0 until baseCnt).map(_ => source.next().split(" ").map(_.toDouble)).map(ar => (ar(0), ar(1)))

      //println(s"-- testCase $testNo --")
      //println(s"BaseCnt: $baseCnt")
      //println(s"Base Position List: ")
      //basePoses.foreach(a => println(a))
      println(solve(basePoses))
    }
    //println(s"Spent Time: ${(System.nanoTime() - startTime) / 1000.0}")
  }

  def solve(basePoses: Seq[(Double, Double)]): String = {
    val distances = basePoses.map { from =>
      basePoses.map { to =>
        Math.sqrt(Math.pow(to._1 - from._1, 2) + Math.pow(to._2 - from._2, 2))
      }
    }
    //distances.foreach(println)

    @tailrec
    def auxExists(distance: Double,
                  queue: Queue[Int] = Queue(0),
                  visited: Seq[Boolean] = Seq.fill(distances.size)(false)): Boolean = {
      if (queue.isEmpty) {
        visited.forall(identity)
      } else {
        val cur = queue.head
        val indices = visited.zipWithIndex.filter(!_._1).filter { i =>
          distances(cur)(i._2) <= distance
        }.map(_._2)
        val newQueue = indices.foldLeft(queue.tail) { (nQueue, i) =>
          nQueue :+ i
        }
        val newVisited = indices.foldLeft(visited) { (nVisited, i) =>
          nVisited.updated(i, true)
        }
        auxExists(distance, newQueue, newVisited)
      }
    }

    val loopCnt = 100

    val result = (0 until loopCnt).foldLeft((0.0, 1416.0)) { (result, _) =>
      val mid = (result._1 + result._2) / 2.0
      if (auxExists(mid)) (result._1, mid)
      else (mid, result._2)
    }._1

    val rounded = Math.round(result * 100.0) / 100.0
    rounded.toString
  }

}

