package ch11

object P03Kakuro2 {

  import scala.io._

  val in: String =
    """1
      |8
      |0 0 0 0 0 0 0 0
      |0 1 1 0 0 1 1 1
      |0 1 1 0 1 1 1 1
      |0 1 1 1 1 1 0 0
      |0 0 1 1 0 1 1 0
      |0 0 0 1 1 1 1 1
      |0 1 1 1 1 0 1 1
      |0 1 1 1 0 0 1 1
      |24
      |2 1 0 16
      |2 5 0 24
      |3 1 0 17
      |3 4 0 29
      |4 1 0 35
      |5 2 0 7
      |5 5 0 8
      |6 3 0 16
      |7 1 0 21
      |7 6 0 5
      |8 1 0 6
      |8 6 0 3
      |1 2 1 23
      |1 3 1 30
      |1 6 1 27
      |1 7 1 12
      |1 8 1 16
      |2 5 1 17
      |3 4 1 15
      |4 7 1 12
      |5 5 1 7
      |5 8 1 7
      |6 2 1 11
      |6 3 1 10
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    val startTime = System.nanoTime()
    (1 to testCount).foreach { testNo =>
      val boardSize = source.next().toInt
      val board = Board((0 until boardSize).map(_ => source.next().split(" ").toSeq.map(_.toInt)))
      val hintSize = source.next().toInt
      val hintSeq = (0 until hintSize).map { _ => val line = source.next().split(" ").map(_.toInt); Hint(line(0), line(1), line(2), line(3)) }

      println(s"-- testCase $testNo --")
      println(s"Board: ")
      println(board)
      println(s"Hit List: ")
      hintSeq.foreach(println)
      //println(solve2(friends, foods))
    }
    println(s"Spent Time: ${(System.nanoTime() - startTime) / 1000.0}")
  }

  def maskSize(mask: Int): Int = Integer.bitCount(mask)

  def maskSum(mask: Int): Int = {
    (1 to 9).map { i =>
      if (((1 << i) & mask) != 0) i else 0
    }.sum
  }

  def candidates(len: Int, sum: Int, known: Int): Int = {
    (0 until 1024 by 2).foldLeft(0) { (allSets, set) =>
      if ((set & known) == known && maskSize(set) == len && maskSum(set) == sum)
        allSets | set
      else
        allSets
    } & ~known
  }


  sealed trait Direction

  case class Board(board: Seq[Seq[Int]]) extends Seq[Seq[Int]] {
    override def toString: String = {
      val builder = new StringBuilder
      board.foreach(row => builder.append(row.mkString(" ") + "\n"))
      builder.toString
    }

    def length: Int = board.length

    def apply(idx: Int): Seq[Int] = board(idx)

    def iterator: Iterator[Seq[Int]] = board.iterator
  }

  case class Hint(y: Int, x: Int, direction: Direction, sum: Int)

  object Direction {

    import scala.language.implicitConversions

    object Horizontal extends Direction {
      override def toString: String = "Horizontal"
    }

    object Vertical extends Direction {
      override def toString: String = "Vertical"
    }

    implicit def int2direction(in: Int): Direction = {
      in match {
        case 0 => Horizontal
        case 1 => Vertical
      }
    }
  }

}

