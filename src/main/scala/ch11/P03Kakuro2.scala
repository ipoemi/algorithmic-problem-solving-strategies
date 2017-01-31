package ch11

object P03Kakuro2 {

  import scala.io._
  import com.util.memoize

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

  val MaxLength = 10
  val MaxSum = 45
  val MaxSubset = 1024

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    //val startTime = System.nanoTime()
    (1 to testCount).foreach { testNo =>
      val boardSize = source.next().toInt
      val board = Board((0 until boardSize).map(_ => source.next().split(" ").toSeq.map(_.toInt)))
      val hintSize = source.next().toInt
      val hints = (0 until hintSize).map { _ =>
        val line = source.next().split(" ").map(_.toInt)
        val hint = Hint(y = line(0) - 1, x = line(1) - 1, direction = line(2), sum = line(3), known = 0, length = 0)
        val len = if (hint.direction == Direction.Horizontal) {
          board(hint.y).slice(hint.x + 1, board(hint.y).size).takeWhile(_ == 1).size
        } else {
          val seq = board.map(_ (hint.x))
          seq.slice(hint.y + 1, seq.size).takeWhile(_ == 1).size
        }
        hint.copy(length = len)
      }

      //println(s"-- testCase $testNo --")
      //println(s"Board: ")
      //println(board)
      //println(s"Hit List: ")
      //hints.foreach(println)
      println(solve(board, hints))
    }
    //println(s"Spent Time: ${(System.nanoTime() - startTime) / 1000.0}")
  }

  @inline
  def maskSize(mask: Int): Int = Integer.bitCount(mask)

  def maskSum(mask: Int): Int = {
    (1 to 9).map { i =>
      if (((1 << i) & mask) != 0) i else 0
    }.sum
  }

  /*
  val candidates: (Int, Int, Int) => Int = Function.untupled(memoize {
    case (len, sum, known) =>
      (0 until MaxSubset by 2).foldLeft(0) { (allSets, set) =>
        if ((set & known) == known && maskSize(set) == len && maskSum(set) == sum)
          allSets | set
        else
          allSets
      } & ~known
  })
  */

  def solve(colorBoard: Board[Int], hints: Seq[Hint]): String = {
    val auxCandidates: Seq[Seq[Array[Int]]] = Seq.fill(MaxLength + 1)(Seq.fill(MaxSum + 1)(Array.fill(MaxSubset)(0)))

    (0 until 1024 by 2).foreach { set =>
      val l = maskSize(set)
      val s = maskSum(set)
      var subset = set
      auxCandidates(l)(s)(subset) |= (set & ~subset)
      while (subset != 0) {
        subset = (subset - 1) & set
        auxCandidates(l)(s)(subset) |= (set & ~subset)
      }
    }

    def candidates(len: Int, sum: Int, known: Int): Int = {
      auxCandidates(len)(sum)(known)
    }

    //val candidates: Seq[Seq[Seq[Int]]]
    val indexedHints = hints.zipWithIndex
    val hHints = indexedHints.filter(_._1.direction == Direction.Horizontal)
    val vHints = indexedHints.filter(_._1.direction == Direction.Vertical)
    val hintBoard =
      Board(
        for (y <- colorBoard.indices) yield
          for (x <- colorBoard(y).indices) yield
            if (colorBoard(y)(x) == 1)
              Some((hHints.filter(_._1.y == y).filter(_._1.x < x).maxBy(_._1.x)._2,
                  vHints.filter(_._1.x == x).filter(_._1.y < y).maxBy(_._1.y)._2))
            else
              None)
    val valueBoard = Board(Seq.fill(colorBoard.size, colorBoard.size)(0))

    def aux(axValueBoard: Board[Int], axHints: Seq[Hint]): String = {
      def put(y: Int, x: Int, value: Int): (Board[Int], Seq[Hint]) = {
        val newValueBoard = axValueBoard.updated(y, x, value)
        val hIdx = hintBoard(y)(x).get._1
        val vIdx = hintBoard(y)(x).get._2
        val tmpHints = axHints.updated(hIdx, axHints(hIdx).copy(known = axHints(hIdx).known + (1 << value)))
        val newHints = tmpHints.updated(vIdx, axHints(vIdx).copy(known = axHints(vIdx).known + (1 << value)))
        (newValueBoard, newHints)
      }

      def candCoord(y: Int, x: Int): Int = {
        val hHint = axHints(hintBoard(y)(x).get._1)
        val vHint = axHints(hintBoard(y)(x).get._2)
        candidates(hHint.length, hHint.sum, hHint.known) & candidates(vHint.length, vHint.sum, vHint.known)
      }

      val cands = for {
        y <- colorBoard.indices
        x <- colorBoard(y).indices
        if colorBoard(y)(x) == 1 && axValueBoard(y)(x) == 0
        cand = candCoord(y, x)
      } yield (cand, y, x)

      if (cands.isEmpty) axValueBoard.toString
      else {
        val minCands = cands.minBy(v => maskSize(v._1) == 0)
        if (minCands._1 == 0) ""
        else {
          val cc = (1 to 9).filter(v => (minCands._1 & (1 << v)) == (1 << v))
          cc.foldLeft("") { (acc, v) =>
            if (acc != "") acc
            else {
              val (newValueBoard, newHints) = put(minCands._2, minCands._3, v)
              aux(newValueBoard, newHints)
            }
          }
        }
      }
    }

    aux(valueBoard, hints)
  }


  case class Board[+T](board: Seq[Seq[T]]) extends Seq[Seq[T]] {
    override def toString: String = {
      val builder = new StringBuilder
      board.foreach(row => builder.append(row.mkString(" ") + "\n"))
      builder.toString
    }

    def length: Int = board.length

    def apply(idx: Int): Seq[T] = board(idx)

    def iterator: Iterator[Seq[T]] = board.iterator

    @inline
    def updated[B >: T](y: Int, x: Int, value: B): Board[B] = Board(board.updated(y, board(y).updated(x, value)))
  }

  case class Hint(y: Int, x: Int, direction: Direction, sum: Int, known: Int, length: Int)

  sealed trait Direction

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

