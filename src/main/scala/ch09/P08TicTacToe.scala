package ch09

object P08TicTacToe {

  import com.util.memoize

  import scala.io._

  val in: String =
    """3
      |...
      |...
      |...
      |xx.
      |oo.
      |...
      |xox
      |oo.
      |x.x
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    (1 to testCount).foreach { testNo =>
      val board = (0 until 3).toVector.map(_ => source.next().toVector)

      //println(s"-- testCase $testNo --")
      //println(s"board: ")
      //board.foreach(println)
      println(solve(board))
    }
  }

  def solve(board: Seq[Seq[Char]]): String = {
    lazy val canWin: (Seq[Seq[Char]], Char) => Int = Function.untupled(memoize {
      case (b, t) if isFinished(b, switchTurn(t)) => -1
      case (b, t) =>
        val valueColl = getPossiblePos(b).map { pos =>
          val newBoard = replace(b, pos, t)
          val value = canWin(newBoard, switchTurn(t))
          value
        }
        val minValue = (valueColl :+ 2).min
        if (minValue == 2 || minValue == 0) 0
        else -minValue
    })

    val turn = getNextTurn(board)
    //println(s"next turn: $turn")
    (canWin(board, turn) match {
      case 1 => turn
      case 0 => "TIE"
      case _ => switchTurn(turn)
    }).toString
  }

  def isFinished(board: Seq[Seq[Char]], turn: Char): Boolean = {
    (board ++ board.transpose ++ getDiagonals(board)).exists(_.forall(_ == turn))
  }

  def getDiagonals(board: Seq[Seq[Char]]): Seq[Seq[Char]] = {
    board match {
      case Seq(
      Seq(a1, _, c1),
      Seq(_, b2, _),
      Seq(a3, _, c3)) => Seq(Seq(a1, b2, c3), Seq(c1, b2, a3))
    }
  }

  def getNextTurn(board: Seq[Seq[Char]]): Char = {
    val coll = board.flatten
    if (coll.count(_ == 'x') > coll.count(_ == 'o')) 'o'
    else 'x'
  }

  def switchTurn(turn: Char): Char = ('o' + 'x' - turn).toChar

  def getPossiblePos(board: Seq[Seq[Char]]): Seq[(Int, Int)] = {
    for {
      y <- board.indices
      x <- board(y).indices if board(y)(x) == '.'
    } yield (y, x)
  }

  def replace(board: Seq[Seq[Char]], pos: (Int, Int), ch: Char): Seq[Seq[Char]] = {
    board.zipWithIndex.map { case (row, y) =>
      if (y == pos._1) {
        row.zipWithIndex.map {
          case (_, x) if x == pos._2 => ch
          case (c, _) => c
        }
      } else {
        row
      }
    }
  }

}

