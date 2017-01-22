package ch11

object P01BoardCover2 {

  import scala.io._

  case class Block(block: Seq[Seq[Int]]) extends Seq[Seq[Int]] {
    override def toString: String = {
      val builder = new StringBuilder
      block.foreach(row => builder.append(row.mkString + "\n"))
      builder.toString
    }

    def length: Int = block.length

    def apply(idx: Int): Seq[Int] = block(idx)

    def iterator: Iterator[Seq[Int]] = block.iterator

    @inline
    def updated(y: Int, x: Int, value: Int): Block = Block(block.updated(y, block(y).updated(x, value)))

    def canUpdate(positions: Seq[(Int, Int)]): Boolean = positions.forall { pos =>
      (pos._1 >= 0 && pos._1 < block.size) && (pos._2 >= 0 && pos._2 < block.head.size) && (block(pos._1)(pos._2) == 0)
    }

  }

  type Board = Block
  val Board = Block

  val in: String =
    """2
      |4 7 2 3
      |##.##..
      |#......
      |#....##
      |#..####
      |###
      |#..
      |5 10 3 3
      |..........
      |..........
      |..........
      |..........
      |..........
      |.#.
      |###
      |..#
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    //val startTime = System.currentTimeMillis()
    (1 to testCount).foreach { testNo =>
      val Array(boardHeight, _, blockHeight, _) = source.next().split(" ").map(_.toInt)
      val board = Board((0 until boardHeight).map(_ => source.next().map { ch => if (ch == '#') 1 else 0 }))
      val block = Block((0 until blockHeight).map(_ => source.next().map { ch => if (ch == '#') 1 else 0 }))

      //println(s"-- testCase $testNo --")
      //println(s"Board: ")
      //println(board)
      //println(s"Block: ")
      //println(block)
      println(solve(board, block))
    }
    //println(s"Spent Time: ${System.currentTimeMillis() - startTime}")
  }

  def rotate(block: Block): Block = {
    Block(
      for (y <- block.head.indices) yield
        for (x <- block.indices) yield
          block(block.size - 1 - x)(y))
  }

  def blockToPositions(block: Block): Seq[(Int, Int)] = {
    val builder = Vector.newBuilder[(Int, Int)]
    for (y <- block.indices)
      for (x <- block(y).indices)
        if (block(y)(x) != 0) builder += ((y, x))
    val prePosColl = builder.result()
    val head = prePosColl.head
    prePosColl.map { case (y, x) => (y - head._1, x - head._2) }
  }

  def createBlocks(block: Block): Seq[Block] = {
    val resultSet = Set.newBuilder[Block]
    resultSet += block
    resultSet += rotate(block)
    resultSet += rotate(rotate(block))
    resultSet += rotate(rotate(rotate(block)))
    resultSet.result().toSeq
  }

  def indexOfOpen(board: Board): Option[(Int, Int)] = {
    for (y <- board.indices)
      for (x <- board(y).indices)
        if (board(y)(x) == 0) return Some((y, x))
    None
  }

  def indexOfOpen(board: Seq[Array[Int]]): Option[(Int, Int)] = {
    for (y <- board.indices)
      for (x <- board(y).indices)
        if (board(y)(x) == 0) return Some((y, x))
    None
  }

  def solve(board: Board, block: Block): String = {
    val uniqueBlockPositions = createBlocks(block).map(blockToPositions)
    //println(uniqueBlockPositions)

    //var best = 0

    //val boardArray = board.map(_.toArray)

    //def set(aBoard: Seq[Array[Int]], positions: Seq[(Int, Int)], value: Int): Unit = {
    //  positions.foreach { pos => aBoard(pos._1)(pos._2) += value }
    //}

    //def canUpdate(aBoard: Seq[Array[Int]], positions: Seq[(Int, Int)]): Boolean = positions.forall { pos =>
    //  (pos._1 >= 0 && pos._1 < aBoard.size) && (pos._2 >= 0 && pos._2 < aBoard.head.length) && (aBoard(pos._1)(pos._2) == 0)
    //}

    val blockSize = block.flatten.count(_ != 0)

    def blanks(aBoard: Board): Int = {
      aBoard.flatten.count(_ == 0)
    }

    /*
    def aux(placed: Int, blankCnt: Int): Unit = {
      //boardArray.foreach(row => println(row.mkString))
      //println
      indexOfOpen(boardArray) match {
        case Some(startPos) if placed + blankCnt / blockSize > best =>
          for (positions <- uniqueBlockPositions) {
            val newPositions = positions.map(pos => (pos._1 + startPos._1, pos._2 + startPos._2))
            if (canUpdate(boardArray, newPositions)) {
              set(boardArray, newPositions, 1)
              aux(placed + 1, blankCnt - blockSize)
              set(boardArray, newPositions, -1)
            }
          }
          boardArray(startPos._1)(startPos._2) = 1
          aux(placed, blankCnt - 1)
          boardArray(startPos._1)(startPos._2) = 0
        case _ => best = best.max(placed)
      }
    }
    */

    def aux(auxBoard: Board, blankCnt: Int, placedCnt: Int, auxBest: Int): Int = {
      indexOfOpen(auxBoard) match {
        case Some(startPos) =>
          val res1 = uniqueBlockPositions.foldLeft(auxBest) { (acc, blockPosColl) =>
            if ((blankCnt - blockSize) / blockSize + placedCnt + 1 > acc) {
              val newBlockPosColl = blockPosColl.map(pos => (pos._1 + startPos._1, pos._2 + startPos._2))
              if (auxBoard.canUpdate(newBlockPosColl)) {
                val newBoard = newBlockPosColl.foldLeft(auxBoard) { (boardAcc, pos) =>
                  boardAcc.updated(pos._1, pos._2, boardAcc(pos._1)(pos._2) + 1)
                }
                aux(newBoard, blankCnt - blockSize, placedCnt + 1, auxBest).max(acc)
              }
              else acc
            } else {
              acc
            }
          }
          if ((blankCnt - 1) / blockSize + placedCnt > res1) {
            val newBoard = auxBoard.updated(startPos._1, startPos._2, auxBoard(startPos._1)(startPos._2) + 1)
            res1.max(aux(newBoard, blankCnt - 1, placedCnt, res1))
          }
          else res1
        /*
        val res1 = for {
          blockPosColl <- uniqueBlockPositions
          newBlockPosColl = blockPosColl.map(pos => (pos._1 + startPos._1, pos._2 + startPos._2))
          if auxBoard.canUpdate(newBlockPosColl)
        } yield {
          val newBoard = newBlockPosColl.foldLeft(auxBoard) { (boardAcc, pos) =>
            boardAcc.updated(pos._1, pos._2, boardAcc(pos._1)(pos._2) + 1)
          }
          aux(newBoard, result + 1)
        }
        val newBoard = auxBoard.updated(startPos._1, startPos._2, auxBoard(startPos._1)(startPos._2) + 1)
        (res1 :+ aux(newBoard, result)).max
        */
        case _ => placedCnt.max(auxBest)
      }
    }

    aux(board, blanks(board), 0, 0).toString
    //aux(0, blanks(board))
    //best.toString
  }

}

