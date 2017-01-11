package ch09

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object P10BlockGame {

	import scala.io._

	type Board = Seq[Seq[Boolean]]
	val in: String =
		"""3
			|.....
			|.##..
			|##..#
			|#.###
			|..#..
			|.....
			|.....
			|.....
			|.....
			|.....
			|#..##
			|##.##
			|##.##
			|#...#
			|##.##
			|""".stripMargin
	val RowCnt = 5
	val ColCnt = 5
	val moveColl: Seq[Int] = {
		val buffer = new ListBuffer[Int]
		for (y <- 0 until RowCnt; x <- 0 until ColCnt - 1) {
			buffer += (cell(y, x) + cell(y, x + 1))
			buffer += (cell(x, y) + cell(x + 1, y))
		}
		for (y <- 0 until RowCnt - 1; x <- 0 until ColCnt - 1) {
			val blockColl = for (dy <- 0 until 2; dx <- 0 until 2) yield {
				cell(y + dy, x + dx)
			}
			val entire = blockColl.foldLeft(0) { (result, block) => result | block }
			for (block <- blockColl) {
				buffer += (entire - block)
			}
		}
		buffer.result()
	}
	val cache: mutable.HashMap[Int, Boolean] = mutable.HashMap[Int, Boolean]()

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val board = (0 until RowCnt).toVector.map(_ => source.next().toVector.map(ch => if (ch == '.') false else true))

			//println(s"-- testCase $testNo --")
			//board.foreach(println)
			//val encodedBoard = encodeBoard(board)
			//println(s"encoded board = ${encodedBoard}")
			//println(s"decoded board:")
			//val decodedBoard = decodeBoard(encodedBoard)
			//decodedBoard.foreach(println)
			//println(s"rotate decoded board:")
			//val rotatedEncodedBoard = rotate(encodedBoard)
			//val rotateBoard = decodeBoard(rotatedEncodedBoard)
			//rotateBoard.foreach(println)
			println(solve(board))
			//println(play(0))
		}
	}

	def rotate90(board: Board): Board = {
		for (y <- 0 until 5) yield
			for (x <- 0 until 5) yield
				board(board.size - 1 - x)(y)
	}

	def encodeBoard(board: Board): Int = {
		board.zipWithIndex.foldLeft(0) { case (rowResult, (row, rowIdx)) =>
			rowResult | row.zipWithIndex.foldLeft(0) { case (colResult, (value, colIdx)) =>
				colResult | (if (value) cell(rowIdx, colIdx) else 0)
			}
		}
		/*
		var result = 0
		for (y <- board.indices)
			for (x <- board(y).indices)
				if (board(y)(x)) result = result | (1 << (y * ColCnt + x))
		result
		*/
	}

	def decodeBoard(encoded: Int): Board = {
		val buffer = ArrayBuffer.fill(RowCnt, ColCnt)(false)
		for (y <- buffer.indices; x <- buffer(y).indices)
			if ((encoded & (1 << (y * RowCnt + x))) != 0) buffer(y)(x) = true
		buffer
	}

	def rotate(encodedBoard: Int): Int = {
		var result = 0
		for (y <- 0 until 5) yield
			for (x <- 0 until 5) yield
				if ((encodedBoard & cell(RowCnt - 1 - x, y)) != 0) {
					result |= cell(y, x)
				}
		result
	}

	//println(moveColl.size)

	//println(moveColl.size)

	/*
	moveColl.foreach { move =>
		decodeBoard(move).foreach { row =>
			row.foreach { cell => if (cell) print('#') else print('.') }
			println
		}
		println
	}
	*/

	@inline
	def cell(y: Int, x: Int): Int = 1 << (y * ColCnt + x)

	@inline
	def canMove(encodedBoard: Int, move: Int): Boolean = (encodedBoard & move) == 0

	def play: (Int) => Boolean = {
		case b if cache.get(b).nonEmpty => cache(b)
		case b =>
			val possibleMoveColl = moveColl.filter(canMove(b, _)).toStream
			val retColl = possibleMoveColl.map { move => play(b | move) }.dropWhile(identity)
			cache(b) = if (retColl.isEmpty) false else true
			val r90 = rotate(b)
			val r180 = rotate(r90)
			val r270 = rotate(r180)
			cache(r90) = cache(b)
			cache(r180) = cache(b)
			cache(r270) = cache(b)
			cache(b)
	}

	def solve(board: Board): String = {
		val encodedBoard = encodeBoard(board)


		/*
		val cache = Array.fill[Option[Boolean]](1 << 25)(None)

		def play(board: Int): Boolean = {
			val cachedValue = cache(board)
			if (cachedValue.nonEmpty) return cachedValue.get
			val possibleMoveColl = moveColl.filter(canMove(board, _)).toStream
			val retColl = possibleMoveColl.map { move => play(board | move) }.dropWhile(identity)
			cache(board) = Some(if (retColl.isEmpty) false else true)
			cache(board).get
		}
		*/

		if (play(encodedBoard)) {
			"WINNING"
		} else {
			"LOSING"
		}
	}
}

