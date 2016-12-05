package ch08

import scala.collection.mutable

/**
	* Created by ipoemi on 2016-12-05.
	*/
object P1JumpGame {

	import scala.io._

	val in: String =
		"""2
			|7
			|2 5 1 6 1 4 1
			|6 1 1 2 2 9 3
			|7 2 3 2 1 3 1
			|1 1 3 1 7 1 2
			|4 1 2 3 4 1 2
			|3 3 1 2 3 4 1
			|1 5 2 9 4 7 0
			|7
			|2 5 1 6 1 4 1
			|6 1 1 2 2 9 3
			|7 2 3 2 1 3 1
			|1 1 3 1 7 1 2
			|4 1 2 3 4 1 3
			|3 3 1 2 3 4 1
			|1 5 2 9 4 7 0
			|""".stripMargin

	def solve(board: Seq[Seq[Int]], y: Int, x: Int, cache: mutable.HashMap[(Int, Int), Boolean] = mutable.HashMap()): Boolean = {
		if (y >= board.size || x >= board(y).size) return false
		if (y == board.size - 1 && x == board.size - 1) return true
		cache.getOrElseUpdate((y, x), {
			val jumpSize = board(y)(x)
			solve(board, y + jumpSize, x, cache) || solve(board, y, x + jumpSize, cache)
		})
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next.toInt
		(1 to testCount).foreach {
			testNo =>
				val boardSize = source.next().toInt
				val board = (0 until boardSize).map(_ => source.next().split(" ").map(_.toInt).toVector)

				println(s"-- testCase $testNo --")
				//println(s"Board: ")
				//board.foreach { x => x.foreach(print); println() }
				println(if (solve(board, 0, 0)) "YES" else "NO")
		}
	}

}
