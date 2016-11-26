package ch6

import scala.annotation.tailrec

/**
	* Created by ipoemi on 2016-11-24.
	*/
object Code6_3 {
	val delta = Vector((-1, -1), (-1, 0), (-1, 1), (1, -1), (1, 0), (1, 1), (0, -1), (0, 1))

	val board: Vector[Vector[Char]] = Vector(
		Vector('N', 'N', 'N', 'N', 'S'),
		Vector('N', 'E', 'E', 'E', 'N'),
		Vector('N', 'E', 'Y', 'E', 'N'),
		Vector('N', 'E', 'E', 'E', 'N'),
		Vector('N', 'N', 'N', 'N', 'S')
	).reverse

	def inRange(x: Int, y: Int): Boolean = {
		if (y < 0 || y >= board.size) return false
		if (x < 0 || x >= board(0).size) return false
		return true
	}

	def hasWord(x: Int, y: Int, word: String): Boolean = {
		if (!inRange(x, y)) return false
		if (board(y)(x) != word(0)) return false
		if (word.length == 1) return true

		delta.foreach { case (dx, dy) =>
			val nextX = x + dx
			val nextY = y + dy
			if (hasWord(nextX, nextY, word.substring(1)))
				return true
		}
		return false
	}

	def main(args: Array[String]): Unit = {
		println(hasWord(2, 2, "YES"))
	}
}
