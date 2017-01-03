package ch06

object P01Boggle {

	import scala.io._

	val in: String =
		"""1
			|URLPM
			|XPRET
			|GIAET
			|XTNZY
			|XOQRS
			|6
			|PRETTY
			|GIRL
			|REPEAT
			|KARA
			|PANDORA
			|GIAZAPX
			|""".stripMargin

	val delta = Vector((-1, -1), (-1, 0), (-1, 1), (1, -1), (1, 0), (1, 1), (0, -1), (0, 1))

	def inRange(board: Vector[Vector[Char]], x: Int, y: Int): Boolean = {
		if (y < 0 || y >= board.size) return false
		if (x < 0 || x >= board(0).size) return false
		true
	}

	def hasWord(board: Vector[Vector[Char]], x: Int, y: Int, word: String): Boolean = {
		if (!inRange(board, x, y)) return false
		if (board(y)(x) != word(0)) return false
		if (word.length == 1) return true

		delta.foreach { case (dx, dy) =>
			val nextX = x + dx
			val nextY = y + dy
			if (hasWord(board, nextX, nextY, word.substring(1)))
				return true
		}
		false
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next.toInt
		(1 to testCount).foreach { testNo =>
			val headRow: Vector[Char] = source.next().toVector
			val tailRow: Vector[Vector[Char]] = (1 until headRow.size).map(_ => source.next().toVector).toVector
			val board = headRow +: tailRow
			val wordCount = source.next().toInt
			val words = (0 until wordCount).map(_ => source.next())

			println(s"-- testCase $testNo --")
			println(s"board: ")
			println(board)
			println(s"wordCount: $wordCount")
			println(s"words:")
			println(words)

			words.foreach { word =>
				var found = false
				for (y <- board.indices if !found)
					for (x <- board(y).indices if !found) {
						if (hasWord(board, x, y, word)) {
							found = true
							println(s"$word YES")
						}
					}
				if (!found) println(s"$word NO")
			}
		}
	}
}
