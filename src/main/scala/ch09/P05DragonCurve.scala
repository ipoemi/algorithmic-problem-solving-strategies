package ch09

import scala.annotation.tailrec

object P05DragonCurve {

	import com.util.memoize

	import scala.io._

	lazy val count: (Int) => (Int) = memoize {
		case 0 => 1
		case n1 => MaxValue.min(count(n1 - 1) * 2 + 2)
	}
	val in: String =
		"""4
			|0 1 2
			|1 1 5
			|2 6 5
			|42 764853475 30
			|""".stripMargin
	val MaxValue: Int = 1000000000 + 1

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val Array(n, p, l) = source.next().split(" ").map(_.toInt)

			//println(s"-- testCase $testNo --")
			//println(s"n = $n, p = $p, l = $l")
			println(solve(n, p, l))

		}
	}

	def solve(n: Int, p: Int, l: Int): String = {
		@tailrec
		def kthAux(str: String, level: Int, skip: Int): String = {
			if (level == 0) return str(skip).toString
			//println(s"head = ${str.head}, count($level) = ${count(level)}, skip = $skip")
			str.head match {
				case ch if ch == 'X' || ch == 'Y' =>
					if (skip < count(level)) {
						if (ch == 'X') {
							kthAux("X+YF", level - 1, skip)
						} else {
							kthAux("FX-Y", level - 1, skip)
						}
					} else {
						kthAux(str.tail.mkString, level, skip - count(level))
					}
				case hd if skip == 0 => hd.toString
				case _ => kthAux(str.tail.mkString, level, skip - 1)
			}
		}

		(0 until l).map(lN => kthAux("FX", n, p - 1 + lN)).mkString

	}

	def curve(seed: String, level: Int): String = {
		if (level == 0) return seed
		seed.flatMap {
			case 'X' => curve("X+YF", level - 1)
			case 'Y' => curve("FX-Y", level - 1)
			case other => other.toString
		}.mkString
	}

}

