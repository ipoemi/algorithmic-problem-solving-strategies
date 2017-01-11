package ch10

import scala.annotation.tailrec

object P01MatchOver {

	import scala.io._
	import com.util._

	val in: String =
		"""3
			|6
			|3000 2700 2800 2200 2500 1900
			|2800 2750 2995 1800 2600 2000
			|3
			|1 2 3
			|3 2 1
			|4
			|2 3 4 5
			|1 2 3 4
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val _ = source.next().split(" ").map(_.toInt)
			val enemyPlayerRatingColl = source.next().split(" ").map(_.toInt).toVector
			val ourPlayerRatingColl = source.next().split(" ").map(_.toInt).toVector

			//println(s"-- testCase $testNo --")
			//println(s"Enemy: ${enemyPlayerRatingColl}")
			//println(s"Our: ${ourPlayerRatingColl}")
			println(solve(enemyPlayerRatingColl, ourPlayerRatingColl))
		}
	}

	def solve(enemyPlayerRatingColl: Vector[Int], ourPlayerRatingColl: Vector[Int]): String = {
		val sortedOurPlayerRatingColl = ourPlayerRatingColl.sorted

		@tailrec
		def aux(auxEnemyColl: Vector[Int], auxOurColl: Vector[Int], result: Int): Int = {
			//println(s"auxEnemyColl: $auxEnemyColl")
			//println(s"auxOurColl: $auxOurColl")
			if (auxEnemyColl.isEmpty) result
			else {
				val head = auxEnemyColl.head
				val tail = auxEnemyColl.tail
				if (head > auxOurColl.last) {
					aux(tail, auxOurColl.tail, result)
				} else {
					aux(tail, auxOurColl.diff(List(findMinUpper(auxOurColl, head).get)), result + 1)
				}
			}
		}

		aux(enemyPlayerRatingColl, sortedOurPlayerRatingColl, 0).toString
	}

}

