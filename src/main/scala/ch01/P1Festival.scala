package ch01

import scala.annotation.tailrec

/**
	* Created by ipoemi on 2016-11-26.
	*/
object P1Festival {

	import scala.io._

	val in: String =
		"""2
			|6 3
			|1 2 3 1 2 3
			|6 2
			|1 2 3 1 2 3
			|""".stripMargin

	def printSelectedDays(selectedDays: Array[Int]): Unit = {
		for (d <- selectedDays) print(s"$d ")
		println()
	}

	@tailrec
	def solve(selectedDays: Array[Int], dayValues: Vector[Int], minDay: Int, curMin: Double): Double = {
		//printSelectedDays(selectedDays)
		val lastIdx = selectedDays.lastIndexWhere(_ != 0)
		val firstIdx = selectedDays.indexWhere(_ != 0)

		val avg = if (minDay <= selectedDays.sum)
			selectedDays.indices.map { idx =>
				if (selectedDays(idx) == 1) dayValues(idx)
				else 0
			}.sum / selectedDays.sum.toDouble
		else
			Double.MaxValue

		//println(s"avg: $avg")

		if (lastIdx == firstIdx && lastIdx == selectedDays.length - 1) return curMin.min(avg)

		if (lastIdx == selectedDays.length - 1) {
			(firstIdx to lastIdx).foreach(selectedDays(_) = 0)
			selectedDays(firstIdx + 1) = 1
		} else {
			selectedDays(lastIdx + 1) = 1
		}
		solve(selectedDays, dayValues, minDay, curMin.min(avg))

	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next.toInt
		(1 to testCount).foreach { testNo =>
			val Array(dayCnt, teamCnt) = source.next.split(" ").map(_.toInt)
			val dayValues = source.next.split(" ").map(_.toInt).toVector
			val selectedDays = new Array[Int](dayValues.size)


			println(s"-- testCase $testNo --")
			println(s"DayCnt: $dayCnt, TeamCnt: $teamCnt")
			println(s"DayValues: $dayValues")
			println(solve(selectedDays, dayValues, teamCnt, Double.MaxValue))
		}
	}
}
