package ch06

import scala.annotation.tailrec

/**
	* Created by ipoemi on 2016-11-26.
	*/
object P4ClockSync {

	import scala.io._

	implicit class IntAddedTimes(n: Int) {
		def times(block: => Unit): Unit = {
			(1 to n).foreach(_ => block)
		}
	}

	val MaxCnt: Int = Int.MaxValue / 2

	val switches = Vector(
		Vector(0, 1, 2),
		Vector(3, 7, 9, 11),
		Vector(4, 10, 14, 15),
		Vector(0, 4, 5, 6, 7),
		Vector(6, 7, 8, 10, 12),
		Vector(0, 2, 14, 15),
		Vector(3, 14, 15),
		Vector(4, 5, 7, 14, 15),
		Vector(1, 2, 3, 4, 5),
		Vector(3, 4, 5, 9, 13)
	)

	val in: String =
		"""2
			|12 6 6 6 6 6 12 12 12 12 12 12 12 12 12 12
			|12 9 3 12 6 6 9 3 12 9 12 9 12 12 6 6
			|""".stripMargin

	def applySwitch(switchType: Int, watches: Array[Int], delta: Int): Unit = {
		switches(switchType).foreach { idx =>
			watches(idx) = Math.floorMod(watches(idx) + delta, 4)
		}
	}

	def solve(watches: Array[Int], cur: Int = 0): Int = {
		if (cur == switches.length) {
			return if (watches.forall(_ == 0)) 0 else MaxCnt
		}

		(0 until 4).map { n =>
			applySwitch(cur, watches, n)
			val value = n + solve(watches, cur + 1)
			applySwitch(cur, watches, -n)
			value
		}.min
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next.toInt
		(1 to testCount).foreach {
			testNo =>
				val watches = source.next.split(" ").map(n => Math.floorMod(n.toInt / 3, 4))

				println(s"-- testCase $testNo --")
				println(solve(watches))
		}
	}
}
