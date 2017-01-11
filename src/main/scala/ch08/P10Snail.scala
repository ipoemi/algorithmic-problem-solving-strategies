package ch08

import scala.collection.mutable

object P10Snail {

	import scala.io._

	val in: String =
		"""4
			|5 4
			|5 3
			|4 2
			|3 2
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val Array(height, days) = source.next().split(" ").map(_.toInt)

			//println(s"-- testCase $testNo --")
			//println(s"Height: $height")
			//println(s"Days: $days")
			println(solve(days, height).formatted("%.7f"))
		}
	}

	def solve(days: Int, height: Int): Double = {
		val cache: mutable.HashMap[(Int, Int), Double] = mutable.HashMap()

		def aux(passedDays: Int, passedHeight: Int): Double = {
			if (passedDays == days) return if (passedHeight >= height) 1 else 0
			val cachedValue = cache.get((passedDays, passedHeight))
			if (cachedValue.nonEmpty) return cachedValue.get
			cache((passedDays, passedHeight)) =
					0.25 * aux(passedDays + 1, passedHeight + 1) + 0.75 * aux(passedDays + 1, passedHeight + 2)
			cache((passedDays, passedHeight))
		}

		aux(0, 0)
	}

}
