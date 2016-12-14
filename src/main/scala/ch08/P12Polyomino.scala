package ch08

import scala.collection.mutable

/**
	* Created by ipoemi on 2016-12-05.
	*/
object P12Polyomino {

	import scala.io._

	val in: String =
		"""3
			|2
			|4
			|92
			|""".stripMargin

	val Mod: Int = 10000000

	def solve(n: Int): Int = {

		val cache: mutable.HashMap[(Int, Int), Int] = mutable.HashMap()

		def aux(blocks: Int, first: Int): Int = {
			if (blocks == first) return 1
			cache.getOrElseUpdate((blocks, first), {
				val combs = for (n <- 1 to (blocks - first)) yield
					(first + n - 1) * aux(blocks - first, n)
				combs.fold(0)((n1, n2) => (n1 % Mod + n2 % Mod) % Mod) % Mod
			})
		}

		(1 to n).map(aux(n, _)).fold(0)((n1, n2) => (n1 % Mod + n2 % Mod) % Mod)
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val n = source.next().toInt

			println(s"-- testCase $testNo --")
			//println(s"N: $n")
			println(solve(n))
		}
	}

}
