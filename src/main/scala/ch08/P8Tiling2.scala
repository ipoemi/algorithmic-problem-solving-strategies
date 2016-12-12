package ch08

/**
	* Created by ipoemi on 2016-12-05.
	*/
object P8Tiling2 {

	import scala.io._

	val in: String =
		"""3
			|1
			|5
			|100
			|""".stripMargin

	val Mod = 1000000007

	def solve(n: Int): Int = {
		val cache: Array[Option[Int]] = Array.fill(n + 1)(None)

		def aux(n: Int): Int = {
			if (n <= 1) return 1
			val cachedValue = cache(n)
			if (cachedValue.nonEmpty) return cachedValue.get
			cache(n) = Some((aux(n - 2) + aux(n - 1)) % Mod)
			cache(n).get
		}

		aux(n)
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val n = source.next().toInt

			println(s"-- testCase $testNo --")
			println(s"n: $n")
			println(solve(n))
		}
	}

}
