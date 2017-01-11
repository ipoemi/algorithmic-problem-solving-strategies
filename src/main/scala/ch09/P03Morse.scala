package ch09

import com.util.memoize

object P03Morse {

	import scala.io._

	lazy val bino: (Int, Int) => Int = Function.untupled(memoize {
		case (_, 0) => 1
		case (n, r) if n == r => 1
		case (n, r) => M.min(bino(n - 1, r - 1) + bino(n - 1, r))
	})
	val in: String =
		"""3
			|2 2 4
			|4 8 13
			|6 4 1
			|""".stripMargin
	val NMLimit: Int = 100
	val KLimit: Int = 1000000000
	//val bino = Array.fill(NMLimit * NMLimit + 1, NMLimit * NMLimit + 1)(0)
	val M: Int = KLimit + NMLimit

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val Array(n, m, k) = source.next().split(" ").map(_.toInt)

			//println(s"-- testCase $testNo --")
			//println(solve(n, m, k))
			//println(solve2(n, m, k))
			println(solve3(n, m, k))
		}
	}

	def solve(n: Int, m: Int, k: Int): String = {
		val kthElem = (List.fill(n)(1) ++ List.fill(m)(2)).permutations.drop(k - 1).next()
		kthElem.map(n => if (n == 1) "_" else "o").mkString("")
	}

	def solve2(n: Int, m: Int, k: Int): String = {
		var skip = k - 1
		var result = ""

		def generate3(n: Int, m: Int, s: String): Unit = {
			if (skip < 0) return
			if (n == 0 && m == 0) {
				if (skip == 0) result = s
				skip -= 1
				return
			}
			if (bino(n + m, n) <= skip) {
				skip -= bino(n + m, n)
				return
			}
			if (n > 0) generate3(n - 1, m, s + "_")
			if (m > 0) generate3(n, m - 1, s + "o")
		}

		generate3(n, m, "")
		result
	}

	def solve3(n: Int, m: Int, k: Int): String = {
		if (n == 0) return "o" * m
		if (k <= bino(n + m - 1, n - 1)) return "-" + solve3(n - 1, m, k)
		"o" + solve3(n, m - 1, k - bino(n + m - 1, n - 1))
	}
}
