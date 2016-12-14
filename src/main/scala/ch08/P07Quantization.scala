package ch08

/**
 * Created by ipoemi on 2016-12-05.
 */

object P07Quantization {

	import scala.io._

	val in: String =
		"""2
			|10 3
			|3 3 3 1 2  3 2 2 2 1
			|9 3
			|1 744 755 4 897 902 890 6 777
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val Array(_, partCnt) = source.next().split(" ").map(_.toInt)
			val nums = source.next().split("\\s+").map(_.toInt).toVector

			println(s"-- testCase $testNo --")
			println(s"Parts: $partCnt")
			println(s"Nums: $nums")
			println(solve(nums, partCnt))
		}
	}

	def solve(nums: Seq[Int], partCnt: Int): Int = {
		val MaxValue = Int.MaxValue / 2
		val elems = nums.sorted
		//println(nums.sorted)
		val pSums = elems.scanLeft(0)(_ + _)
		val pSquareSums = elems.scanLeft(0)((x, y) => x + y * y)

		def minError(lo: Int, hi: Int): Int = {
			//println(s"hi: $hi, lo: $lo")
			val sum = pSums(hi) - (if (lo == 0) 0 else pSums(lo - 1))
			val squareSum = pSquareSums(hi) - (if (lo == 0) 0 else pSquareSums(lo - 1))

			val m = Math.round(sum.toDouble / (hi - lo + 1)).toInt
			val ret = squareSum - 2 * m * sum + m * m * (hi - lo + 1)
			//println(s"ret: $ret")
			ret
		}

		val cache: Array[Array[Option[Int]]] = Array.fill(nums.size + 1)(Array.fill(partCnt + 1)(None))

		def aux(from: Int, partSize: Int): Int = {
			if (from == elems.size) return 0
			if (partSize == 0) return MaxValue
			//println(s"from: $from, partSize: $partSize")
			val cachedValue = cache(from)(partSize)
			if (cachedValue.nonEmpty) return cachedValue.get
			val candidates =
				for (to <- (from + 1) until elems.size) yield {
					minError(from, to) + aux(to + 1, partSize - 1)
				}
			cache(from)(partSize) = Some((candidates :+ MaxValue).min)
			cache(from)(partSize).get
		}

		aux(0, partCnt)
	}

}
