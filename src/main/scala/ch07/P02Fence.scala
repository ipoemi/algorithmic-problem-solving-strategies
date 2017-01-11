package ch07

object P02Fence {

	import scala.io._

	val in: String =
		"""3
			|7
			|7 1 5 9 6 7 3
			|7
			|1 4 4 4 4 1 1
			|4
			|1 8 2 2
			|""".stripMargin

	def solve(fenceHeights: Seq[Int], leftIdx: Int, rightIdx: Int): Int = {
		if (leftIdx == rightIdx) return fenceHeights(leftIdx)
		val mid = (leftIdx + rightIdx) / 2
		val left = solve(fenceHeights, leftIdx, mid)
		val right = solve(fenceHeights, mid + 1, rightIdx)

		def maxValue(lo: Int, hi: Int, height: Int = Int.MaxValue / 2, curValue: Int = 0): Int = {
			if (lo < leftIdx || hi > rightIdx) return curValue
			val minHeight = fenceHeights(lo).min(fenceHeights(hi)).min(height)
			val area = minHeight * (hi - lo + 1)
			//println(s"area($lo, $hi): $area")

			if (lo == leftIdx) maxValue(lo, hi + 1, minHeight, curValue.max(area))
			else if (hi < rightIdx && fenceHeights(lo) < fenceHeights(hi)) maxValue(lo, hi + 1, minHeight, curValue.max(area))
			else maxValue(lo - 1, hi, minHeight, curValue.max(area))
		}

		maxValue(mid, mid + 1).max(left).max(right)

		/*
		var lo = mid
		var hi = mid + 1
		var height = fenceHeights(lo).min(fenceHeights(hi))
		var ret = left.max(right).max(height * 2)
		println(s"area($lo, $hi): ${height * 2}")
		while (leftIdx < lo || hi < rightIdx) {
			if (hi < rightIdx && (lo == leftIdx || fenceHeights(lo - 1) < fenceHeights(hi + 1))) {
				hi += 1
				height = height.min(fenceHeights(hi))
			} else {
				lo -= 1
				height = height.min(fenceHeights(lo))
			}
			println(s"area($lo, $hi): ${height * (hi - lo + 1)}")
			ret = ret.max(height * (hi - lo + 1))
		}
		ret
		*/
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next.toInt
		(1 to testCount).foreach {
			testNo =>
				val _ = source.next().toInt
				val fenceHeights = source.next().split(" ").map(_.toInt)

				//println(s"-- testCase $testNo --")
				println(s"Fence Height: ${fenceHeights.toVector}")
				println(solve(fenceHeights, 0, fenceHeights.length - 1))
		}
	}
}
