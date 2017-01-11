package ch09

object P06Zimbabwe {

	import com.util.memoize

	import scala.io._

	val in: String =
		"""4
			|321 3
			|123 3
			|422 2
			|12738173912 7
			|""".stripMargin

	val Mod: Int = 1000000000 + 7

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val Array(price, m) = source.next().split(" ")

			//println(s"-- testCase $testNo --")
			//println(s"price = $price, m = $m")
			println(solve(price, m.toInt))

		}
	}

	def solve(price: String, m: Int): String = {
		val priceColl = price.toSeq
		val digitColl = price.toSeq.sorted

		lazy val priceAux: (Int, Int, Int, Boolean) => (Int) = Function.untupled(memoize {
			case (idx, _, mod, less) if idx == digitColl.size => if (mod == 0 && less) 1 else 0
			case (idx, taken, mod, less) =>
				digitColl.indices.filter { i =>
					(taken & (1 << i)) == 0
				}.filter { i =>
					less || priceColl(idx) >= digitColl(i)
				}.filter { i =>
					i == 0 || digitColl(i - 1) != digitColl(i) || (taken & (1 << (i - 1))) != 0
				}.map { i =>
					val nextTaken = taken | (1 << i)
					val nextMod = (mod * 10 + digitColl(i).toInt) % m
					val nextLess = less || priceColl(idx) > digitColl(i)
					priceAux(idx + 1, nextTaken, nextMod, nextLess)
				}.foldLeft(0) { (acc, value) =>
					(acc + value) % Mod
				} % Mod
		})
		priceAux(0, 0, 0, false).toString
	}

}

