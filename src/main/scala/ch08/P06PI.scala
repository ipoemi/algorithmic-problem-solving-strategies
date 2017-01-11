package ch08

object P06PI {

	import scala.io._

	val in: String =
		"""5
			|12341234
			|11111222
			|12122222
			|22222222
			|12673939
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach {
			testNo =>
				val nums = source.next()

				//println(s"-- testCase $testNo --")
				println(s"Nums: $nums")
				println(solve(nums))
		}
	}

	def solve(nums: String): Int = {
		val MaxValue = Int.MaxValue / 2
		val cache = Array.fill[Option[Int]](nums.length)(None)

		def solveAux(start: Int): Int = {
			val cachedValue = cache(start)
			if (cachedValue.nonEmpty) return cachedValue.get
			if (nums.length - start < 3) {
				cache(start) = Some(MaxValue)
				return MaxValue
			}
			if (nums.length - start < 6) {
				val score = getScore(nums, start, nums.length - start)
				cache(start) = Some(score)
				return score
			}
			val v1 = getScore(nums, start, 3) + solveAux(start + 3)
			val v2 = getScore(nums, start, 4) + solveAux(start + 4)
			val v3 = getScore(nums, start, 5) + solveAux(start + 5)
			cache(start) = Some(v1.min(v2).min(v3))
			cache(start).get
		}

		solveAux(0)
	}

	def getScore(str: String, start: Int, cnt: Int): Int = {
		if (isAllSame(str, start, cnt)) 1
		else if (isOneArithmetical(str, start, cnt)) 2
		else if (isAlternately(str, start, cnt)) 4
		else if (isArithmetical(str, start, cnt)) 5
		else 10
	}

	def isAllSame(str: String, start: Int, cnt: Int): Boolean = {
		(1 until cnt).forall(n => str(start) == str(start + n))
	}

	def isOneArithmetical(str: String, start: Int, cnt: Int): Boolean = {
		val delta = str(start + 1) - str(start)
		delta.abs == 1 && isArithmetical(str, start, cnt)
	}

	def isArithmetical(str: String, start: Int, cnt: Int): Boolean = {
		val delta = str(start + 1) - str(start)
		(0 until cnt - 1).forall(n => str(start + n) + delta == str(start + n + 1))
	}

	def isAlternately(str: String, start: Int, cnt: Int): Boolean = {
		(2 until cnt).forall { n =>
			(n % 2 == 0 && str(start + n) == str(start)) ||
					(n % 2 == 1 && str(start + n) == str(start + 1))
		}
	}

}
