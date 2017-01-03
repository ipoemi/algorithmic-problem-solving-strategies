package ch09

object P09NumberGame {

	import com.util.memoize

	import scala.io._

	val in: String =
		"""3
			|5
			|-1000 -1000 -3 -1000 -1000
			|6
			|100 -1000 -1000 100 -1000 -1000
			|10
			|7 -5 8 5 1 -4 -8 6 7 9
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val _ = source.next()
			val numColl = source.next().split(" ").map(_.toInt).toVector

			println(s"-- testCase $testNo --")
			//println(s"numColl = $numColl")
			println(solve(numColl))
		}
	}

	def solve(numColl: Seq[Int]): String = {
		lazy val play: (Int, Int) => Int = Function.untupled(memoize {
			case (left, right) if left > right => 0
			case (left, right) =>
				val valueColl = {
					if (right - left + 1 >= 2)
						Vector(-play(left + 2, right), -play(left, right - 2))
					else
						Vector()
				} ++ Vector(numColl(left) - play(left + 1, right), numColl(right) - play(left, right - 1))
				valueColl.max
		})
		play(0, numColl.size - 1).toString
	}

}

