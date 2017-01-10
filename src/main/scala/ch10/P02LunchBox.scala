package ch10

object P02LunchBox {

	import scala.io._

	val in: String =
		"""2
			|3
			|2 2 2
			|2 2 2
			|3
			|1 2 3
			|1 2 1
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val _ = source.next().split(" ").map(_.toInt)
			val warmTimeColl = source.next().split(" ").map(_.toInt).toVector
			val eatTimeColl = source.next().split(" ").map(_.toInt).toVector

			println(s"-- testCase $testNo --")
			//println(s"WarmTimes: ${warmTimeColl}")
			//println(s"EatTimes: ${eatTimeColl}")
			println(solve(warmTimeColl, eatTimeColl))
		}
	}

	def solve(warmTimeColl: Seq[Int], eatTimeColl: Seq[Int]): String = {
		val sortedColl = warmTimeColl.zip(eatTimeColl).sortBy(-_._2)
		val result = sortedColl.scanLeft((0, 0)) { case (acc, (warmTime, eatTime)) =>
			(acc._1 + warmTime, eatTime)
		}.maxBy { case (warmTimeAcc, eatTime) =>
			warmTimeAcc + eatTime
		}
		(result._1 + result._2).toString
	}

}

