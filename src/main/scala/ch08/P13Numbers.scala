package ch08

import scala.collection.mutable

object P13Numbers {

	import scala.io._

	val in: String =
		"""2
			|5 2 0
			|0 1 1 1 0
			|1 0 0 0 1
			|1 0 0 0 0
			|1 0 0 0 0
			|0 1 0 0 0
			|3
			|0 2 4
			|8 2 3
			|0 1 1 1 0 0 0 0
			|1 0 0 1 0 0 0 0
			|1 0 0 1 0 0 0 0
			|1 1 1 0 1 1 0 0
			|0 0 0 1 0 0 1 1
			|0 0 0 1 0 0 0 1
			|0 0 0 0 1 0 0 0
			|0 0 0 0 1 1 0 0
			|4
			|3 1 2 6
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val Array(villageCnt, dayCnt, startNo) = source.next().split(" ").map(_.toInt)
			val matrix = (0 until villageCnt).map(_ => source.next().split(" ").map(_.toInt).toVector).toVector
			val _ = source.next()
			val targetNoList = source.next().split(" ").map(_.toInt).toVector

			//println(s"-- testCase $testNo --")
			//println(s"VillageCnt: $villageCnt")
			//println(s"DayCnt: $dayCnt")
			//println(s"StartNo: $startNo")
			//println(s"TargetNoList: $targetNoList")
			println(solve(matrix, dayCnt, startNo, targetNoList))
		}
	}

	def solve(matrix: Seq[Seq[Int]], dayCnt: Int, startNo: Int, targetNoList: Vector[Int]): String = {
		val cache: mutable.HashMap[(Int, Int), Double] = mutable.HashMap()
		val deg = matrix.indices.map { no =>
			matrix.indices.count(n => matrix(no)(n) == 1)
		}.toVector

		def aux(villageNo: Int, days: Int): Double = {
			if (days == 0) return if (villageNo == startNo) 1 else 0
			cache.getOrElseUpdate((villageNo, days), {
				val preVillages = matrix.indices.filter(n => matrix(villageNo)(n) == 1)
				preVillages.map { prev =>
					1.0 / deg(prev) * aux(prev, days - 1)
				}.sum
			})
		}

		targetNoList.map(no => aux(no, dayCnt).formatted("%.7f")).mkString(" ")
	}

}
