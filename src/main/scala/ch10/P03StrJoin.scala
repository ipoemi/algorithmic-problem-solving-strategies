package ch10

import scala.collection.mutable

object P03StrJoin {

	import scala.io._

	val in: String =
		"""3
			|3
			|2 2 4
			|5
			|3 1 3 4 1
			|8
			|1 1 1 1 1 1 1 2
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val _ = source.next().split(" ").map(_.toInt)
			val strLengthColl = source.next().split(" ").map(_.toInt).toVector

			//println(s"-- testCase $testNo --")
			//println(s"String Lengths: ${strLengthColl}")
			println(solve(strLengthColl))
		}
	}

	def solve(strLengthColl: Seq[Int]): String = {
		var ret = 0
		val pq = new mutable.PriorityQueue[Int]()(Ordering.by(-_))
		strLengthColl.foreach(pq.enqueue(_))
		while (pq.size > 1) {
			val min1 = pq.dequeue()
			val min2 = pq.dequeue()
			pq.enqueue(min1 + min2)
			ret += min1 + min2
		}
		ret.toString
	}

}

