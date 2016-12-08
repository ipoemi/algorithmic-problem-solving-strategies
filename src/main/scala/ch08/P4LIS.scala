package ch08

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
	* Created by ipoemi on 2016-12-05.
	*/
object P4LIS {

	import scala.io._

	val in: String =
		"""3
			|4
			|1 2 3 4
			|8
			|5 4 3 2 1 6 7 8
			|8
			|5 6 7 8 1 2 3 4
			|""".stripMargin

	/*
	def solve(seq: Seq[Int], cache: mutable.HashMap[Seq[Int], Seq[Int]] = mutable.HashMap()): Seq[Int] = {
		if (seq.isEmpty) return seq
		cache.getOrElseUpdate(seq, {
			seq.indices.map { n =>
				seq(n) +: solve(seq.slice(n, seq.size).filter(_ > seq(n)), cache)
			}.maxBy(_.size)
		})
	}
	*/

	def solve(seq: Seq[Int]): Int = {
		if (seq.isEmpty) return 0

		val cache: Array[Option[Int]] = Array.fill(seq.size + 1)(None)

		def aux(idx: Int): Int = {
			val cachedValue = cache(idx + 1)
			if (cachedValue.nonEmpty) return cachedValue.get
			val retList =
				for (i <- seq.indices if i > idx && (idx == -1 || seq(i) > seq(idx))) yield
					1 + aux(i)
			cache(idx + 1) = Some((retList :+ 1).max)
			cache(idx + 1).get
		}

		aux(-1) - 1
	}

	def solve2(seq: Seq[Int]): Int = {

		if (seq.isEmpty) return 0

		val buffer = new ArrayBuffer[Int]()

		def findMinUpper(start: Int, end: Int, elem: Int): Int = {
			//println(s"start: $start, end: $end")
			if (start > end) return -1
			if (start == end) return if (elem <= buffer(start)) start else -1
			val half = start + end / 2
			if (buffer(half) < elem && buffer(half + 1) > elem) half
			else if (buffer(half) < elem) findMinUpper(half, end, elem)
			else findMinUpper(start, half, elem)
		}


		var idx = 0

		buffer += seq(idx)
		idx += 1

		while (idx < seq.size) {
			//println(s"idx: $idx, buffer-size: ${buffer.size}")
			if (buffer(buffer.size - 1) < seq(idx)) buffer += seq(idx)
			else {
				val idx1 = findMinUpper(0, buffer.size - 1, seq(idx))
				//println(s"buffer: $buffer, elem: ${seq(idx)}")
				//println(s"idx1: $idx1")
				buffer(idx1) = seq(idx)
			}
			idx += 1
		}
		buffer.size
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).take(3).foreach {
			testNo =>
				val _ = source.next()
				val sequence = source.next().split(" ").toVector.map(_.toInt)

				println(s"-- testCase $testNo --")
				println(s"Sequence: $sequence")
				println(solve(sequence))
				println(solve2(sequence))
		}
	}

}
