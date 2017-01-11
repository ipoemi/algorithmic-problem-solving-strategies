package ch07

import scala.collection.mutable.ArrayBuffer

object P03FanMeeting {

	import scala.io._

	val in: String =
		"""4
			|FFFMMM
			|MMMFFF
			|FFFFF
			|FFFFFFFFFF
			|FFFFM
			|FFFFFMMMMF
			|MFMFMFFFMMMFMF
			|MMFFFFFMFFFMFFFFFFMFFFMFFFFMFMMFFFFFFF
			|""".stripMargin


	val out: String =
		"""1
			|6
			|2
			|2
		""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next.toInt
		(1 to testCount).foreach {
			testNo =>
				val members = source.next()
				val fans = source.next()

				//println(s"-- testCase $testNo --")
				println(s"Members: $members")
				println(s"Fans: $fans")

				val membersInt = members.map(x => if (x == 'M') '1' else '0')
				val fansInt = fans.map(x => if (x == 'M') '1' else '0')
				val membersIntList = IntList(membersInt.reverse)
				val fansIntList = IntList(fansInt)
				val mulRet = membersIntList * fansIntList
				println(mulRet.toString.split(" ").slice(membersIntList.size - 1, fansIntList.size).map(_.toInt).count(_ == 0))
		}
	}

	class IntList(from: Seq[Int]) {
		private val KaratsubaThreshold = 50

		private val elems = from.toVector.reverse

		def *(that: IntList): IntList = {
			if (this.size < that.size) return that * this
			if (this.size == 0 || that.size == 0) return IntList(Seq())
			if (this.size < KaratsubaThreshold) return this.multiply(that)

			val half = this.size / 2
			val a0 = IntList(elems.slice(0, half))
			val a1 = IntList(elems.slice(half, elems.size))
			val b0 = IntList(elems.slice(0, half.min(that.elems.size)))
			val b1 = IntList(elems.slice(half.min(that.elems.size), that.elems.size))

			val z2 = a1 * b1
			val z0 = a0 * b0
			val z1 = (a0 + a1) * (b0 + b1)
			z0 + (z1 << half) + (z2 << (half * 2))
		}

		def <<(n: Int): IntList = {
			val ret = new ArrayBuffer[Int]()
			(0 until n).foreach(_ => ret += 0)
			ret ++= elems
			IntList(ret.reverse)
		}

		def +(that: IntList): IntList = {
			val size = this.size.max(that.size)
			val ret = ArrayBuffer.fill(size)(0)
			for (n <- 0 until size)
				ret(n) = (if (this.size > n) elems(n) else 0) + (if (that.size > n) that.elems(n) else 0)
			IntList(ret.reverse)
		}

		def -(that: IntList): IntList = {
			val size = this.size.max(that.size)
			val ret = ArrayBuffer.fill(size)(0)
			for (n <- 0 until size)
				ret(n) = (if (this.size > n) elems(n) else 0) - (if (that.size > n) that.elems(n) else 0)
			IntList(ret.reverse)
		}

		def size: Int = elems.size

		override def toString: String = {
			elems.reverse.mkString(" ")
		}

		private def multiply(that: IntList): IntList = {
			val ret = ArrayBuffer.fill(this.size.max(that.size))(0)
			for (i <- elems.indices)
				for (j <- that.elems.indices) {
					if (i + j >= ret.size) ret += 0
					ret(i + j) += elems(i) * that.elems(j)
				}
			//println(s"i1: ${elems}, i2: ${that.elems}")
			//println(s"multi: ${ret}")
			IntList(ret.reverse)
		}
	}

	object IntList {
		def apply(from: String) = new IntList(from.map(_.toString.toInt))

		def apply(from: Seq[Int]) = new IntList(from)
	}

}
