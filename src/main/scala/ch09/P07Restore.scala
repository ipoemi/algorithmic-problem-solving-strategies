package ch09

object P07Restore {

	import com.util.memoize

	import scala.collection.mutable.ArrayBuffer
	import scala.io._


	val in: String =
		"""3
			|3
			|geo
			|oji
			|jing
			|2
			|world
			|hello
			|3
			|abrac
			|cadabra
			|dabr
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val numWords = source.next().toInt
			val wordColl = new ArrayBuffer[String]()
			(0 until numWords).foreach { _ =>
				val word = source.next()
				if (wordColl.forall(!_.contains(word))) wordColl += word
			}

			//println(s"-- testCase $testNo --")
			//println(s"NumOfWords = $numWords")
			//println(s"Words:")
			//println(wordColl)
			println(solve(wordColl))
		}
	}

	def solve(wordColl: Seq[String]): String = {
		lazy val overlapAux: (Int, Int) => (Int) = Function.untupled(memoize {
			case (_, used) if used == ((1 << wordColl.size) - 1) => 0
			case (last, used) =>
				wordColl.indices.filter { i =>
					(used & (1 << i)) == 0
				}.map { i =>
					val overlapCnt = if (last == -1) 0 else getOverlapCnt(wordColl(last), wordColl(i))
					overlapCnt + overlapAux(i, used | (1 << i))
				}.max
		})

		def getShortestWords(last: Int, used: Int): String = {
			if (used == ((1 << wordColl.size) - 1)) return ""
			val idxOpt = wordColl.indices.filter { i =>
				(used & (1 << i)) == 0
			}.find { i =>
				val overlapCnt = if (last == -1) 0 else getOverlapCnt(wordColl(last), wordColl(i))
				//println(s"overlapAux(last, used) = ${overlapAux(last, used)}, other = ${overlapCnt + overlapAux(i, used | (1 << i))}")
				overlapCnt + overlapAux(i, used | (1 << i)) == overlapAux(last, used)
			}
			val overlapCnt = if (last == -1) 0 else getOverlapCnt(wordColl(last), wordColl(idxOpt.get))
			val nextWord = wordColl(idxOpt.get)
			val nextResult = nextWord.substring(overlapCnt)
			nextResult + getShortestWords(idxOpt.get, used | (1 << idxOpt.get))
		}

		//wordColl.indices.map(overlapAux(_, 0)).max.toString
		//overlapAux(-1, 0).toString
		getShortestWords(-1, 0)
	}

	def getOverlapCnt(a: String, b: String): Int = {
		val cnt = a.length.min(b.length)
		var maxMatch = 0
		(0 until cnt).foreach { i =>
			if ((0 to i).forall(n => a(a.length - 1 - n) == b(i - n)))
				maxMatch = i + 1
		}
		maxMatch
		/*
		val diff = a.length - b.length
		val (newA, newB) =
			if (diff >= 0) (a.drop(diff), b)
			else (a, b.take(-diff))
		(0 until newA.size).map { n =>
			val droppedA = newA.drop(n)
			if (droppedA.zip(newB).forall(z => z._1 == z._2)) droppedA.length
			else 0
		}.max
		*/
	}

}

