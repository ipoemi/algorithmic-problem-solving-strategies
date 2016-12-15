package ch09

import scala.collection.mutable

/**
 * Created by ipoemi on 2016-12-05.
 */

object P02OCR {

	import scala.io._

	val in: String =
		"""5 3
			|I am a boy buy
			|1.0 0.0 0.0 0.0 0.0
			|0.1 0.6 0.1 0.1 0.1
			|0.1 0.1 0.6 0.1 0.1
			|0.1 0.1 0.1 0.6 0.1
			|0.2 0.2 0.2 0.2 0.2
			|0.2 0.2 0.2 0.2 0.2
			|0.8 0.1 0.0 0.1 0.0
			|0.1 0.7 0.0 0.2 0.0
			|0.0 0.1 0.8 0.0 0.1
			|0.0 0.0 0.0 0.5 0.5
			|0.0 0.0 0.0 0.5 0.5
			|4 I am a buy
			|4 I I a boy
			|4 I am am boy
			|""".stripMargin

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val Array(wordCnt, sentenceCnt) = source.next().split(" ").map(_.toInt)
		val wordSeq = source.next().split(" ").toSeq
		val firstPosProbSeq = source.next().split(" ").map(n => Math.log(n.toFloat)).toSeq
		val nextPosProbMat = (0 until wordCnt).map(_ => source.next().split(" ").map(n => Math.log(n.toFloat)).toSeq)
		val errorPosProbMat = (0 until wordCnt).map(_ => source.next().split(" ").map(n => Math.log(n.toFloat)).toSeq)
		val sentenceSeq = (0 until sentenceCnt).map(_ => source.next().split(" ").drop(1).toSeq)

		//println(s"Words: $wordSeq")
		//println(s"First Position Probability: $firstPosProbSeq")
		//println(s"Next Position Probability: ")
		//nextPosProbMat.foreach(row => println(row))
		//println(s"Error Position Probability: ")
		//errorPosProbMat.foreach(row => println(row))
		//println(s"Sentences: ")
		//sentenceSeq.foreach(row => println(row))
		sentenceSeq.foreach(row => println(solve(row, wordSeq, firstPosProbSeq +: nextPosProbMat, errorPosProbMat)))
	}

	def solve(sentence: Seq[String], wordSeq: Seq[String],
			nextPosProbMat: Seq[Seq[Double]], errorPosProbMat: Seq[Seq[Double]]): String = {

		val sentenceIdxSeq = sentence.map(word => wordSeq.indexWhere(_ == word))

		val cache: mutable.HashMap[(Int, Int), (Double, Seq[Int])] = mutable.HashMap()

		def aux(idx: Int, prevWordIdx: Int): (Double, Seq[Int]) = {
			if (idx == sentenceIdxSeq.size) return (0, List())
			cache.getOrElseUpdate((idx, prevWordIdx), {
				val candSeq =
					for (curWordIdx <- wordSeq.indices) yield {
						val ret = aux(idx + 1, curWordIdx)
						val value = nextPosProbMat(prevWordIdx + 1)(curWordIdx) +
								errorPosProbMat(curWordIdx)(sentenceIdxSeq(idx)) + ret._1
						(value, idx +: ret._2)
					}
				candSeq.maxBy(_._1)
			})
		}

		val (value, seq) = aux(0, -1)
		(value, seq.map(wordSeq(_)))._2.mkString(" ")
	}

}
