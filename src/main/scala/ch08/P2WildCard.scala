package ch08

import scala.collection.mutable

/**
	* Created by ipoemi on 2016-12-05.
	*/
object P2WildCard {

	import scala.io._

	val in: String =
		"""3
			|he?p
			|3
			|help
			|heap
			|helpp
			|*p*
			|3
			|help
			|papa
			|hello
			|*bb*
			|1
			|babbbc
			|""".stripMargin

	//type Parser = (String) => Option[(Seq[Char], String)]

	object Parser {
		def hasNext(p: (String) => Boolean): Parser = new Parser {
			def apply(v1: String): Option[(Vector[Char], String)] = {
				if (p(v1)) Some((Vector(), v1)) else None
			}
		}

		def satisfy(p: (String) => Boolean): Parser = new Parser {
			def apply(v1: String): Option[(Vector[Char], String)] = {
				if (p(v1)) Some((Vector(v1.head), v1.tail.toString)) else None
			}
		}
	}

	trait Parser extends ((String) => Option[(Vector[Char], String)]) {
		self: ((String) => Option[(Vector[Char], String)]) =>

		def &&(that: Parser): Parser = new Parser {
			def apply(v1: String): Option[(Vector[Char], String)] = {
				val res1 = self(v1)
				res1.flatMap { x1 =>
					that(x1._2).map { x2 =>
						(x1._1 ++ x2._1, x2._2)
					}
				}
			}
		}

		def ||(that: Parser): Parser = new Parser {
			def apply(v1: String): Option[(Vector[Char], String)] = {
				val res1 = self(v1)
				if (res1.nonEmpty) res1
				else that(v1)
			}
		}

		def many: Parser = new Parser {
			def apply(v1: String): Option[(Vector[Char], String)] = {
				((self && self.many) || self) (v1)
			}
		}

	}

	val ch: Parser = Parser.hasNext(str => str.length > 0) && Parser.satisfy(str => str.head.isDigit || str.head.isUpper || str.head.isLower)




	def solve(wildcard: String, target: String): Boolean = {

		val cache = mutable.HashMap[(Int, Int), Boolean]()

		def aux(wIdx: Int, tIdx: Int): Boolean = {
			val cachedValue = cache.get((wIdx, tIdx))
			if (cachedValue.nonEmpty) return cachedValue.get

			if (wIdx < wildcard.length && tIdx < target.length && (wildcard(wIdx) == '?' || wildcard(wIdx) == target(tIdx))) {
				cache((wIdx, tIdx)) = aux(wIdx + 1, tIdx + 1)
				return cache((wIdx, tIdx))
			}

			if (wIdx == wildcard.length) {
				cache((wIdx, tIdx)) = tIdx == target.length
				return cache((wIdx, tIdx))
			}

			if (wildcard(wIdx) == '*') {
				if (aux(wIdx + 1, tIdx) || (tIdx < target.length && aux(wIdx, tIdx + 1))) {
					cache((wIdx, tIdx)) = true
					return cache((wIdx, tIdx))
				}
			}

			false
		}
		aux(0, 0)
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach {
			testNo =>
				val wildcard = source.next()
				val wordCnt = source.next().toInt

				val words = (0 until wordCnt).map(_ => source.next())

				println(s"-- testCase $testNo --")
				//println(s"WildCard: $wildcard")
				//println(s"Words: ")
				//words.foreach(println)
				words.filter(solve(wildcard, _)).sorted.foreach(println)
		}
	}

}
