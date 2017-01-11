package ch09

import scala.annotation.tailrec

object P11Sushi {

	import scala.io._

	val in: String =
		"""2
			|6 10000
			|2500 7
			|3000 9
			|4000 10
			|5000 12
			|10000 20
			|15000 1
			|6 543975612
			|2500 7
			|3000 9
			|4000 10
			|5000 12
			|10000 20
			|15000 1
			|""".stripMargin

	val MaxSushi = 20
	val MaxBudget = 1000000000
	val MaxPrice = 20000

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val Array(n, budget) = source.next().split(" ").map(_.toInt)
			val sushiColl = (0 until n).map { _ =>
				val sushiSrc = source.next().split(" ").map(_.toInt)
				Sushi(sushiSrc(0), sushiSrc(1))
			}

			//println(s"-- testCase $testNo --")
			//println(s"Money = $budget")
			//println(s"n = $n")
			//println(s"Sushi List = $sushiColl")
			println(solve(sushiColl, budget))
		}
	}

	def solve(sushiColl: Seq[Sushi], budget: Int): String = {
		val newSushiColl = sushiColl.map(sushi => sushi.copy(price = sushi.price / 100))
		val newBudget = budget / 100

		/*
		var ret = 0
		val iterim = Array.fill(201)(0)
		for (fBudget <- 1 to newBudget) {
			val cand = (0 +: newSushiColl.filter(_.price <= fBudget).map { sushi =>
				iterim((fBudget - sushi.price) % 201) + sushi.pref
			}).max
			iterim(fBudget % 201) = cand
			ret = ret.max(cand)
		}
		ret.toString
		*/
		@tailrec
		def aux(auxBudget: Int, auxTmp: Vector[Int], auxResult: Int): Int = {
			if (auxBudget > newBudget) auxResult
			else {
				val cand = (0 +: newSushiColl.filter(_.price <= auxBudget).map { sushi =>
					auxTmp((auxBudget - sushi.price) % 201) + sushi.pref
				}).max
				aux(auxBudget + 1, auxTmp.updated(auxBudget % 201, cand), auxResult.max(cand))
			}
		}

		aux(0, Vector.fill(201)(0), 0).toString
	}

	case class Sushi(price: Int, pref: Int)

}

