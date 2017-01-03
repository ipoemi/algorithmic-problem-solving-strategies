package ch09

import scala.annotation.tailrec

object P12Genius {

	import com.util.memoize

	import scala.io._

	val in: String =
		"""3
			|3 6 3
			|4 4 2
			|0.18 0.40 0.42
			|0.15 0.46 0.39
			|0.58 0.23 0.19
			|0 1 2
			|4 10 4
			|1 3 2 4
			|0.26 0.07 0.49 0.18
			|0.21 0.33 0.15 0.31
			|0.41 0.20 0.38 0.01
			|0.28 0.31 0.18 0.23
			|2 0 3 1
			|4 1000 4
			|4 3 4 4
			|0.08 0.47 0.12 0.33
			|0.10 0.02 0.39 0.49
			|0.08 0.33 0.35 0.24
			|0.14 0.19 0.58 0.09
			|1 3 2 0
			|""".stripMargin

	val MaxMusicCnt = 50
	val MaxElapseTime = 1000000
	val MaxFavoriteMusic = 10
	val MaxPlayTime = 4
	val PlayTimeMod: Int = MaxPlayTime + 1

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next().toInt
		(1 to testCount).foreach { testNo =>
			val Array(musicCnt, elapseTime, _) = source.next().split(" ").map(_.toInt)
			val playTimeColl = source.next().split(" ").map(_.toInt).toVector
			val relationMatrix = (0 until musicCnt).toVector.map(_ => source.next().split(" ").map(_.toDouble).toVector)
			val favoriteMusicNoColl = source.next().split(" ").map(_.toInt).toVector

			println(s"-- testCase $testNo --")
			/*
			println(s"Music Cnt = $musicCnt")
			println(s"Elapse Time = $elapseTime")
			println(s"Favorite Music Cnt = $favoriteMusicCnt")
			println(s"Play Times = $playTimeColl")
			println(s"Relation Matrix: ")
			relationMatrix.foreach(println)
			println(s"Favorite Music No. List = $favoriteMusicNoColl")
			*/
			println(solve1(musicCnt, elapseTime, playTimeColl, relationMatrix, favoriteMusicNoColl))
			println(solve2(musicCnt, elapseTime, playTimeColl, relationMatrix, favoriteMusicNoColl))
			println(solve3(musicCnt, elapseTime, playTimeColl, relationMatrix, favoriteMusicNoColl))
		}
	}

	def solve2(musicCnt: Int, elapseTime: Int, playTimeColl: Vector[Int],
			relationMatrix: Vector[Vector[Double]], favoriteMusicNoColl: Vector[Int]): String = {

		@tailrec
		def aux(auxTime: Int, auxTmp: Vector[Vector[Double]]): Seq[Double] = {
			if (auxTime > elapseTime) {
				(0 until musicCnt).map { no =>
					(elapseTime - playTimeColl(no) + 1 to elapseTime).map(i => auxTmp(i % PlayTimeMod)(no)).sum
				}
			} else {
				val newAuxTmp = auxTmp.updated(auxTime % PlayTimeMod, (0 until musicCnt).toVector.map { cur =>
					(0 until musicCnt).map { prev =>
						auxTmp((auxTime - playTimeColl(prev) + PlayTimeMod) % PlayTimeMod)(prev) * relationMatrix(prev)(cur)
					}.sum
				})
				aux(auxTime + 1, newAuxTmp)
			}
		}

		val tmp = Vector.fill[Double](PlayTimeMod, musicCnt)(0.0)
		val result = aux(1, tmp.updated(0, tmp(0).updated(0, 1.0))).map(_.formatted("%.8f"))
		favoriteMusicNoColl.map(result(_)).mkString(" ")
	}

	def solve1(musicCnt: Int, elapseTime: Int, playTimeColl: Vector[Int],
			relationMatrix: Vector[Vector[Double]], favoriteMusicNoColl: Vector[Int]): String = {

		/*
		val cache = mutable.HashMap[(Int, Int), Double]()

		def aux(auxTime: Int, auxMusicNo: Int): Double = {
			val cachedValue = cache.get((auxTime, auxMusicNo))
			if (cachedValue.isEmpty) {
				cache((auxTime, auxMusicNo)) = (auxTime, auxMusicNo) match {
					case _ if auxTime <= 0 => if (auxTime == 0 && auxMusicNo == 0) 1.0 else 0
					case _ =>
						(0 until musicCnt).map { prevNo =>
							aux(auxTime - playTimeColl(prevNo), prevNo) * relationMatrix(prevNo)(auxMusicNo)
						}.sum
				}
			}
			cache((auxTime, auxMusicNo))
		}
		*/

		lazy val aux: (Int, Int) => (Double) = Function.untupled(memoize {
			case (time, musicNo) if time <= 0 => if (time == 0 && musicNo == 0) 1.0 else 0
			case (time, musicNo) =>
				(0 until musicCnt).map { prevNo =>
					aux(time - playTimeColl(prevNo), prevNo) * relationMatrix(prevNo)(musicNo)
				}.sum
		})


		val result = favoriteMusicNoColl.map { no =>
			(elapseTime - playTimeColl(no) + 1 to elapseTime).map(aux(_, no)).sum.formatted("%.8f")
		}.mkString(" ")
		//println(cache.keys.size)
		result
	}

	def solve3(musicCnt: Int, elapseTime: Int, playTimeColl: Vector[Int],
			relationMatrix: Vector[Vector[Double]], favoriteMusicNoColl: Vector[Int]): String = {
		val tmpArray = Array.fill[Double](4 * musicCnt, 4 * musicCnt)(0)
		(0 until 3 * musicCnt).foreach(i => tmpArray(i)(i + musicCnt) = 1.0)
		for (i <- 0 until musicCnt; j <- 0 until musicCnt)
			tmpArray(3 * musicCnt + i)((4 - playTimeColl(j)) * musicCnt + j) = relationMatrix(j)(i)
		val w = new SquareMatrix(tmpArray.map(_.toVector).toVector)
		val wk = w.pow(elapseTime)
		val result = (0 until musicCnt).map { no =>
			(0 until playTimeColl(no)).map(p => wk((3 - p) * musicCnt + no, 3 * musicCnt)).sum.formatted("%.8f")
		}
		favoriteMusicNoColl.map(result(_)).mkString(" ")
	}

	class SquareMatrix[A: Numeric](private val matrix: Vector[Vector[A]]) {
		assert(matrix.nonEmpty)
		assert(matrix.size == matrix(0).size)

		def +(that: SquareMatrix[A]): SquareMatrix[A] = {
			assert(that.matrix.size == this.matrix.size)
			val matrixBuilder = Vector.newBuilder[Vector[A]]
			for (y <- matrix.indices) {
				val rowBuilder = Vector.newBuilder[A]
				for (x <- matrix(y).indices) {
					rowBuilder += implicitly[Numeric[A]].plus(this.matrix(y)(x), that.matrix(y)(x))
				}
				matrixBuilder += rowBuilder.result()
			}
			new SquareMatrix(matrixBuilder.result())
		}

		def updated(y: Int, x: Int, value: A): SquareMatrix[A] = {
			val matrixBuilder = Vector.newBuilder[Vector[A]]
			for (y1 <- matrix.indices) {
				val rowBuilder = Vector.newBuilder[A]
				for (x1 <- matrix(y).indices) {
					if (y1 == y && x1 == x)
						rowBuilder += value
					else
						rowBuilder += this.matrix(y1)(x1)
				}
				matrixBuilder += rowBuilder.result()
			}
			new SquareMatrix(matrixBuilder.result())
		}

		def *(that: SquareMatrix[A]): SquareMatrix[A] = {
			assert(that.matrix.size == this.matrix(0).size)
			val num = implicitly[Numeric[A]]
			val matrixBuilder = Vector.newBuilder[Vector[A]]
			for (y <- matrix.indices) {
				val rowBuilder = Vector.newBuilder[A]
				for (x <- that.matrix.indices) {
					val value = matrix(y).zip(that.matrix.map(_ (x))).map { case (v1, v2) => num.times(v1, v2) }.sum
					rowBuilder += value
				}
				matrixBuilder += rowBuilder.result()
			}
			new SquareMatrix(matrixBuilder.result())
		}

		def pow(n: Int): SquareMatrix[A] = {
			assert(this.matrix.size == this.matrix(0).size)
			if (n == 0) SquareMatrix.identity(this.matrix.size)
			else if (n % 2 > 0) pow(n - 1) * this
			else {
				val half = pow(n / 2)
				half * half
			}
		}

		def apply(y: Int, x: Int): A = {
			matrix(y)(x)
		}

		override def toString: String = {
			matrix.map("|" + _.mkString(" ") + "|").mkString("\n")
		}
	}

	object SquareMatrix {

		def apply[A: Numeric](n: Int): SquareMatrix[A] = {
			new SquareMatrix[A](Vector.fill(n, n)(implicitly[Numeric[A]].zero))
		}

		def identity[A: Numeric](n: Int): SquareMatrix[A] = {
			val num = implicitly[Numeric[A]]
			new SquareMatrix(Vector.tabulate(n, n) { case (y, x) => if (y == x) num.one else num.zero })
		}
	}

}

