package ch08

import scala.collection.mutable

object P11AsymmetryTiling {

  import scala.io._

  val in: String =
    """3
      |2
      |4
      |92
      |""".stripMargin

  val Mod: Int = 1000000007

  val tilingCache: mutable.HashMap[Int, Int] = mutable.HashMap()

  def tilingAux(n: Int): Int = {
    if (n <= 1) return 1
    val cachedValue = tilingCache.get(n)
    if (cachedValue.nonEmpty) return cachedValue.get
    tilingCache(n) = (tilingAux(n - 2) + tilingAux(n - 1)) % Mod
    tilingCache(n)
  }

  def solve(n: Int): Int = {
    def asymAux(tiles: Int): Int = {
      if (tiles % 2 == 1) {
        (tilingAux(tiles) - tilingAux(tiles / 2) + Mod) % Mod
      } else {
        (tilingAux(tiles) - tilingAux(tiles / 2) - tilingAux((tiles - 2) / 2) + Mod * 2) % Mod
      }
    }

    asymAux(n)
  }

  def solve2(n: Int): Int = {
    val asymCache: mutable.HashMap[Int, Int] = mutable.HashMap()

    def asymAux(tiles: Int): Int = {
      if (tiles <= 2) return 0
      asymCache.getOrElseUpdate(tiles, {
        var ret = asymAux(tiles - 2) % Mod
        ret = (ret + asymAux(tiles - 4)) % Mod
        ret = (ret + tilingAux(tiles - 3)) % Mod
        ret = (ret + tilingAux(tiles - 3)) % Mod
        ret
      })
    }

    asymAux(n)
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    (1 to testCount).foreach { testNo =>
      val n = source.next().toInt

      //println(s"-- testCase $testNo --")
      //println(s"N: $n")
      //println(solve(n))
      println(solve2(n))
    }
  }

}
