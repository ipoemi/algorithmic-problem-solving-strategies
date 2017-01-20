package ch09

import scala.collection.mutable

object P01Packing {

  import scala.io._

  val in: String =
    """2
      |6 10
      |laptop 4 7
      |camera 2 10
      |xbox 6 6
      |grinder 4 7
      |dumbell 2 5
      |encyclopedia 10 4
      |6 17
      |laptop 4 7
      |camera 2 10
      |xbox 6 6
      |grinder 4 7
      |dumbell 2 5
      |encyclopedia 10 4
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    (1 to testCount).foreach { testNo =>
      val Array(itemCnt, capacity) = source.next().split(" ").map(_.toInt)
      val itemList = (0 until itemCnt).map { _ =>
        val line = source.next().split(" ")
        Item(line(0), line(1).toInt, line(2).toInt)
      }.toVector

      //println(s"-- testCase $testNo --")
      //println(s"Items:")
      //itemList.foreach(println)
      println(solve(itemList, capacity))
    }
  }

  def solve(itemList: Seq[Item], capacity: Int): String = {
    val cache: mutable.HashMap[(Int, Int), Int] = mutable.HashMap()

    def pack(itemIdx: Int, remainCapacity: Int): Int = {
      if (itemIdx == itemList.size) return 0
      cache.getOrElseUpdate((itemIdx, remainCapacity), {
        val cand1 = pack(itemIdx + 1, remainCapacity)
        val item = itemList(itemIdx)
        val cand2 =
          if (remainCapacity >= item.volume) {
            pack(itemIdx + 1, remainCapacity - item.volume) + item.need
          } else 0
        cand1.max(cand2)
      })
    }

    def aux(itemIdx: Int, remainCapacity: Int, packed: Vector[Item]): Vector[Item] = {
      if (itemIdx == itemList.size) return packed
      val item = itemList(itemIdx)
      if (pack(itemIdx, remainCapacity) == pack(itemIdx + 1, remainCapacity)) {
        aux(itemIdx + 1, remainCapacity, packed)
      } else {
        aux(itemIdx + 1, remainCapacity - item.volume, packed :+ item)
      }
    }

    val maxNeed = pack(0, capacity)
    val packedItems = aux(0, capacity, Vector())
    val resultBuilder = new StringBuilder()
    resultBuilder.append(s"$maxNeed ${packedItems.size}")
    packedItems.foreach(item => resultBuilder.append(s"\n${item.name}"))
    resultBuilder.toString
  }

  case class Item(name: String, volume: Int, need: Int)

}
