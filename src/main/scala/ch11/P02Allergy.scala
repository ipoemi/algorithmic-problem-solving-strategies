package ch11

object P02Allergy {

  import scala.io._

  val in: String =
    """2
      |4 6
      |cl bom dara minzy
      |2 dara minzy
      |2 cl minzy
      |2 cl dara
      |1 cl
      |2 bom dara
      |2 bom minzy
      |10 7
      |a b c d e f g h i j
      |6 a c d h i j
      |3 a d i
      |7 a c f g h i j
      |3 b d g
      |5 b c f h i
      |4 b e g j
      |5 b c g h i
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next().toInt
    //val startTime = System.nanoTime()
    (1 to testCount).foreach { testNo =>
      val Array(_, foodCnt) = source.next().split(" ").map(_.toInt)
      val friends = source.next().split(" ").toSeq
      val foods = (0 until foodCnt).map(_ => source.next().split(" ").tail.toSeq.map(name => friends.indexOf(name)))

      //println(s"-- testCase $testNo --")
      //println(s"Friends: $friends")
      //println(s"Foods: ")
      //foods.foreach(println)
      println(solve2(friends, foods))
    }
    //println(s"Spent Time: ${(System.nanoTime() - startTime) / 1000.0}")
  }

  def solve(friends: Seq[String], foods: Seq[Seq[Int]]): String = {

    def validSelect(selectedFoods: Seq[Int]): Boolean = {
      selectedFoods.flatMap(idx => foods(idx)).toSet.size == friends.size
    }

    def aux(remainFoods: Seq[Int], selectedFood: Seq[Int], best: Seq[Int]): Seq[Int] = {
      remainFoods match {
        case Seq() =>
          if (validSelect(selectedFood))
            if (selectedFood.size < best.size) return selectedFood
          best
        case _ =>
          if (selectedFood.size < best.size) {
            val value = aux(remainFoods.tail, selectedFood, best)
            aux(remainFoods.tail, selectedFood :+ remainFoods.head, value)
          } else {
            best
          }
      }
    }

    val result = aux(foods.indices, Seq(), foods.indices)
    println(result)
    result.size.toString
  }

  def solve2(friends: Seq[String], foods: Seq[Seq[Int]]): String = {

    //val sortedFoods = foods.sortBy(_.size)
    val sortedFoods = foods
    val canEat = friends.indices.map(friendNo => sortedFoods.indices.filter(idx => sortedFoods(idx).contains(friendNo)))
    val newCanEat = canEat
    val newFoods = sortedFoods
    //canEat.foreach(println)
    //val sortedCanEat = canEat.zipWithIndex.sortBy(_._1.size)
    //val idxMap = sortedCanEat.map(_._2).zipWithIndex.sortBy(_._1).map(_._2)
    //println(idxMap)

    //val newFoods = sortedFoods.map(edible => edible.map(idxMap(_)))
    //val newCanEat = sortedCanEat.map(_._1)
    /*
    println("==============================ipoemi==============================")
    newCanEat.foreach(println)
    println("-------------------------------------------------------------")
    val tmpCanEat = friends.indices.map(friendNo => newFoods.indices.filter(idx => newFoods(idx).contains(friendNo)))
    tmpCanEat.foreach(println)
    */

    def aux(edible: Seq[Boolean], chosen: Int, best: Int): Int = {
      if (chosen >= best) best
      else {
        edible.zipWithIndex.filter(!_._1).map(_._2) match {
          case Seq() => chosen
          case cand =>
            newCanEat(cand.head).foldLeft(best) { (newBest, foodNo) =>
              val newEdible = newFoods(foodNo).foldLeft(edible) { (acc, friendNo) => acc.updated(friendNo, true) }
              newBest.min(aux(newEdible, chosen + 1, newBest))
            }
        }
      }
    }

    //aux(0, friends.map(_ => false), 0, foods.size).toString
    aux(friends.map(_ => false), 0, foods.size).toString
  }

}

