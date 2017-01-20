package ch06

object P03BoardCover {

  import scala.io._

  val coverTypes = Vector(
    Vector((0, 0), (1, 0), (0, 1)),
    Vector((0, 0), (0, 1), (1, 1)),
    Vector((0, 0), (1, 0), (1, 1)),
    Vector((0, 0), (1, 0), (1, -1))
  )

  val in: String =
    """3
      |3 7
      |#.....#
      |#.....#
      |##...##
      |3 7
      |#.....#
      |#.....#
      |##..###
      |8 10
      |##########
      |#........#
      |#........#
      |#........#
      |#........#
      |#........#
      |#........#
      |##########
      |""".stripMargin

  def printBoard(board: Array[Array[Int]]): Unit = {
    board.indices.foreach { y =>
      board(y).indices.foreach { x =>
        print(s"${board(y)(x)} ")
      }
      println()
    }
    println()
  }

  def set(board: Array[Array[Int]], y: Int, x: Int, coverType: Int, delta: Int): Boolean = {
    var ret = true
    coverTypes(coverType).foreach { tp =>
      val (ny, nx) = (y + tp._1, x + tp._2)
      if (ny < 0 || ny >= board.length || nx < 0 || nx > board(y).length) {
        ret = false
      } else {
        board(ny)(nx) += delta
        if (board(ny)(nx) > delta) ret = false
      }
    }
    ret
  }

  def solve(board: Array[Array[Int]]): Int = {
    //printBoard(board)
    var (y, x) = (-1, -1)
    for (ny <- board.indices if y == -1)
      for (nx <- board(ny).indices if x == -1)
        if (board(ny)(nx) == 0) {
          y = ny
          x = nx
        }
    //println(s"y: $y, x: $x")
    if (y == -1) {
      //printBoard(board)
      return 1
    }
    coverTypes.indices.map { coverType =>
      val ret =
        if (set(board, y, x, coverType, 1)) solve(board)
        else 0
      set(board, y, x, coverType, -1)
      ret
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromString(in).getLines()
    val testCount = source.next.toInt
    (1 to testCount).foreach { testNo =>
      val Array(rowCnt, _) = source.next.split(' ').map(_.toInt)
      val board = (0 until rowCnt).map { _ =>
        source.next.map(x => if (x == '#') 1 else 0).toArray
      }.toArray

      //println(s"-- testCase $testNo --")
      println(solve(board))
    }
  }
}
