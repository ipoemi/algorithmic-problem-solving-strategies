package ch06

/**
	* Created by ipoemi on 2016-11-30.
	*/
object P5QuadTree {

	import scala.io._

	implicit class IntAddedTimes(n: Int) {
		def times(block: => Unit): Unit = {
			(1 to n).foreach(_ => block)
		}
	}

	val MaxSize: Int = Math.pow(2, 20).toInt

	val in: String =
		"""4
			|w
			|xbwwb
			|xbwxwbbwb
			|xxwwwbxwxwbbbwwxxxwwbbbwwwwbb
			|""".stripMargin

	def decompress(decompressed: Array[Array[Int]], strIter: Iterator[Char], y: Int, x: Int, size: Int): Unit = {
		val head = strIter.next()
		if (head == 'b' || head == 'w') {
			for (ny <- 0 until size) {
				for (nx <- 0 until size) {
					decompressed(ny)(nx) = head
				}
			}
		} else {
			val half = size / 2
			decompress(decompressed, strIter, y, x, half)
			decompress(decompressed, strIter, y, x + half, half)
			decompress(decompressed, strIter, y + half, x, half)
			decompress(decompressed, strIter, y + half, x + half, half)
		}
	}

	trait QuadTree

	case class Leaf(char: Char) extends QuadTree

	case class Node(n1: QuadTree, n2: QuadTree, n3: QuadTree, n4: QuadTree) extends QuadTree

	def buildTree(strIter: Iterator[Char]): QuadTree = {
		val head = strIter.next()
		if (head == 'b' || head == 'w') {
			Leaf(head)
		} else {
			Node(
				buildTree(strIter),
				buildTree(strIter),
				buildTree(strIter),
				buildTree(strIter)
			)
		}
	}

	def tree2invStr(tree: QuadTree): String = {
		tree match {
			case leaf: Leaf => leaf.char.toString
			case node: Node =>
				"x" + tree2invStr(node.n3) + tree2invStr(node.n4) + tree2invStr(node.n1) + tree2invStr(node.n2)
		}
	}

	def reverse(strIter: Iterator[Char]): String = {
		val head = strIter.next()
		if (head == 'b' || head == 'w') {
			head.toString
		} else {
			val leftTop = reverse(strIter)
			val rightTop = reverse(strIter)
			val leftBottom = reverse(strIter)
			val rightBottom = reverse(strIter)
			"x" + leftBottom + rightBottom + leftTop + rightTop
		}
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromString(in).getLines()
		val testCount = source.next.toInt
		(1 to testCount).foreach {
			testNo =>
				val compressedStr = source.next()
				println(s"-- testCase $testNo --")
				println(s"Compressed String: $compressedStr")
				println(s"Reverse Compressed String: ${reverse(compressedStr.toIterator)}")
		}
	}
}
