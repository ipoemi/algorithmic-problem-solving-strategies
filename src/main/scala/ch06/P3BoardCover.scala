package ch06

/**
	* Created by ipoemi on 2016-11-26.
	*/
object P3BoardCover {

	import scala.io._

	val coverType = Vector(
		Vector((0, 0), (1, 0), (0, 1)),
		Vector((0, 0), (0, 1), (1, 1)),
		Vector((0, 0), (1, 0), (1, 1)),
		Vector((0, 0), (1, 0), (1, -1))
	)

	val in =
		"""3
			|2 1
			|0 1
			|4 6
			|0 1 1 2 2 3 3 0 0 2 1 3
			|6 10
			|0 1 0 2 1 2 1 3 1 4 2 3 2 4 3 4 3 5 4 5
			|""".stripMargin

}
