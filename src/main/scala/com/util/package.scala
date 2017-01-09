package com

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordering.Implicits._

package object util {
	def memoize[I, O](f: I => O): collection.Map[I, O] = new mutable.HashMap[I, O]() {
		override def apply(key: I): O = getOrElseUpdate(key, f(key))
	}

	def findMinUpper[T: Ordering](coll: Seq[T], elem: T): Option[T] = {
		val idx = indexOfMinUpper(coll, elem)
		if (idx == -1) None
		else Some(coll(idx))
	}

	def findMaxLower[T: Ordering](coll: Seq[T], elem: T): Option[T] = {
		val idx = indexOfMaxLower(coll, elem)
		if (idx == -1) None
		else Some(coll(idx))
	}

	def indexOfMinUpper[T: Ordering](coll: Seq[T], elem: T): Int = {
		@tailrec
		def aux(start: Int, end: Int, elem: T): Int = {
			//println(s"start: $start, end: $end")
			if (start > end) return -1
			if (start == end) return if (elem <= coll(start)) start else -1
			val half = (start + end) / 2
			if (elem > coll(half) && elem <= coll(half + 1)) half + 1
			else if (elem > coll(half)) aux(half, end, elem)
			else aux(start, half, elem)
		}

		aux(0, coll.size - 1, elem)
	}

	def indexOfMaxLower[T: Ordering](coll: Seq[T], elem: T): Int = {
		@tailrec
		def aux(start: Int, end: Int, elem: T): Int = {
			//println(s"start: $start, end: $end")
			if (start > end) return -1
			if (start == end) return if (elem >= coll(start)) start else -1
			val half = (start + end) / 2
			if (elem >= coll(half) && elem < coll(half + 1)) half
			else if (elem < coll(half)) aux(start, half, elem)
			else aux(half, end, elem)
		}

		aux(0, coll.size - 1, elem)
	}

}
