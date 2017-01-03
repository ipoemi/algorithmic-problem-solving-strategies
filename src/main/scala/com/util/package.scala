package com

import scala.collection.mutable

package object util {
	def memoize[I, O](f: I => O): collection.Map[I, O] = new mutable.HashMap[I, O]() {
		override def apply(key: I): O = getOrElseUpdate(key, f(key))
	}
}
