package com

import scala.collection.mutable

/**
 * Created by ipoemi on 2016-12-20.
 */

package object util {
	def memoize[I, O](f: I => O): collection.Map[I, O] = new mutable.HashMap[I, O]() {
		override def apply(key: I): O = getOrElseUpdate(key, f(key))
	}
}
