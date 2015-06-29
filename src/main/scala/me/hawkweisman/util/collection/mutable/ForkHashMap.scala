package me.hawkweisman.util
package collection.mutable

import me.hawkweisman.util.collection.ForkTable

import scala.collection.mutable

/**
 * Created by hawk on 6/29/15.
 */
class ForkHashMap[K,V](
                        protected var parent: Option[ForkHashMap[K,V]] = None,
                        protected var children: Seq[ForkHashMap[K,V]] = Seq()
                        ) extends ForkTable[K,V] {
  override type Self = ForkHashMap[K,V]
  /**
   * Change the parent corresponding to this scope.
   * @param nParent the new parent
   * @throws IllegalArgumentException if the specified parent was invalid
   */
  override def reparent(nParent: Self): Self = {
    require(nParent != this, "Scope cannot mount itself as parent!")
    parent foreach ( _ removeChild this )
    nParent addChild this
    parent = Some(nParent)
    this
  }

  override def freeze(): Self = {
    parent foreach { oldParent =>
      this.parent = None
      this.back ++= oldParent.iterator withFilter {
        case((key,_)) => !back.contains(key) && !whiteouts.contains(key)
      }
    }
    this
  }

  /**
   * Forks this table, returning a new `ForkTable[K,V]`.
   *
   * This level of the table will be set as the child's
   * parent. The child will be created with an empty backing
   * [[scala.collection.mutable.HashMap HashMap]] and no keys whited out.
   *
   * @return a new child of this scope
   */
  override def fork(): Self = {
    val c = new Self(parent = Some(this))
    children = children :+ c
    c
  }

  override protected def addChild(other: Self): Self = {
    children = children :+ other; this
  }

  override protected def removeChild(other: Self): Self = {
    this.children = children.filter({other != _}); this
  }

  override protected val back = mutable.Map()[K,V]

  override protected val whiteouts = mutable.Set()[K]

  /**
   * Inserts a key-value pair from the map.
   *
   * If the key already had a value present in the map, that
   * value is returned. Otherwise, [[None None]] is returned.
   *
   * If the key is currently whited out (i.e. it was defined
   * in a lower level of the map and was removed) then it will
   * be un-whited out and added at this level.
   *
   * @param key a key
   * @param value the value to associate with the key
   * @return an [[scala.Option Option]] containing the previous
   *         value associated with that key, or [[None None]]
   *         if the key was undefined
   */
  override def put(key: K, value: V): Option[V] = {
    if (whiteouts contains key) whiteouts -= key
    back.put(key, value)
  }

  /**
   * Removes a binding from the map and returns the value
   * corresponding to the given key.
   *
   * If the removed value exists in a lower level of the table,
   * it will be whited out at this level. This means that the entry
   * will be 'removed' at this level and this table will not provide
   * access to it, but the mapping will still exist in the level where
   * it was defined. Note that the key will not be returned if it is
   * defined in a lower level of the table.
   *
   * @param  key the key to remove.
   * @return a [[scala.Option Option]] containing the value of the
   *         key, or [[None None]] if it is undefined.
   */
  override def remove(key: K): Option[V] = back remove key orElse {
    parent flatMap (_ get key) map {(v) => whiteouts += key; v}
  }

}
