package me.hawkweisman
package collection

import scala.collection.{ AbstractMap, DefaultMap }

trait ForkTable[K,V]
extends AbstractMap[K,V]
  with DefaultMap[K,V] {

  type SelfType <: ForkTable[K,V]

  protected def _parent: Option[SelfType]
  protected[this] def _children: Seq[SelfType]
  protected[this] def whiteouts: Set[K]
  protected[this] def back: Map[K,V]

  /**
   * Inserts a key-value pair from the map.
   *
   * If the key already had a value present in the map, that
   * value is returned. Otherwise, [[scala.None None]] is returned.
   *
   * If the key is currently whited out (i.e. it was defined
   * in a lower level of the map and was removed) then it will
   * be un-whited out and added at this level.
   *
   * @param key a key
   * @param value the value to associate with the key
   * @return an [[scala.Option Option]] containing the previous
   *         value associated with that key, or [[scala.None None]]
   *         if the key was undefined
   */
  def put(key: K, value: V): Option[V]

  /**
   * @return true if this is the root-level, false if it is not
   */
  def root: Boolean
    = _parent.isEmpty
  /**
   * @return true if this is a bottom-level leaf, false if it is not
   */
  def leaf: Boolean
    = _parent.isDefined && _children.isEmpty
  /**
   * @return an [[scala.Option Option]] containing a reference to
   *         the parent table, or [[scala.None None]] if this is
   *         the root level of the tree
   */
  def parent: Option[SelfType]
    = _parent

  /**
   * @return a sequence of this level's child ForkTables.
   */
  def children: Seq[ForkTable[K,V]]
    = _children

  protected def removeChild(other: SelfType): SelfType

  protected def addChild(other: SelfType): SelfType

  /**
   * @return the number of keys defined in this level plus all previous levels
   */
  def chainSize: Int
    = size + (_parent map (_.chainSize) getOrElse 0)

  /**
   * Change the parent corresponding to this scope.
   * @param nParent the new parent
   * @throws IllegalArgumentException if the specified parent was invalid
   */
  @throws[IllegalArgumentException]("if the specified parent was invalid")
  def reparent(nParent: SelfType): SelfType

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
   *         key, or [[scala.None None]] if it is undefined.
   */
  def remove(key: K): Option[V]

  def freeze(): SelfType

  /** @return the number of entries in this level over the table.
   */
  override def size: Int
    = back.size

  /** @return an Iterator over all of the (key, value) pairs in the tree.
    */
  override def iterator: Iterator[(K,V)]
    = parent match {
      case None         ⇒ back.iterator // TODO: make tail-recursive?
      case Some(parent) ⇒ back.iterator ++ parent.iterator.withFilter({
        case ((key,_))  ⇒ !back.contains(key) && !whiteouts.contains(key)
      })
    }

  /**
   * Returns true if this contains the selected key OR if any of its' parents
   * contains the key
   * @param key the key to search for
   * @return true if this or any of its' parents contains the selected key.
   */
  def chainContains(key: K): Boolean

  /**
   * @param  key the key to look up
   * @return true if this level contains a binding for the given key, false otherwise
   */
  override def contains(key: K): Boolean
    = back contains key

  /**
   * Search this level for a (key, value) pair matching a predicate.
   *
   * @param  p the predicate to search for
   * @return true if there exists a pair defined at this level for
   *         which the predicate holds, false otherwise.
   */
  override def exists(p: ((K, V)) ⇒ Boolean): Boolean
    = back exists p

  /**
   * Search the whole chain down from this level
   * for a (key, value) pair matching a predicate.
   *
   * @param  p the predicate to search for
   * @return true if there exists a pair for which the
   *         predicate holds, false otherwise.
   */
 def chainExists(p: ((K, V)) ⇒ Boolean): Boolean

  /**
   * Look up the given key
   * @param  key the key to look up
   * @return the value bound to that key.
   */
  override def apply(key: K): V
    = back(key)

  /**
   * Forks this table, returning a new `ForkTable[K,V]`.
   *
   * This level of the table will be set as the child's
   * parent. The child will be created with an empty backing
   * [[scala.collection.mutable.HashMap HashMap]] and no keys whited out.
   *
   * @return a new child of this scope
   */
  def fork(): ForkTable[K, V]

 /**
  * @return a String representation of this ForkTable
  */
  override def toString(): String
  = this.prettyPrint(0)

  /**
   * Helper method for printing indented levels of a ForkTable
   *
   * @param indentLevel the level to indent to
   * @return a String representing this table indented at the specified level
   */
  def prettyPrint(indentLevel: Int): String = (" " * indentLevel) + this.keys.foldLeft(""){
    (acc, key) ⇒
      acc + "\n" + (" " * indentLevel) + s"$key ==> ${this.get(key).getOrElse("")}"
  }
}
