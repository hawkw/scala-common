package me.hawkweisman
package collection.mutable

import me.hawkweisman.collection.ForkTable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection

/**
 * Created by hawk on 6/29/15.
 */
class ForkHashMap[K,V](
  protected var _parent: Option[ForkHashMap[K,V]] = None,
  protected var _children: Seq[ForkHashMap[K,V]] = Seq()
  ) extends ForkTable[K,V] {

  override type SelfType = ForkHashMap[K,V]
  protected[this] var back: Map[K,V] = Map()
  protected[this] var whiteouts: Set[K] = Set()

  /**
   * Change the parent corresponding to this scope.
   * @param nParent the new parent
   * @throws IllegalArgumentException if the specified parent was invalid
   */
  override def reparent(nParent: SelfType): SelfType
    = { require(nParent != this, "Scope cannot mount itself as parent!")
        _parent foreach ( _ removeChild this )
        nParent addChild this
        _parent = Some(nParent)
        this
      }

  override def freeze(): SelfType
    = { parent foreach { oldParent ⇒
        _parent = None
        this.back ++= oldParent.iterator withFilter {
          case((key,_)) ⇒ !back.contains(key) && !whiteouts.contains(key)
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
  override def fork(): SelfType
    = { val c = new SelfType(_parent = Some(this))
        _children = _children :+ c
        c
      }

  override protected def addChild(other: SelfType): SelfType
    = { _children = _children :+ other; this }

  override protected def removeChild(other: SelfType): SelfType
    = { _children = _children.filter({other != _}); this }

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
  override def put(key: K, value: V): Option[V]
    = { if (whiteouts contains key) whiteouts -= key
        val oldV = back get key
        back = back updated (key, value)
        oldV
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
   *         key, or [[scala.None None]] if it is undefined.
   */
  override def remove(key: K): Option[V]
    = back get key map { v ⇒
        back -= key; v
      } orElse {
        _parent flatMap { _ get key } map {
          v ⇒ whiteouts += key; v
        }
      }

  @tailrec override final def chainContains(key: K): Boolean
    = back contains key match {
        case true                            ⇒ true
        case false if whiteouts contains key ⇒ false
        case false                           ⇒ _parent match {
          case None        ⇒ false
          case Some(thing) ⇒ thing.chainContains(key)
        }
      }

  @tailrec override final def chainExists(p: ((K, V)) ⇒ Boolean): Boolean
    = back exists p match {
        case true  ⇒ true // this method could look much simpler
                          // were it not for `tailrec`
        case false ⇒ _parent match {
          case None        ⇒ false
          case Some(thing) ⇒ thing.chainExists(p)
        }
      }
      
  @tailrec final override def get(key: K): Option[V] =
    whiteouts contains key match {
      case true  ⇒ None
      case false ⇒ back get key match {
        case value: Some[V] ⇒ value
        case None           ⇒ parent match {
          case None         ⇒ None
          case Some(thing)  ⇒ thing.get(key)
        }
      }
    }
}
