package me.hawkweisman.util
package collection

import scala.collection.immutable.{DefaultMap, AbstractMap}

trait TrieLike[K <: Seq[E], E, V]
  extends AbstractMap[K, V]
  with DefaultMap[K,V] {

  type SelfTypeType <: TrieLike[K,E,V]

  protected val value: Option[V]
  protected def children: Map[E, SelfTypeType]

  override def get(key: K): Option[V] = key match {
    case Seq() => value
    case Seq(k: E, ks @ _) => for {
      child   <- children get k
      result  <- child get ks
    } yield result
  }

  override def iterator: Iterator[(K,V)]

}

case class TrieNode[K <% Seq[E], E, V] (
  value: Option[V],
  children: Map[E, TrieNode[K,E,V]]
  ) extends TrieLike[K,E,V]
