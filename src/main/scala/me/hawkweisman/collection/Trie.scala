// package me.hawkweisman
// package collection
//
// import scala.collection.AbstractIterator
// import scala.collection.immutable.{DefaultMap, AbstractMap}
//
// trait TrieLike[E, V] {
//   protected val value: Option[V]
//   protected def children: Map[E, SelfTypeType]
//
//   def get(key: ): Option[V] = key match {
//     case Seq() => value
//     case k +: ks => for {
//       child   <- children get k
//       result  <- child get ks
//     } yield result
//   }
//
// }
//
// case class TrieNode[K <% Seq[E], E, V](
//   override val value: Option[V],
//   override var children: Map[E, TrieNode[K,E,V]]
// ) extends TrieLike[E,V]
//   with AbstractMap[K, V]
//   with DefaultMap[K, V]
