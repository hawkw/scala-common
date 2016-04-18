package me.hawkweisman.collection.mutable

import org.scalactic.Requirements

import scala.annotation.tailrec
import scala.collection.{AbstractMap, Map, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.{specialized => sp}

/**
  * Created by hawk on 4/18/16.
  */
class BinaryHeap[V, @sp(Int, Float, Double) P](implicit ord: Ordering[P])
extends AbstractMap[P, V]
  with Requirements {

  import Ordering.Implicits._

  private[this] case class Pair(priority: P, value: V)

  private[this] implicit val pairOrdering = new Ordering[Pair] {
    override def compare(x: Pair, y: Pair): Int
      = ord.compare(x.priority, y.priority)
  }

  private[this] implicit def pair2tuple(p: Pair): (P, V)
    = (p.priority, p.value)

  private[this] val heap = ArrayBuffer[Pair]()

  private[this] val priorityIdxs = mutable.Map[P, Int]()

  @inline override final def size: Int = heap.size
  @inline override final def isEmpty: Boolean = size == 0

  override def get(key: P): Option[V]
    = priorityIdxs get key map { idx => heap(idx).value }

  override def +[B1 >: V](kv: (P, B1)): Map[P, B1] = ???

  override def iterator: Iterator[(P, V)] = ???

  override def -(key: P): Map[P, V] = ???

  def push(value: V, priority: P)
    = if (priorityIdxs contains priority) update (priority, value)
      else {
        heap += Pair(priority, value)
        val idx = size - 1
        priorityIdxs += priority -> idx
        bubbleUp(idx)
      }

  def update(priority: P, value: V): Unit
    = priorityIdxs get priority match {
        case Some(i) if heap(i).priority != priority =>
          val newPair = Pair(priority, value)
          heap update (i, newPair)

          if (i > 0 && newPair < heap((i - 1) / 2)) bubbleUp(i)
          else bubbleDown(i)
        case None =>
          throw new IllegalArgumentException( "Cannot update priority " +
                                             s"$priority as it is not present")
  }

  @tailrec private[this] def bubbleUp(i: Int): Unit = {
    val j = (i - 1) / 2
    val (current, parent) = (heap(i), heap(j))
    if (i != 0 && current < parent) {
      heap update (j, current)
      heap update (i, parent)
      priorityIdxs += ( current.priority -> j
              , parent.priority -> i )
      bubbleUp(j)
    }
  }

  @tailrec private[this] def bubbleDown(i: Int): Unit = ???
}
