package ru.yudnikov.pulkovo.tree

import java.util.concurrent.ConcurrentHashMap

import ru.yudnikov.pulkovo.Implicits._
import ru.yudnikov.pulkovo.MyApp.length

import scala.collection.mutable
import scala.collection.parallel.{ParMap, ParSeq}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Node {

  val value: Int
  val vector: Vector[Boolean] = value.toBoolVector
  val children: List[Node]
  val isRoot: Boolean
  val isComplete: Boolean

  lazy val roots: Set[Node] = this.filter(_.isRoot).toSet

  def withChildren(children: List[Node]): Node

  def combine(grouped: ParMap[Int, ParSeq[Vector[Option[String]]]],
              parent: ParSeq[Vector[Option[String]]] = ParSeq.empty): List[Vector[Option[String]]] = {

    def join(a: Vector[Option[String]], b: Vector[Option[String]]): Vector[Option[String]] = {
      val res = a.zip(b).map { ab =>
        if (ab._1.isDefined) ab._1 else ab._2
      }
      res
    }

    val currentVectors = grouped.getOrElse(value, ParSeq.empty)

    def combine(a: Node, b: Node): List[Vector[Option[String]]] = {
      val ac = a.combine(grouped)
      val bc = b.combine(grouped)
      val joined = for {
        c1 <- ac
        c2 <- bc
      } yield join(c1, c2)
      joined
    }

    val key = List(value) ::: children.map(_.value)

    Node.cache.get(key) match {
      case res: List[Vector[Option[String]]] =>
        res
      case _ =>
        val res = children match {
          case Nil =>
            currentVectors.toList
          case List(a, b) =>
            val res = currentVectors.union {
              combine(a, b).par
            }.toList
            res
          case list: List[Node] if list.length % 2 == 0 =>
            list.grouped(2).flatMap {
              case List(a, b) =>
                val res = currentVectors.union {
                  combine(a, b).par
                }.toList
                res
            }.toList.distinct
        }
        Node.cache.put(key, res)
        res
    }


  }

  /*def combineFuture(grouped: ParMap[Int, ParSeq[Vector[Option[String]]]],
                    parent: ParSeq[Vector[Option[String]]] = ParSeq.empty): Future[List[Vector[Option[String]]]] = {

    def join(a: Vector[Option[String]], b: Vector[Option[String]]): Vector[Option[String]] = {
      val res = a.zip(b).map { ab =>
        if (ab._1.isDefined) ab._1 else ab._2
      }
      res
    }

    val currentVectors = grouped.getOrElse(value, ParSeq.empty)

    def combine(a: Node, b: Node): Future[ParSeq[Vector[Option[String]]]] = {
      val f1 = a.combineFuture(grouped)
      val f2 = b.combineFuture(grouped)
      f1.zip(f2).map { t =>
        val joined = for {
          c1 <- t._1
          c2 <- t._2
        } yield join(c1, c2)
        joined.par
      }
    }

    children match {
      case Nil =>
        Future(currentVectors.toList)
      case List(a, b) =>
        combine(a, b).map(_.toList)
    }

  }*/

  def filter(p: Node => Boolean): List[Node] = {
    if (p(this))
      List(this) ::: children.flatMap(_.filter(p))
    else
      children.flatMap(_.filter(p))
  }

  def flatten: List[Node] = {
    if (children.nonEmpty) {
      children.flatMap(_.flatten)
    } else
      this :: Nil
  }

  def merge(row: Node): Node = {
    if ((!isRoot && !isComplete || isRoot && isComplete)
      && value == row.value
      && children != row.children) {
      val c = children.union(row.children)
      withChildren(c)
    } else
      withChildren(children.map(_.merge(row)))
  }

  def print(level: Int = 0): Unit = {
    println("\t" * level + vector.map {
      case true => "x"
      case false => "_"
    }.mkString(" "))
    children.foreach(_.print(level + 1))
  }

}

object Node {

  val cache: ConcurrentHashMap[List[Int], List[Vector[Option[String]]]] = new ConcurrentHashMap()

  def isValidPair(t: (Node, Node)): Boolean = {
    val ab = t._1.vector.zip(t._2.vector)
    !ab.exists(bb => bb._1 & bb._2)
  }

  def merge[T1 <: Node, T2 <: Node](to: List[T1], from: List[T2]): (List[T1], List[T2]) = {
    var completeKeys = mutable.ArrayBuffer[T1]()
    for (key <- to) {
      var current = key
      for (f <- from) {
        current = current.merge(f).asInstanceOf[T1]
      }
      completeKeys += current
    }
    val heap = completeKeys.toList.par.flatMap(_.flatten).map(_.value).seq
    completeKeys.toList -> from.filter(k => !heap.contains(k.value))
  }

}
