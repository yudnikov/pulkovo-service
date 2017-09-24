package ru.yudnikov.trash

import org.json4s.jackson.{JsonMethods, Serialization}
import org.json4s.{DefaultFormats, Formats}
import ru.yudnikov.pulkovo.json.MySerializer
import ru.yudnikov.trash.rows.{CompleteRow, NonCompleteRow, Row}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object OldApp extends App {

  val source = Source.fromResource(s"${args(0)}.json")
  val json = JsonMethods.parse(source.reader())

  implicit val formats: Formats = DefaultFormats + MySerializer

  val in = json.extract[List[Vector[Option[String]]]]
  val out = Serialization.write(in)
  println(in)
  println(out)

  val lengths = in.map(_.length).distinct

  assume(lengths.length == 1)

  implicit val length: Int = lengths.head

  println(length)

  type Stair[T] = (T, List[T])

  @tailrec
  def getStairs[T](list: List[T], acc: List[(T, List[T])] = Nil): List[Stair[T]] = {
    if (list.nonEmpty)
      getStairs(list.tail, (list.head -> list.tail) :: acc)
    else
      acc
  }

  def splitVectors(vectors: List[Vector[Boolean]]): (List[CompleteRow], List[NonCompleteRow]) = {
    val resTuple = vectors collect {
      case v if isComplete(v) =>
        CompleteRow(isRoot = true)
      case v if !isComplete(v) =>
        NonCompleteRow(v, isRoot = true)
    } partition {
      _.isInstanceOf[CompleteRow]
    }
    resTuple._1.asInstanceOf[List[CompleteRow]] -> resTuple._2.asInstanceOf[List[NonCompleteRow]]
  }

  def splitRows(rows: List[Row]): (List[CompleteRow], List[NonCompleteRow]) = {
    val stairs: List[Stair[Row]] = getStairs(rows)
    val resTuple = stairs.flatMap { stair =>
      stair._2.map { r =>
        stair._1 -> r
      }
    } collect {
      case t if isComplete(t) =>
        CompleteRow(List(t._1, t._2))
      case t if nonComplete(t) =>
        NonCompleteRow(sum(t._1, t._2), List(t._1, t._2))
    } partition {
      _.isInstanceOf[CompleteRow]
    }
    resTuple._1.asInstanceOf[List[CompleteRow]] -> resTuple._2.asInstanceOf[List[NonCompleteRow]]
  }

  def sum(a: Vector[Boolean], b: Vector[Boolean]): Vector[Boolean] = a.zip(b).map(bb => bb._1 | bb._2)

  def isValidTuple(t: (Vector[Boolean], Vector[Boolean])): (Boolean, Vector[(Boolean, Boolean)]) = {
    val ab = t._1.zip(t._2)
    (!ab.exists(bb => bb._1 & bb._2), ab)
  }

  def isComplete(v: Vector[Boolean]): Boolean = !v.contains(false)

  def isComplete(t: (Vector[Boolean], Vector[Boolean])): Boolean = {
    val vt = isValidTuple(t)
    vt._1 && vt._2.forall(bb => bb._1 | bb._2)
  }

  def nonComplete(t: (Vector[Boolean], Vector[Boolean])): Boolean = {
    val vt = isValidTuple(t)
    vt._1 && vt._2.exists(bb => !bb._1 & !bb._2)
  }

  def merge[T1 <: Row, T2 <: Row](to: List[T1], from: List[T2]): (List[T1], List[T2]) = {
    var completeKeys = mutable.ArrayBuffer[T1]()
    for (key <- to) {
      var current = key
      for (f <- from) {
        current = current.merge(f).asInstanceOf[T1]
      }
      completeKeys += current
    }
    val heap = completeKeys.flatMap(_.flatten).map(_.value)
    completeKeys.toList -> from.filter(k => !heap.contains(k.value))
  }

  val mapped = in.map(row => (row.map(_.isDefined), row))
  val groups = mapped.groupBy(_._1)
  val keys = groups.keys.toList

  //null

  val s0 = splitVectors(keys)

  @tailrec
  def solve(completeRows: List[CompleteRow], nonCompleteRows: List[NonCompleteRow], prevNonComplete: Int = 0): Unit = {
    val currentSplit = splitRows(nonCompleteRows)
    val currentSplitMerged = merge(currentSplit._1, currentSplit._2)
    //if (completeRows.length == currentSplit._1.length && prevNonComplete == currentSplit._2.length) {
    if (prevNonComplete == nonCompleteRows.length) {
      println(s"finished")
      //currentSplitMerged._1.foreach(_.print())
      println(s"roots: \n" +
        s"${completeRows.flatMap(_.roots).distinct.mkString("\n")}")
      /*val x = merge(List(completeRows.head), completeRows.tail)
      x._1.head.print()*/
      completeRows.foreach(_.print())
    } else {
      solve(completeRows.union(currentSplitMerged._1).distinct, nonCompleteRows.union(currentSplitMerged._2).distinct, nonCompleteRows.length)
    }
  }

  solve(s0._1, s0._2)

}
