package ru.yudnikov.pulkovo

import org.json4s.{CustomSerializer, DefaultFormats, Formats, JValue, Serializer, TypeInfo}
import org.json4s.jackson.{JsonMethods, Serialization}
import ru.yudnikov.pulkovo.rows.{CompleteRow, NonCompleteRow, Row}

import scala.annotation.tailrec
import scala.io.Source

object MyApp extends App {

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
        CompleteRow()
      case v if !isComplete(v) =>
        NonCompleteRow(v)
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

  val bool = in.map(row => (row.map(_.isDefined), row))
  val groups = bool.groupBy(_._1)
  val keys = groups.keys.toList

  val s0 = splitVectors(keys)

  def solve(completeRows: List[CompleteRow], nonCompleteRows: List[NonCompleteRow], prevNonComplete: Int = 0): Unit = {
    val completeRoots = completeRows.flatMap(_.roots)
    val currentSplit = splitRows(nonCompleteRows)
    val currentNonCompleteRoots = currentSplit._2.flatMap(_.roots).distinct.diff(completeRoots)
    if (currentNonCompleteRoots.isEmpty || currentNonCompleteRoots.length == prevNonComplete)
      println(s"solved!")
    else
      solve(completeRows.union(currentSplit._1).distinct, nonCompleteRows.union(currentSplit._2).distinct, currentNonCompleteRoots.length)
  }

  solve(s0._1, s0._2)

}
