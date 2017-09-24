package ru.yudnikov.pulkovo

import ru.yudnikov.pulkovo.json.{Json, MySerializer}

import scala.annotation.tailrec
import Implicits._
import ru.yudnikov.pulkovo.tree.{CompleteNode, Node, NonCompleteNode}

import scala.collection.parallel.{ParMap, ParSeq}
import scala.util.Random

object MyApp extends App {

  lazy val r = new Random()
  lazy val randomInput = List.fill(10000)(Vector.fill(4)(if (r.nextInt(10) >= 5) None else Some(s"${r.nextInt(1000)}"))).par
  lazy val inputBig = List.fill(100)(input).flatten.par

  val input = Json.extract[List[Vector[Option[String]]]](args(0), MySerializer).par

  val lengths = input.map(_.length).distinct

  assume(lengths.length == 1)

  implicit val length: Int = lengths.head

  val mapped = input.distinct.map(row => row.toInt -> row)
  val grouped: ParMap[Int, ParSeq[Vector[Option[String]]]] =
    mapped.groupBy(_._1).map(t => t._1 -> t._2.map(_._2))
  val targetSum = Math.pow(2, length).toInt - 1
  val keys = grouped.keys.toList

  type Stair[T] = (T, List[T])

  @tailrec
  def getStairs[T](list: List[T], acc: List[(T, List[T])] = Nil): List[Stair[T]] = {
    if (list.nonEmpty)
      getStairs(list.tail, (list.head -> list.tail) :: acc)
    else
      acc
  }

  def splitKeys(list: List[Int]): (List[CompleteNode], List[NonCompleteNode]) = {
    val resTuple = list map {
      case x if x == targetSum =>
        CompleteNode(targetSum, isRoot = true)
      case x =>
        NonCompleteNode(x, isRoot = true)
    } partition {
      _.isInstanceOf[CompleteNode]
    }
    resTuple._1.asInstanceOf[List[CompleteNode]] -> resTuple._2.asInstanceOf[List[NonCompleteNode]]
  }

  def splitNodes(nodes: List[Node]): (List[CompleteNode], List[NonCompleteNode]) = {
    val stairs = getStairs[Node](nodes)
    val resTuple = stairs.flatMap { stair =>
      stair._2.map { r =>
        stair._1 -> r
      }
    } collect {
      case t if Node.isValidPair(t) =>
        t._1.value + t._2.value match {
          case x if x == targetSum =>
            CompleteNode(x, List(t._1, t._2))
          case x if x < targetSum =>
            NonCompleteNode(x, List(t._1, t._2))
        }
    } partition {
      _.isInstanceOf[CompleteNode]
    }
    resTuple._1.asInstanceOf[List[CompleteNode]] -> resTuple._2.asInstanceOf[List[NonCompleteNode]]
  }

  @tailrec
  def solve(completeNodes: List[CompleteNode],
            nonCompleteNodes: List[NonCompleteNode],
            prevNonCompleteLength: Int = 0): List[CompleteNode] = {
    val currentSplit = splitNodes(nonCompleteNodes)
    val currentSplitMerged = Node.merge(currentSplit._1, currentSplit._2)
    if (prevNonCompleteLength == nonCompleteNodes.length) {
      println(s"finished:")
      completeNodes.foreach(_.print())
      completeNodes
    } else {
      solve(completeNodes.union(currentSplitMerged._1).distinct,
        nonCompleteNodes.union(currentSplitMerged._2).distinct,
        nonCompleteNodes.length)
    }
  }

  val split = splitKeys(keys)
  val res = solve(split._1, split._2).par
  val out = res.par.flatMap {
    _.combine(grouped)
  }.distinct

  Json.write[List[Vector[Option[String]]]](out.toList, "output", MySerializer)

}
