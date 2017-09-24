package ru.yudnikov.pulkovo

import ru.yudnikov.pulkovo.json.{Json, MySerializer}

import scala.annotation.tailrec

import Implicits._

object MyApp extends App {

  val input = Json.extract[List[Vector[Option[String]]]](args(0), MySerializer)
  val lengths = input.map(_.length).distinct

  assume(lengths.length == 1)

  implicit val length: Int = lengths.head

  val mapped = input.map(row => row.toInt -> row)
  val grouped = mapped.groupBy(_._1)
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

  //def split

  null

}
