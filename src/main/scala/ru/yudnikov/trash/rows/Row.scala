package ru.yudnikov.trash.rows

trait Row {

  val value: Vector[Boolean]
  val children: List[Row]
  val isRoot: Boolean
  val isComplete: Boolean

  lazy val roots: Set[Row] = this.filter(_.isRoot).toSet

  def withChildren(children: List[Row]): Row

  def filter(p: Row => Boolean): List[Row] = {
    if (p(this))
      List(this) ::: children.flatMap(_.filter(p))
    else
      children.flatMap(_.filter(p))
  }

  def flatten: List[Row] = {
    if (children.nonEmpty) {
      children.flatMap(_.flatten)
    } else
      this :: Nil
  }

  def merge(row: Row): Row = {
    if ((!isRoot && !isComplete || isRoot && isComplete)
        && value == row.value
        && children != row.children) {
      val c = children.union(row.children)
      withChildren(c)
    } else
      withChildren(children.map(_.merge(row)))
  }

  def print(level: Int = 0): Unit = {
    println("\t" * level + value.map {
      case true => "x"
      case false => "_"
    }.mkString(" "))
    children.foreach(_.print(level + 1))
  }

  //override def toString = print()

}

object Row {

  // some implicit magic...
  implicit def toVector(row: Row): Vector[Boolean] = row.value

  implicit def toTuple(t: (Row, Row)): (Vector[Boolean], Vector[Boolean]) = t._1.value -> t._2.value

}
