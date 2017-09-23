package ru.yudnikov.pulkovo.rows

trait Row {

  val vector: Vector[Boolean]
  val children: List[Row]

  lazy val roots: Set[Row] = this.filter(_.children.isEmpty).toSet

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
    if (vector == row.vector)
      withChildren(children.union(row.children))
    else
      withChildren(children.map(_.merge(row)))
  }

  def print(level: Int = 0): Unit = {
    println("\t" * level + vector)
    children.foreach(_.print(level + 1))
  }
}

object Row {

  // some implicit magic...
  implicit def toVector(row: Row): Vector[Boolean] = row.vector

  implicit def toTuple(t: (Row, Row)): (Vector[Boolean], Vector[Boolean]) = t._1.vector -> t._2.vector

}
