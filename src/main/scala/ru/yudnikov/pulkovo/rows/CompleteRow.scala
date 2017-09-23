package ru.yudnikov.pulkovo.rows

case class CompleteRow(children: List[Row] = Nil)(implicit length: Int) extends Row {

  override val vector: Vector[Boolean] = Vector.fill(length)(true)

  override def withChildren(children: List[Row]): CompleteRow = copy(children = children)

}
