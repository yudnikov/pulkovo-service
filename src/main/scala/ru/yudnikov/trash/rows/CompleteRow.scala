package ru.yudnikov.trash.rows

case class CompleteRow(children: List[Row] = Nil, isRoot: Boolean = false)(implicit length: Int) extends Row {

  override val isComplete = true

  override val value: Vector[Boolean] = Vector.fill(length)(true)

  override def withChildren(children: List[Row]): CompleteRow = copy(children = children)

}
