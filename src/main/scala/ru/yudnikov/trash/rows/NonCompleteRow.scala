package ru.yudnikov.trash.rows

case class NonCompleteRow(value: Vector[Boolean], children: List[Row] = Nil, isRoot: Boolean = false) extends Row {

  override val isComplete = false

  override def withChildren(children: List[Row]): NonCompleteRow = copy(children = children)

}
