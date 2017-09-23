package ru.yudnikov.pulkovo.rows

case class NonCompleteRow(vector: Vector[Boolean], children: List[Row] = Nil) extends Row {

  override def withChildren(children: List[Row]): NonCompleteRow = copy(children = children)

}
