package ru.yudnikov.pulkovo.tree

case class NonCompleteNode(value: Int, children: List[Node] = Nil, isRoot: Boolean = false) extends Node {

  override val isComplete = false

  override def withChildren(children: List[Node]): NonCompleteNode = copy(children = children)

  assume(children == Nil || (isComplete && children.length == 2) || !isComplete, s"$children")

}
