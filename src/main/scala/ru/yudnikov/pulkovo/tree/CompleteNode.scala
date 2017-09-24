package ru.yudnikov.pulkovo.tree

case class CompleteNode(value: Int,
                        children: List[Node] = Nil,
                        isRoot: Boolean = false)
  extends Node {

  override val isComplete = true

  override def withChildren(children: List[Node]): CompleteNode = copy(children = children)

  assume(children == Nil || children.length == 2)

}
