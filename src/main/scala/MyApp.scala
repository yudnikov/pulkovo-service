import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object MyApp extends App {


  val input: List[Vector[Option[String]]] =
    List(
      Vector(Some("a1"), Some("a2"), Some("a3"), Some("a4")),
      Vector(Some("b1"), None, None, Some("b4")),
      Vector(None, Some("c2"), Some("c3"), None),
      Vector(Some("d1"), None, None, Some("d4")),
      Vector(None, Some("e2"), Some("e3"), None),
      Vector(None, Some("f2"), Some("f3"), Some("f4")),
      Vector(Some("g1"), None, None, None),
      Vector(Some("h1"), None, None, None),
      Vector(None, None, None, Some("i4")),
      Vector(Some("j1"), Some("j2"), Some("j3"), None),
      Vector(None, Some("j2"), None, None),
      Vector(None, Some("k2"), None, None),
      Vector(None, Some("l2"), None, None)
    )

  implicit val length = input.map(_.length).head

  val bool = input.map(t => (t.map(_.isDefined), t))
  val grouped = bool.groupBy(_._1)
  val keys = grouped.keys.toList

  trait Key {
    val vector: Vector[Boolean]
    val children: List[Key]
    def flatten: List[Key] = {
      if (children.nonEmpty) {
        children.flatMap(_.flatten)
      } else
        this :: Nil
    }
    def merge(key: Key): Key = {
      if (vector == key.vector)
        withChildren(children.union(key.children))
      else
        withChildren(children.map(_.merge(key)))
    }
    def withChildren(children: List[Key]): Key
  }

  case class CompleteKey(val children: List[Key] = Nil)(implicit length: Int) extends Key {

    override val vector: Vector[Boolean] = Vector.fill(length)(true)

    override def withChildren(children: List[Key]): Key = copy(children = children)

  }

  case class NonCompleteKey(val vector: Vector[Boolean], val children: List[Key] = Nil) extends Key {

    override def withChildren(children: List[Key]): Key = copy(children = children)

  }

  val keys0 = keys.partition(_.forall(_ == true))
  val completeKeys0 = keys0._1.map(_ => new CompleteKey())
  val uncompleteKeys = keys0._2.map(new NonCompleteKey(_))

  @tailrec
  def stairs[T](list: List[T], acc: List[(T, List[T])] = Nil): List[(T, List[T])] = {
    if (list.nonEmpty)
      stairs(list.tail, (list.head -> list.tail) :: acc)
    else
      acc
  }

  val s0 = stairs(uncompleteKeys)

  val res: (List[CompleteKey], List[NonCompleteKey]) = {
    s0.flatMap { stair =>
      stair._2.map { k =>
        stair._1 -> k
      }
    } collect {
      case t if isComplete(t) => {
        new CompleteKey(List(t._1, t._2))
      }
      case t if nonComplete(t) => {
        println(t)
        val res = new NonCompleteKey(simplify(t._1.vector, t._2.vector), List(t._1, t._2))
        res
      }
    } partition {
      _.isInstanceOf[CompleteKey]
    }
  }.asInstanceOf[(List[CompleteKey], List[NonCompleteKey])]

  val completeKeys1 = completeKeys0.union(res._1).distinct
  val x = res._2.partition(k => completeKeys0.flatMap(_.flatten).exists(_.vector == k.vector))

  val m1 = res._1(2)
  val m2 = res._2(3)
  val m = m1.merge(m2)

  null

  def merge[T <: Key](to: List[Key], from: List[Key]): List[T] = {

    for (t <- to) {
      var buf = t
      for (f <- from) {
        buf = buf.merge(f)
      }
    }
  }

  def simplify(a: Vector[Boolean], b: Vector[Boolean]): Vector[Boolean] = {
    val ab = a.zip(b)
    ab.map(bb => bb._1 | bb._2)
  }

  null

  def isComplete(t: (NonCompleteKey, NonCompleteKey)): Boolean = isComplete(t._1, t._2)

  def isComplete(a: NonCompleteKey, b: NonCompleteKey): Boolean = {
    val ab = a.vector.zip(b.vector)
    val valid = !ab.exists(bb => bb._1 & bb._2)
    valid && ab.forall(bb => bb._1 | bb._2)
  }

  def nonComplete(t: (NonCompleteKey, NonCompleteKey)): Boolean = nonComplete(t._1, t._2)

  def nonComplete(a: NonCompleteKey, b: NonCompleteKey): Boolean = {
    val ab = a.vector.zip(b.vector)
    val valid = !ab.exists(bb => bb._1 & bb._2)
    valid && ab.exists(bb => !bb._1 & !bb._2)
  }

}
