import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Trash extends App {


  /*val input: List[Vector[Option[String]]] =
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
    )*/

  val input: List[Vector[Option[String]]] =
    List(
      //Vector(Some("a1"), Some("a2"), Some("a3"), Some("a4")),
      Vector(Some("b1"), None, None, None),
      Vector(None, Some("c2"), None, None),
      Vector(None, None, Some("d3"), None),
      Vector(None, None, None, Some("e4")),
      Vector(Some("f1"), None, None, None),
      Vector(Some("g1"), Some("g2"), None, None),
      Vector(None, None, Some("g1"), Some("g2"))
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
    def print(level: Int = 0): Unit = {
      println("\t"*level + vector)
      children.foreach(_.print(level + 1))
    }
    def filter(p: Key => Boolean): List[Key] = {
      if (p(this))
        List(this) ::: children.flatMap(_.filter(p))
      else
        children.flatMap(_.filter(p))
    }
    lazy val roots: Set[Key] = this.filter(_.children.isEmpty).toSet
  }

  case class CompleteKey(children: List[Key] = Nil)(implicit length: Int) extends Key {

    override val vector: Vector[Boolean] = Vector.fill(length)(true)

    override def withChildren(children: List[Key]): Key = copy(children = children)

  }

  case class NonCompleteKey(vector: Vector[Boolean], children: List[Key] = Nil) extends Key {

    override def withChildren(children: List[Key]): Key = copy(children = children)

  }

  val keys0 = keys.partition(_.forall(_ == true))
  val completeKeys0 = keys0._1.map(_ => CompleteKey())
  val nonCompleteKeys0 = keys0._2.map(NonCompleteKey(_))

  @tailrec
  def stairs[T](list: List[T], acc: List[(T, List[T])] = Nil): List[(T, List[T])] = {
    if (list.nonEmpty)
      stairs(list.tail, (list.head -> list.tail) :: acc)
    else
      acc
  }

  def split(stairs: List[(Key, List[Key])]): (List[CompleteKey], List[NonCompleteKey]) = {
    stairs.flatMap { stair =>
      stair._2.map { k =>
        stair._1 -> k
      }
    } collect {
      case t if isComplete(t) =>
        CompleteKey(List(t._1, t._2))
      case t if nonComplete(t) =>
        NonCompleteKey(simplify(t._1.vector, t._2.vector), List(t._1, t._2))
    } partition {
      _.isInstanceOf[CompleteKey]
    }
  }.asInstanceOf[(List[CompleteKey], List[NonCompleteKey])]

  val res0 = split(stairs(nonCompleteKeys0))

  val completeKeys1 = completeKeys0.union(res0._1).distinct
  val x = res0._2.partition(k => completeKeys0.flatMap(_.flatten).exists(_.vector == k.vector))

  val mrg = merge(res0._1, res0._2)

  val heap = mrg._1.flatMap(_.flatten)

  val nonCompleteKeys1 = (nonCompleteKeys0 union res0._2).distinct.asInstanceOf[List[Key]]

  val res1 = split(stairs(nonCompleteKeys1))

  val nonCompleteKeys2 = (nonCompleteKeys1 union res1._2).distinct

  val res2 = split(stairs(nonCompleteKeys2))

  val nonCompleteKeys3 = (nonCompleteKeys2 union res2._2).distinct

  val res3 = split(stairs(nonCompleteKeys3))

  val c3 = res3._1.map(ck => ck.flatten.map(_.vector)).distinct

  val nonCompleteKeys4 = (nonCompleteKeys3 union res3._2).distinct

  val res4 = split(stairs(nonCompleteKeys4))

  val c4 = res4._1.map(ck => ck.flatten.map(_.vector)).distinct

  val rootsComplete = res4._1.map(k => k.roots -> k).toMap
  val rootsNonComplete = res4._2.map(k => k.roots -> k).toMap

  null

  def merge(to: List[CompleteKey], from: List[NonCompleteKey]): (Set[CompleteKey], Set[NonCompleteKey]) = {
    var completeKeys = mutable.ArrayBuffer[CompleteKey]()
    for (key <- to) {
      var current = key
      for (f <- from) {
        current = current.merge(f).asInstanceOf[CompleteKey]
      }
      completeKeys += current
    }
    val heap = completeKeys.flatMap(_.flatten).map(_.vector)
    completeKeys.toSet -> from.filter(k => !heap.contains(k.vector)).toSet
  }

  def simplify(a: Vector[Boolean], b: Vector[Boolean]): Vector[Boolean] = a.zip(b).map(bb => bb._1 | bb._2)

  def isValidTuple(t: (Key, Key)): (Boolean, Vector[(Boolean, Boolean)]) = {
    val ab = t._1.vector.zip(t._2.vector)
    (!ab.exists(bb => bb._1 & bb._2), ab)
  }

  def isComplete(t: (Key, Key)): Boolean = {
    val vt = isValidTuple(t)
    vt._1 && vt._2.forall(bb => bb._1 | bb._2)
  }

  def nonComplete(t: (Key, Key)): Boolean = {
    val vt = isValidTuple(t)
    vt._1 && vt._2.exists(bb => !bb._1 & !bb._2)
  }

}
