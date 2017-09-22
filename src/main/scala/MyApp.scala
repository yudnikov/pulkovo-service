import scala.annotation.tailrec

object MyApp extends App {

  type T = Vector[Option[String]]

  val input: List[T] =
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

  val bool = input.map(t => (t.map(_.isDefined), t))
  val grouped = bool.groupBy(_._1)
  val keys = grouped.keys.toList

  

  @tailrec
  def stairs[T](list: List[T], acc: List[(T, List[T])] = Nil): List[(T, List[T])] = {
    if (list.tail.nonEmpty)
      stairs(list.tail, (list.head -> list.tail) :: acc)
    else
      acc
  }

  val x = stairs(keys)

  def solveStairs[T](stairs: List[(T, List[T])], f: (T, T) => Boolean): List[(T, T)] = {
    stairs.map { stair =>
      stair._1 -> stair._2.filter {
        f(stair._1, _)
      }
    } filter {
      _._2.nonEmpty
    } flatMap { t =>
      t._2.map {
        _ -> t._1
      }
    }
  }

  val solved = solveStairs(x, isComplete)
  val semi = solveStairs(x, nonComplete)

  val zs = semi.map(t => simplify(t._1, t._2) -> t)

  def simplify(a: Vector[Boolean], b: Vector[Boolean]): Vector[Boolean] = {
    val ab = a.zip(b)
    ab.map(bb => bb._1 | bb._2)
  }

  null

  def isComplete: (Vector[Boolean], Vector[Boolean]) => Boolean = (a, b) => {
    val ab = a.zip(b)
    val valid = !ab.exists(bb => bb._1 & bb._2)
    valid && ab.forall(bb => bb._1 | bb._2)
  }

  def nonComplete: (Vector[Boolean], Vector[Boolean]) => Boolean = (a, b) => {
    val ab = a.zip(b)
    val valid = !ab.exists(bb => bb._1 & bb._2)
    valid && ab.exists(bb => !bb._1 & !bb._2)
  }

}
