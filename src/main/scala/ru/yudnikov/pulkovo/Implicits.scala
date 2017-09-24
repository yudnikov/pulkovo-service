package ru.yudnikov.pulkovo

object Implicits {

  implicit class OptVectorToInt(vector: Vector[Option[_]]) {

    def toInt: Int = Integer.parseInt(vector.map {
      case opt if opt.isDefined => '1'
      case _ => '0'
    }.mkString, 2)

  }

  implicit class IntToBoolVector(int: Int) {

    def toBoolVector(implicit length: Int): Vector[Boolean] = {
      val tail = Integer.toBinaryString(int).map {
        case '1' => true
        case _ => false
      }.toList
      val head = List.fill(length - tail.length)(false)
      (head ::: tail).toVector
    }

  }

}
