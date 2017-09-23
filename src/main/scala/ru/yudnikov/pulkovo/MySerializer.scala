package ru.yudnikov.pulkovo

import org.json4s.{CustomSerializer, JValue}
import org.json4s.JsonAST.{JArray, JNull, JString}

object MySerializer extends CustomSerializer[List[Vector[Option[String]]]](_ => ( {
  case JArray(rows: List[JArray]) => {
    rows map {
      case JArray(columns: List[JValue]) => {
        columns map {
          case JString(s) =>
            Some(s)
          case _ =>
            None
        }
      }.toVector
      case _ =>
        Vector[Option[String]]()
    }
  }
  case _ =>
    Nil
}, {
  case rows: List[Vector[Option[String]]] => JArray(
    rows map {
      row: Vector[Option[String]] =>
        JArray(
          {
            row map {
              case Some(str) => JString(str)
              case _ => JNull
            }
          }.toList
        )
    }
  )
}))
