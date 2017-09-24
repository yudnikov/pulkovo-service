package ru.yudnikov.pulkovo.json

import org.json4s.{DefaultFormats, Formats, Serializer}
import org.json4s.jackson.JsonMethods

import scala.io.Source
import scala.reflect.ClassTag

object Json {

  def extract[T](fileName: String, serializer: Serializer[T])(implicit m: Manifest[T]): T = {
    val source = Source.fromResource(s"$fileName.json")
    val json = JsonMethods.parse(source.reader())
    implicit val formats: Formats = DefaultFormats + serializer
    json.extract[T]
  }

}
