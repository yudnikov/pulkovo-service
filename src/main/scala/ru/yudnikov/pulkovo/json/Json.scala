package ru.yudnikov.pulkovo.json

import java.io.{File, FileOutputStream, PrintWriter}

import org.json4s.{DefaultFormats, Formats, Serializer}
import org.json4s.jackson.{JsonMethods, Serialization}

import scala.io.Source

object Json {

  def extract[T](fileName: String, serializer: Serializer[T])(implicit m: Manifest[T]): T = {
    val source = Source.fromResource(s"$fileName.json")
    val json = JsonMethods.parse(source.reader())
    implicit val formats: Formats = DefaultFormats + serializer
    json.extract[T]
  }

  def write[T <: AnyRef](value: T, fileName: String, serializer: Serializer[T])(implicit m: Manifest[T]): Unit = {
    implicit val formats: Formats = DefaultFormats + serializer
    val json = Serialization.writePretty[T](value)
    val pw = new PrintWriter(new File(s"$fileName.json"))
    pw.write(json)
    pw.close()
  }

}
