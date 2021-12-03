import scala.io.Source
import scala.util.Using.Manager.Resource
import scala.language.reflectiveCalls
import scala.io.BufferedSource
import scala.compiletime.ops.boolean

@main def hello: Unit =
  day2Part1(s"$currentDirectory/src/main/scala/input.txt")

val currentDirectory = new java.io.File(".").getCanonicalPath

def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
  try {
    f(resource)
  } finally {
    resource.close()
  }

val readFile: (fileName: String) => List[String] = (fileName) => {
  using[BufferedSource, List[String]](
    Source.fromFile(fileName)
  )(resource => {
    resource.getLines.toList
  })
}
