package common

import zio._
import zio.Task
import scala.io.Source
import java.io.IOException

object ZFileReader {
  def readLines[T](filePath: String)(f: String => T): Task[List[T]] = {
    val openFile = Task { Source.fromResource(filePath) }
    val close    = (s: Source) => Task { s.close() }.orDie
    openFile.acquireReleaseWith(close(_)) { s => Task { s.getLines().map(f).toList } }.refineToOrDie[IOException]
  }
}
