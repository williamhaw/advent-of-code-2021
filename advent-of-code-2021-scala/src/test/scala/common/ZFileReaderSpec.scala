package common

import zio.test._
import Assertion._

object ZFileReaderSpec extends DefaultRunnableSpec {
    def spec = suite("ZFileReaderSpec")(
        test("readLines should get and parse input"){
            for {
                output <- ZFileReader.readLines[Int]("test-input.txt")((s: String) => {s.toInt})
            } yield assert(output)(equalTo(List(5535, 2202)))
        }
    )
}