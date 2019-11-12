package io.mtproto4s

import org.scalatest.BeforeAndAfter
import org.scalatest.BeforeAndAfterAll
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.Millis
import org.scalatest.time.Seconds
import org.scalatest.time.Span

class DefaultSpec extends WordSpec with Matchers with ScalaFutures
    with BeforeAndAfterAll with BeforeAndAfter {

  implicit val defaultPatience = PatienceConfig(Span(20, Seconds), Span(100, Millis))
}
