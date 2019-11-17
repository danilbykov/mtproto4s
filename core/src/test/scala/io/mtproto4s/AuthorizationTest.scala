package io.mtproto4s
import io.mtproto4s.math.PqFactorization
import io.mtproto4s.math.CryptoUtils
import cats.data.Chain
import scala.util.Random

class AuthorizationTest extends DefaultSpec {
  import MTEncoders._
  import MTDecoders._
  import dump._

  def reverse(long: Long): Long = {
    long.encode.toVector.as[BigEndianLong] match {
      case Success(result, _) =>
        result.underlying
      case Failure(es) =>
        throw new Exception(es.toList.mkString("\n"))
    }
  }

  "An Authorization" must {
    "asdfasd" in {
      val pq = 0x17ED48941A08F981L
      val (p, q) = PqFactorization.factorize(pq).getOrElse(throw new Exception("Not factorized"))
      val pqStr = MtString(BigEndianLong(pq.toLong).encode.toList.toArray)
      val pStr = MtString(BigEndianInt(p.toInt).encode.toList.toArray)
      val qStr = MtString(BigEndianInt(q.toInt).encode.toList.toArray)

      val nonce = Int128(reverse(0x3E0549828CCA27E9L), reverse(0x66B301A48FECE2FCL))
      val serverNonce = Int128(reverse(0xA5CF4D33F4A11EA8L), reverse(0x77BA4AA573907330L))
      val newNonce = Int256(reverse(0x311C85DB234AA264L), reverse(0x0AFC4A76A735CF5BL), reverse(0x1F0FD68BD17FA181L), reverse(0xE1229AD867CC024DL))
      val innerData = PQInnerDataDc(pqStr, pStr, qStr, nonce, serverNonce, newNonce)

      val encodedInnerDataDc = innerData.encode
      println(encodedInnerDataDc.dump)
      val sha1Hash = CryptoUtils.sha1(encodedInnerDataDc)
      println(sha1Hash.dump)
      val hashAndInnerData = Chain.concat(sha1Hash, encodedInnerDataDc)
      val randomTail = Chain.fromSeq(Random.alphanumeric.take(255 - hashAndInnerData.length.toInt).map(_.toByte))

      val rsaKey = List(0xc3b42b026ce86b21L).collectFirst(CryptoUtils.fingerprintToCert)
        .getOrElse(throw new Exception("No cert"))
      val encryptedData = CryptoUtils.rsa(rsaKey, Chain.concat(hashAndInnerData, randomTail))
      println(encryptedData.dump)

      val dhParams = ReqDHParams(nonce, serverNonce, pStr, qStr, rsaKey.fingerprint,
        MtString(encryptedData.toList.toArray))

      val payload = dhParams.encode

      val message = Message(0L, 0L, payload.size.toInt, payload)
      val bytes = message.encode
      println(dump.dumpBytes(bytes))

      1 shouldBe 1
    }
  }
}
