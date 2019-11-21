package io.mtproto4s

import java.lang.Short
import io.mtproto4s.math.PqFactorization
import io.mtproto4s.math.CryptoUtils
import cats.data.Chain
import scala.util.Random
import io.mtproto4s.math.CryptoUtils._

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

  val nonce = Int128(
    reverse(0x3E0549828CCA27E9L),
    reverse(0x66B301A48FECE2FCL)
  )
  val serverNonce = Int128(
    reverse(0xA5CF4D33F4A11EA8L),
    reverse(0x77BA4AA573907330L)
  )
  val newNonce = Int256(
    reverse(0x311C85DB234AA264L),
    reverse(0x0AFC4A76A735CF5BL),
    reverse(0x1F0FD68BD17FA181L),
    reverse(0xE1229AD867CC024DL)
  )

  val encryptedString = """28A92FE20173B347A8BB324B5FAB2667C9A8BBCE6468D5B509A4CBDDC186240AC912CF7006AF8926DE606A2E74C0493CAA57741E6C82451F54D3E068F5CCC49B4444124B9666FFB405AAB564A3D01E67F6E912867C8D20D9882707DC330B17B4E0DD57CB53BFAAFA9EF5BE76AE6C1B9B6C51E2D6502A47C883095C46C81E3BE25F62427B585488BB3BF239213BF48EB8FE34C9A026CC8413934043974DB03556633038392CECB51F94824E140B98637730A4BE79A8F9DAFA39BAE81E1095849EA4C83467C92A3A17D997817C8A7AC61C3FF414DA37B7D66E949C0AEC858F048224210FCC61F11C3A910B431CCBD104CCCC8DC6D29D4A5D133BE639A4C32BBFF153E63ACA3AC52F2E4709B8AE01844B142C1EE89D075D64F69A399FEB04E656FE3675A6F8F412078F3D0B58DA15311C1A9F8E53B3CD6BB5572C294904B726D0BE337E2E21977DA26DD6E33270251C2CA29DFCC70227F0755F84CFDA9AC4B8DD5F84F1D1EB36BA45CDDC70444D8C213E4BD8F63B8AB95A2D0B4180DC91283DC063ACFB92D6A4E407CDE7C8C69689F77A007441D4A6A8384B666502D9B77FC68B5B43CC607E60A146223E110FCB43BC3C942EF981930CDC4A1D310C0B64D5E55D308D863251AB90502C3E46CC599E886A927CDA963B9EB16CE62603B68529EE98F9F5206419E03FB458EC4BD9454AA8F6BA777573CC54B328895B1DF25EAD9FB4CD5198EE022B2B81F388D281D5E5BC580107CA01A50665C32B552715F335FD76264FAD00DDD5AE45B94832AC79CE7C511D194BC42B70EFA850BB15C2012C5215CABFE97CE66B8D8734D0EE759A638AF013"""

  "An Authorization" must {
    "asdfasd" in {
      val pq = 0x17ED48941A08F981L
      val (p, q) = PqFactorization.factorize(pq).getOrElse(throw new Exception("Not factorized"))
      val pqStr = MtString(BigEndianLong(pq.toLong).encode.toList.toArray)
      val pStr = MtString(BigEndianInt(p.toInt).encode.toList.toArray)
      val qStr = MtString(BigEndianInt(q.toInt).encode.toList.toArray)

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

    "decrypt serverdh params" in {
      val newNonceEnc = newNonce.encode
      val serverNonceEnc = serverNonce.encode
      val aesKey = Chain.concat(
        sha1(Chain.concat(newNonceEnc, serverNonceEnc)),
        Chain(sha1(Chain.concat(serverNonceEnc, newNonceEnc)).iterator.take(12).toList: _*)
      )
      val aesIv = Chain.concat(
        Chain(sha1(Chain.concat(serverNonceEnc, newNonceEnc)).iterator.drop(12).take(8).toList: _*),
        Chain.concat(
          sha1(Chain.concat(newNonceEnc, newNonceEnc)),
          Chain(newNonceEnc.iterator.take(4).toList: _*)
        )
      )

      println(aesKey.dump("aes_key"))
      println(aesIv.dump("aes_iv"))

      val encryptedData = Chain.fromSeq(encryptedString
        .grouped(2)
        .map(str => Short.parseShort(str, 16).toByte)
        .toList
        .reverse
      )
      println(decryptAesIge(aesIv, aesKey, encryptedData).dump("aes"))
    }
  }
}
