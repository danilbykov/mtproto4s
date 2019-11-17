package io.mtproto4s

import java.net._
import java.io.OutputStream

import cats.data.Chain
import scala.util.Random
import shapeless.tag

import Boxed._
import java.io.InputStream
import io.mtproto4s.tags._
import io.mtproto4s.math.CryptoUtils._
import io.mtproto4s.math.PqFactorization

object Test extends App {
  import MTEncoders._
  import MTDecoders._
  import dump._

  def send[R: MTEncoder](os: OutputStream, request: R, prepend: Chain[Byte]): Unit = {
    val time = ((System.currentTimeMillis % 1000) << 20) + ((System.currentTimeMillis / 1000) << 32)
    val payload = request.encode
    val message = Message(0L, time, payload.size.toInt, payload)
    val bytes = message.encode
    val payloadWithLength =
      if (bytes.length < 127 * 4) {
        (bytes.length / 4).toByte +: bytes
      } else {
        (bytes.length / 4).encode.initLast
           .getOrElse(throw new Exception("Impossible"))
           ._1
           .prepend(0x7f.toByte)
           .++(bytes)
      }
    os.write(Chain.concat(prepend, payloadWithLength).iterator.toArray)
    println(Chain.concat(prepend, payloadWithLength).dump(s"Request ${request.getClass.getName()}:"))
    os.flush()
  }

  def totalLength(buffer: Array[Byte], offset: Int): Option[(Int, Int)] =
    if (offset > 0) {
      if (buffer.head != 127) {
        Some((1, buffer.head.toInt << 2))
      } else if (offset > 4) {
        (buffer.take(4).drop(1).toVector :+ 0.toByte).as[Int] match {
          case Success(result, _) => Some((4, result << 2))
          case Failure(_) => None
        }
      } else {
        None
      }
    } else {
      None
    }

  val messageDecoder = MTDecoder[Long] ~ MTDecoder[Long] ~ MTDecoder[Int]
  def read[R: MTDecoder](is: InputStream): R = {
    val buffer = new Array[Byte](1024)
    var offset = 0
    do {
      val readBytes = is.read(buffer, offset, buffer.size - offset)
      offset += readBytes
    } while ((offset == 0) || totalLength(buffer, offset).exists { case (prefix, length) => length == prefix + offset })
    val messageBytes = totalLength(buffer, offset) match {
      case Some((prefix, _)) => buffer.take(offset).drop(prefix)
      case None => throw new Exception("Impossible!")
    }
    println(Chain(messageBytes: _*).dump(s"Response:"))
    messageBytes.toVector.as(messageDecoder ~ MTDecoder[R]).map(_._2) match {
      case Success(result, _) =>
        result
      case Failure(es) =>
        throw new Exception(es.toList.mkString("\n"))
    }
  }

  val socket = new Socket(new Proxy(Proxy.Type.SOCKS, new InetSocketAddress("127.0.0.1", 1080)))
  socket.connect(new InetSocketAddress("149.154.167.40", 443), 5000)
  socket.setKeepAlive(true)
  socket.setTcpNoDelay(true)
  val is = socket.getInputStream()
  val os = socket.getOutputStream()

  val nonce = Int128(Random.nextLong(), Random.nextLong())
  send(os, ReqPqMulti(nonce), Chain.one(0xef.toByte))
  val resPq = read[ResPq](is)

  val pq = resPq.pq.bytes.toVector.as[BigEndianLong] match {
    case Success(res, _) => res.underlying
    case Failure(es) => throw new Exception(es.toList.mkString("\n"))
  }
  val Some((pnum, qnum)) = PqFactorization.factorize(pq)

  val p = MtString(BigEndianInt(pnum.toInt).encode.toList.toArray)
  val q = MtString(BigEndianInt(qnum.toInt).encode.toList.toArray)

  val newNonce = Int256(Random.nextLong(), Random.nextLong(), Random.nextLong(), Random.nextLong())
  val innerData = PQInnerDataDc(resPq.pq, p, q, nonce, resPq.serverNonce, newNonce)

  val encodedInnerDataDc = innerData.encode
  // println("encodes-data-dc", encodedInnerDataDc.map(b => (b & 0xff).toHexString).toList.mkString(" "))
  val sha1Hash = sha1(encodedInnerDataDc)
  val hashAndInnerData = Chain.concat(sha1Hash, encodedInnerDataDc)
  val randomTail = Chain.fromSeq(Random.alphanumeric.take(255 - hashAndInnerData.length.toInt).map(_.toByte))

  val rsaKey = resPq.serverPublicKeyFingerPrints.collectFirst(fingerprintToCert)
    .getOrElse(throw new Exception("No cert"))
  val encryptedData = rsa(rsaKey, Chain.concat(hashAndInnerData, randomTail))
  // println("len", encryptedData.length)
  // println("encrypted-data", encryptedData.map(b => (b & 0xff).toHexString).toList.mkString(" "))

  val dhParams = ReqDHParams(nonce, resPq.serverNonce, p, q, rsaKey.fingerprint,
    MtString(encryptedData.toList.toArray))
  send(os, dhParams, Chain.nil)

  val serverDhParams = read[ServerDHParamsOk](is)
  println(serverDhParams)

  val newNonceEnc = newNonce.encode
  val serverNonceEnc = resPq.serverNonce.encode
  val aesKey = Chain.concat(
    sha1(Chain.concat(newNonceEnc, serverNonceEnc)),
    Chain(sha1(Chain.concat(serverNonceEnc, newNonceEnc)).iterator.take(12).toList: _*)
  )
  val aesIv = Chain.concat(
    Chain(sha1(Chain.concat(serverNonceEnc, newNonceEnc)).iterator.drop(12).take(8).toList: _*),
    sha1(Chain.concat(newNonceEnc, newNonceEnc))
  )
  println(aesIv.size)

  decryptAes(aesIv, aesKey, Chain(serverDhParams.encryptedAnswer.bytes: _*)).dump("aes")
}
