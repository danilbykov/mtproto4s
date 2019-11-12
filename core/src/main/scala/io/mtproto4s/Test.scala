package io.mtproto4s

import java.net._
import java.io.OutputStream

import cats.data.Chain
import scala.util.Random
import shapeless.tag

import Boxed._
import java.io.InputStream
import io.mtproto4s.tags._
import io.mtproto4s.math.CryptoUtils
import io.mtproto4s.math.PqFactorization

object Test extends App {
  import MTEncoders._
  import MTDecoders._

  def send[R: MTEncoder](os: OutputStream, request: R, prepend: Chain[Byte]): Unit = {
    val time = ((System.currentTimeMillis % 1000) << 20) + ((System.currentTimeMillis / 1000) << 32)
    val payload = request.encode
    val message = Message(0L, time, payload.size.toInt, payload)
    val bytes = message.encode
    val payloadWithLength =
      if (bytes.length < 127 * 4) {
        (bytes.length / 4).toByte +: bytes
      } else {
        println((bytes.length / 4).encode.map(b => (b & 0xff).toHexString).toList.mkString(" "))
        (bytes.length / 4).encode.initLast
           .getOrElse(throw new Exception("Impossible"))
           ._1
           .prepend(0x7f.toByte)
           .++(bytes)
      }
    os.write(Chain.concat(prepend, payloadWithLength).iterator.toArray)
    println(Chain.concat(prepend, payloadWithLength).map(b => (b & 0xff).toHexString).toList.mkString(" "))
    os.flush()
  }

  val messageDecoder = MTDecoder[Long] ~ MTDecoder[Long] ~ MTDecoder[Int]
  def read[R: MTDecoder](is: InputStream): R = {
    val buffer = new Array[Byte](1024)
    var offset = 0
    do {
      val readBytes = is.read(buffer, offset, buffer.size - offset)
      offset += readBytes
    } while ((offset == 0) || (offset > 0 && (buffer.head.toInt << 2 == offset + 1)))
    println(buffer(0))
    val messageBytes = buffer.take(offset).drop(1)
    println(messageBytes.map(b => (b & 0xff).toHexString).toList.mkString(" "))
    println(messageBytes.size)
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
    case Success(res, _) => res
    case Failure(es) => throw new Exception(es.toList.mkString("\n"))
  }
  val Some((pnum, qnum)) = PqFactorization.factorize(pq)

  val p = MtString(tag[BigEndianTag](pnum.toInt).encode.toList.toArray)
  val q = MtString(tag[BigEndianTag](qnum.toInt).encode.toList.toArray)

  val newNonce = Int256(Random.nextLong(), Random.nextLong(), Random.nextLong(), Random.nextLong())
  val innerData = PQInnerDataDc(resPq.pq, p, q, nonce, resPq.serverNonce, newNonce)

  val encodedInnerDataDc = innerData.encode
  val sha1Hash = CryptoUtils.sha1(encodedInnerDataDc)
  val hashAndInnerData = Chain.concat(sha1Hash, encodedInnerDataDc)
  val randomTail = Chain.fromSeq(Random.alphanumeric.take(255 - hashAndInnerData.length.toInt).map(_.toByte))

  val rsaKey = resPq.serverPublicKeyFingerPrints.collectFirst(CryptoUtils.fingerprintToCert)
    .getOrElse(throw new Exception("No cert"))
  val encryptedData = CryptoUtils.rsa(rsaKey, Chain.concat(hashAndInnerData, randomTail))
  println("len", encryptedData.length)

  val dhParams = ReqDHParams(nonce, resPq.serverNonce, p, q, rsaKey.fingerprint,
    MtString(encryptedData.toList.toArray))
  send(os, dhParams, Chain.nil)

  val serverDhParams = read[ServerDHParamsOk](is)
  println(serverDhParams)
}
