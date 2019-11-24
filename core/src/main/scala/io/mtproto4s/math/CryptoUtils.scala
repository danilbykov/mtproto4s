package io.mtproto4s.math

import java.math.BigInteger
import java.security.interfaces.RSAPublicKey
import java.security.KeyFactory
import java.security.MessageDigest
import java.security.spec.X509EncodedKeySpec
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import cats.data.Chain
import io.mtproto4s.Failure
import io.mtproto4s.MTDecoder
import io.mtproto4s.MTDecoders._
import io.mtproto4s.MTEncoders._
import io.mtproto4s.MtString
import io.mtproto4s.Success
import java.security.spec.RSAPublicKeySpec
import scala.util.Random

case class RsaKey(
  modulus: BigInteger,
  exponent: BigInteger,
  fingerprint: Long
)

object CryptoUtils {

  val fingerprintToCert: Map[Long, RsaKey] =
    List("key1.der", "key2.der", "key3.der", "key4.der", "oldkey.der", "testkey.der")
      .map(rsa)
      .map(key => key.fingerprint -> key)
      .toMap

  def sha1(bytes: Chain[Byte]): Chain[Byte] = {
    val digest = MessageDigest.getInstance("SHA-1")
    digest.update(bytes.toList.toArray)
    Chain(digest.digest(): _*)
  }

  private def rsa(certName: String): RsaKey = {
    val keyFactory = KeyFactory.getInstance("RSA")
    val is = Thread.currentThread.getContextClassLoader.getResourceAsStream(certName)
    val bytes = Stream.continually(is.read).takeWhile(_ != -1).map(_.toByte).toArray
    val publicKey = keyFactory.generatePublic(new X509EncodedKeySpec(bytes)).asInstanceOf[RSAPublicKey]
    val modulusBytes = publicKey.getModulus.toByteArray.dropWhile(_ == 0)
    val expBytes = publicKey.getPublicExponent.toByteArray.dropWhile(_ == 0)
    val modulesAndExpBytes = Chain.concat(MtString(modulusBytes).encode, MtString(expBytes).encode)
    val fingerprint = sha1(modulesAndExpBytes).toList.takeRight(8).toVector.as[Long] match {
      case Success(result, _) => result
      case Failure(es) => throw new Exception(es.map(_.str).reduceLeft(_ + "\n" + _))
    }
    RsaKey(publicKey.getModulus, publicKey.getPublicExponent, fingerprint)
  }

  def rsa(rsaKey: RsaKey, bytes: Chain[Byte]): Chain[Byte] = {
    val keyFactory = KeyFactory.getInstance("RSA")
    val publicKey = keyFactory.generatePublic(new RSAPublicKeySpec(rsaKey.modulus, rsaKey.exponent))
    val cipher = Cipher.getInstance("RSA/ECB/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, publicKey)
    return Chain(cipher.doFinal(bytes.toList.toArray): _*)
  }

  def decryptAes(initialVector: Chain[Byte], aesKey: Chain[Byte], bytes: Chain[Byte]): Chain[Byte] = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
    val ivSpec = new javax.crypto.spec.IvParameterSpec(initialVector.iterator.toArray)
    val keySpec = new SecretKeySpec(aesKey.iterator.toArray, "AES_256")
    cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec)
    Chain(cipher.doFinal(bytes.iterator.toArray): _*)
  }

  def decryptAesIge(initialVector: Chain[Byte], aesKey: Chain[Byte], bytes: Chain[Byte]): Chain[Byte] = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(aesKey.iterator.toArray, "AES"))

    val blocksize = cipher.getBlockSize()
    val (xPrev, yPrev) = initialVector.toList.toArray.splitAt(blocksize)
    bytes.iterator.grouped(blocksize)
      .foldLeft((xPrev, yPrev, Chain.empty[Byte])) { case ((xPrev, yPrev, result), block) =>
        val blockArray = block.toArray
        val y = xor(cipher.doFinal(xor(blockArray, yPrev)), xPrev)
        (blockArray, y, Chain.concat(result, Chain(y: _*)))
      }
      ._3
  }

  def encryptAesIge(initialVector: Chain[Byte], aesKey: Chain[Byte], data: Chain[Byte]): Chain[Byte] = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(aesKey.iterator.toArray, "AES"))

    val blocksize = cipher.getBlockSize()
    val (xPrev, yPrev) = initialVector.toList.toArray.splitAt(blocksize)
    padBytes(data, blocksize).iterator
      .grouped(blocksize)
      .foldLeft((xPrev, yPrev, Chain.empty[Byte])) { case ((xPrev, yPrev, result), block) =>
        val blockArray = block.toArray
        val x = xor(cipher.doFinal(xor(blockArray, xPrev)), yPrev)
        (x, blockArray, Chain.concat(result, Chain(x: _*)))
      }
      ._3
  }

  def padBytes(bytes: Chain[Byte], length: Int): Chain[Byte] = {
    val size = bytes.length
    val remainder = size % length
    if (remainder == 0) {
      bytes
    } else {
      Chain.concat(bytes, Chain.fromSeq(List.fill(length - remainder.toInt)(0))) // FIXME: Generate random chain
    }
  }

  private def xor(left: Array[Byte], right: Array[Byte]): Array[Byte] = {
    val length = left.size max right.size
    val result = new Array[Byte](length)
    0 until length foreach { i =>
      result(i) = (left(i) ^ right(i)).toByte
    }
    result
  }

  def generateSeed(size: Int): Chain[Byte] = {
    val array = new Array[Byte](size)
    Random.nextBytes(array)
    Chain(array: _*)
  }
}
