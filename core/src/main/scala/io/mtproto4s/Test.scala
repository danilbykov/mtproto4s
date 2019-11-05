package io.mtproto4s

import java.net._
import java.io.OutputStream

import scala.util.Random

import Boxed._
import java.io.InputStream

object Test extends App {
  import MTEncoders._
  import MTDecoders._

  def send[R: MTEncoder](os: OutputStream, request: R): Unit = {
    val time = ((System.currentTimeMillis % 1000) << 20) + ((System.currentTimeMillis / 1000) << 32)
    val payload = MTEncoder[R].encode(request)
    val message = Message(0L, time, payload.size.toInt, payload)
    val bytes = MTEncoder[Message].encode(message)
    if (bytes.length < 250) {
      val payloadWithLength = 0xef.toByte +: (bytes.length / 4).toByte +: bytes
      os.write(payloadWithLength.iterator.toArray)
      println(payloadWithLength.map(b => (b & 0xff).toHexString).toList.mkString(" "))
      os.flush()
    } else {
      throw new Exception("Shit happens")
    }
  }

  val messageDecoder = MTDecoder[/*BigEndian*/ Long] ~ MTDecoder[Long] ~ MTDecoder[Int]
  def read[R: MTDecoder](is: InputStream): R = {
    val buffer = new Array[Byte](1024)
    var offset = 0
    do {
      val readBytes = is.read(buffer, offset, buffer.size - offset)
      offset += readBytes
    } while ((offset == 0) || (offset > 0 && (buffer.head.toInt << 2 == offset + 1)))
    val messageBytes = buffer.take(offset).drop(1)
    println(messageBytes.map(b => (b & 0xff).toHexString).toList.mkString(" "))
    println(messageBytes.size)
    (messageDecoder ~ MTDecoder[R]).decode(messageBytes.toVector).map(_._2) match {
      case Success(result, bytesConsumed) =>
        result
      case Failure(es) => 
        throw new Exception(es.toList.mkString("\n"))
    }
  }

  // val address = InetAddress.getByName("149.154.167.40")
  // val socket = new Socket(address, 443)

  val socketAddress = InetSocketAddress.createUnresolved("149.154.167.40", 443)
  //val socketAddress = InetSocketAddress.createUnresolved("google.com", 443)

  val socket = new Socket(new Proxy(Proxy.Type.SOCKS, new InetSocketAddress("127.0.0.1", 1080)))
  socket.connect(new InetSocketAddress("149.154.167.40", 443), 5000)
  socket.setKeepAlive(true)
  socket.setTcpNoDelay(true)
  val is = socket.getInputStream()
  val os = socket.getOutputStream()

  //implicitly[MTDecoder[ReqPqMulti]]

  send(os, ReqPqMulti(Int128(Random.nextLong(), Random.nextLong())))
  val r = read[ResPq](is)
  println(r)

  /*
  var i = 0
  val buffer = new Array[Byte](100)
  // println(is.read(buffer))
  while (true) {
    println("tick")
    val count =
      //if (is.available() > 0) {
        is.read(buffer)
      //} else {
        //0
      //}
    if (count > 0) {
      println(buffer.take(count).map(b => (b & 0xff).toHexString).mkString("  "))
    }
    Thread.sleep(1000)

    i += 1
    //os.write(0x0)
    os.flush()
  }
    */
}
