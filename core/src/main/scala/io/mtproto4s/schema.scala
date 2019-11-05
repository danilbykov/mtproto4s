package io.mtproto4s

import cats.data.Chain

trait Bare extends Any
trait Boxed extends Any

/*
object tags {
  sealed trait LittleEndianTag extends Hashable
  sealed trait BigEndianTag extends Hashable

  type LittleEndianInt = Int @@ LittleEndianTag
  type BigEndianInt = Int @@ BigEndianTag

  type LittleEndianLong = Long @@ LittleEndianTag
  type BigEndianLong = Long @@ BigEndianTag
}

import tags._
*/

case class Int128(left: Long, right: Long) extends Bare

case class ReqPqMulti(nonce: Int128) extends Boxed

case class ResPq(
  nonce: Int128,
  serverNonce: Int128,
  pq: String,
  serverPublicKeyFingerPrints: List[Long]
) extends Boxed

object Boxed {
  import shapeless.labelled.field

  implicit val reqPqMultiHash = field[ReqPqMulti](0xbe7e8ef1)
  implicit val resPqHash = field[ResPq](0x05162463)
}

case class Message(
  authKeyId: Long,
  messageId: Long,
  length: Int,
  payload: Chain[Byte]
) extends Bare
