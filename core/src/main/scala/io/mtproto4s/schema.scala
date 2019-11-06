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

final case class Int128(left: Long, right: Long) extends Bare

final case class Int256(first: Long, second: Long, third: Long, fourth: Long) extends Bare

final case class ReqPqMulti(nonce: Int128) extends Boxed

final case class ResPq(
  nonce: Int128,
  serverNonce: Int128,
  pq: String,
  serverPublicKeyFingerPrints: List[Long]
) extends Boxed

final case class PQInnerDataDc(
  pq: String,
  p: String,
  q: String,
  nonce: Int128,
  serverNonce: Int128,
  newNonce: Int256
) extends Boxed

final case class ReqDHParams(
  nonce: Int128,
  serverNonce: Int128,
  p: String,
  q: String,
  publicKeyFingerprint: Long,
  encryptedData: String
)

sealed trait ServerDHParams
final case class ServerDHParamsFail(
  nonce: Int128,
  serverNonce: Int128,
  newNonceHash: Int128
) extends ServerDHParams with Boxed
final case class ServerDHParamsOk(
  nonce: Int128,
  serverNonce: Int128,
  encryptedAnswer: String
) extends ServerDHParams with Boxed

object Boxed {
  import shapeless.labelled.field

  implicit val reqPqMultiHash = field[ReqPqMulti](0xbe7e8ef1)
  implicit val resPqHash = field[ResPq](0x05162463)
  implicit val pqInnerDataDc = field[PQInnerDataDc](0xa9f55f95)
  implicit val reqDHParams = field[ReqDHParams](0xd712e4be)
  implicit val serverDhParamsFail = field[ServerDHParamsFail](0x79cb045d)
  implicit val serverDhParamsOk = field[ServerDHParamsOk](0xd0e8075c)
}

case class Message(
  authKeyId: Long,
  messageId: Long,
  length: Int,
  payload: Chain[Byte]
) extends Bare
