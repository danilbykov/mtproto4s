package io.mtproto4s

import cats.data.Chain

trait Bare extends Any
trait Boxed extends Any
trait Abstract extends Any

final case class BigEndianInt(val underlying: Int) extends AnyVal
final case class BigEndianLong(val underlying: Long) extends AnyVal

object tags {
  sealed trait BigEndianTag// extends Hashable

  //type BigEndianInt = Int @@ BigEndianTag
  //type BigEndianLong = Long @@ BigEndianTag
}

final case class MtString(bytes: Array[Byte]) {
}

final case class Int128(left: Long, right: Long) extends Bare

final case class Int256(first: Long, second: Long, third: Long, fourth: Long) extends Bare

final case class ReqPqMulti(nonce: Int128) extends Boxed

final case class ResPq(
  nonce: Int128,
  serverNonce: Int128,
  pq: MtString,
  serverPublicKeyFingerPrints: List[Long]
) extends Boxed

final case class PQInnerDataDc(
  pq: MtString,
  p: MtString,
  q: MtString,
  nonce: Int128,
  serverNonce: Int128,
  newNonce: Int256
) extends Boxed

// req_DH_params#d712e4be nonce:int128 server_nonce:int128 p:string q:string public_key_fingerprint:long encrypted_data:string = Server_DH_Params
final case class ReqDHParams(
  nonce: Int128,
  serverNonce: Int128,
  p: MtString,
  q: MtString,
  publicKeyFingerprint: Long,
  encryptedData: MtString
) extends Boxed

sealed trait ServerDHParams extends Abstract
final case class ServerDHParamsFail(
  nonce: Int128,
  serverNonce: Int128,
  newNonceHash: Int128
) extends ServerDHParams with Boxed
final case class ServerDHParamsOk(
  nonce: Int128,
  serverNonce: Int128,
  encryptedAnswer: MtString
) extends ServerDHParams with Boxed

// server_DH_inner_data#b5890dba nonce:int128 server_nonce:int128 g:int dh_prime:string g_a:string server_time:int = Server_DH_inner_data;
final case class ServerDHInnerData(
  nonce: Int128,
  serverNonce: Int128,
  g: Int,
  dhPrime: MtString,
  gA: MtString,
  serverTime: Int
) extends Boxed

// client_DH_inner_data#6643b654 nonce:int128 server_nonce:int128 retry_id:long g_b:string = Client_DH_Inner_Data
final case class ClientDHInnerData(
  nonce: Int128,
  serverNonce: Int128,
  retryId: Long,
  gB: MtString
) extends Boxed

// set_client_DH_params#f5045f1f nonce:int128 server_nonce:int128 encrypted_data:string = Set_client_DH_params_answer;
final case class SetClientDhParams(
  nonce: Int128,
  serverNonce: Int128,
  encryptedData: MtString
) extends Boxed

// dh_gen_ok#3bcbf734 nonce:int128 server_nonce:int128 new_nonce_hash1:int128 = Set_client_DH_params_answer;
final case class DhGenOk(
  nonce: Int128,
  serverNonce: Int128,
  newNonceHash1: Int128
) extends Boxed

object Boxed {
  import shapeless.labelled.field

  implicit val reqPqMultiHash = field[ReqPqMulti](0xbe7e8ef1)
  implicit val resPqHash = field[ResPq](0x05162463)
  implicit val pqInnerDataDc = field[PQInnerDataDc](0x83c95aec)//ec5ac983)//a9f55f95)
  implicit val reqDHParams = field[ReqDHParams](0xd712e4be)
  implicit val serverDhParamsFail = field[ServerDHParamsFail](0x79cb045d)
  implicit val serverDhParamsOk = field[ServerDHParamsOk](0xd0e8075c)
  implicit val serverDhInnerData = field[ServerDHInnerData](0xb5890dba)
  implicit val clientDhInnerData = field[ClientDHInnerData](0x6643b654)
  implicit val setClientDhParams = field[SetClientDhParams](0xf5045f1f)
  implicit val dhGenOk = field[DhGenOk](0x3bcbf734)
}

case class Message(
  authKeyId: Long,
  messageId: Long,
  length: Int,
  payload: Chain[Byte]
) extends Bare
