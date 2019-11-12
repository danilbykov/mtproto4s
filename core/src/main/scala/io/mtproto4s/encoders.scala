package io.mtproto4s

import cats.data.Chain
import shapeless.{::, HList, HNil}
import shapeless.Generic
import shapeless.Lazy
import shapeless.labelled.FieldType

import io.mtproto4s.tags._

trait MTEncoder[-T] {
  def encode(t: T): Chain[Byte]
}

object MTEncoder {
  implicit def apply[T: MTEncoder]: MTEncoder[T] =
    implicitly[MTEncoder[T]]
}

object MTEncoders {
  implicit class MTEncoderOps[A](val a: A) extends AnyVal {
    def encode(implicit encoder: MTEncoder[A]): Chain[Byte] = encoder.encode(a)
  }

  implicit val bigEndianIntEncoder: MTEncoder[BigEndianInt] =
    (i: BigEndianInt) => Chain.fromSeq(
      (i >> 24).toByte :: (i >> 16).toByte :: (i >> 8).toByte :: i.toByte :: Nil
    )

  implicit val littleEndianIntEncoder: MTEncoder[Int] =
    (i: Int) => Chain.fromSeq(
      i.toByte :: (i >> 8).toByte :: (i >> 16).toByte :: (i >> 24).toByte :: Nil
    )

  implicit val littleEndianLongEncoder: MTEncoder[Long] =
    (l: Long) => Chain.fromSeq(
      l.toByte :: (l >> 8).toByte :: (l >> 16).toByte :: (l >> 24).toByte ::
      (l >> 32).toByte :: (l >> 40).toByte :: (l >> 48).toByte :: (l >> 56).toByte ::
      Nil
    )

  implicit val bigEndianLongEncoder: MTEncoder[BigEndianLong] =
    (l: BigEndianLong) => Chain.fromSeq(
      (l >> 56).toByte :: (l >> 48).toByte :: (l >> 40).toByte :: (l >> 32).toByte ::
      (l >> 24).toByte :: (l >> 16).toByte :: (l >> 8).toByte :: l.toByte ::
      Nil
    )

  private val upToThreeZeroes: Map[Int, Chain[Byte]] =
    Map(
      1 -> Chain.one(0),
      2 -> Chain.fromSeq(List(0, 0)),
      3 -> Chain.fromSeq(List(0, 0, 0))
    )
  implicit val stringEncoder: MTEncoder[MtString] =
    (s: MtString) => {
      val bytes = s.bytes
      val bytesSize = bytes.size
      if (bytesSize <= 253) {
        val paddingSize = 4 - (bytesSize + 1) % 4
        val chain = Chain.concat(Chain.one(bytesSize.toByte), Chain(bytes: _*))
        Chain.concat(chain, upToThreeZeroes.getOrElse(paddingSize, Chain.nil))
      } else if (bytesSize < (2 << 24)) {
        val head = Chain(254.toByte, bytesSize.toByte, (bytesSize >> 8).toByte, (bytesSize >> 16).toByte)
        val chain = Chain.concat(head, Chain(bytes: _*))
        val paddingSize = 4 - bytesSize % 4
        Chain.concat(chain, upToThreeZeroes.getOrElse(paddingSize, Chain.nil))
      } else {
        throw new Exception(s"Too long string $bytesSize")
      }
    }

  implicit def vectorEncoder[X : MTEncoder]: MTEncoder[List[X]] =
    (xs: List[X]) => {
      val hash = MTEncoder[Int].encode(0x1cb5c415)
      val init = MTEncoder[Int].encode(xs.size)
      xs.iterator
        .map(MTEncoder[X].encode)
        .foldLeft[Chain[Byte]](Chain.concat(hash, init))(Chain.concat)
    }

  implicit val byteChainEncoder: MTEncoder[Chain[Byte]] =
    (t: Chain[Byte]) => t

  implicit val hnilEncoder: MTEncoder[HNil] =
    (_: HNil) => Chain.nil

  implicit def hlistEncoder[H, T <: HList : MTEncoder](implicit hEncoder: Lazy[MTEncoder[H]]): MTEncoder[H :: T] =
    (t: H :: T) => Chain.concat(hEncoder.value.encode(t.head), MTEncoder[T].encode(t.tail))

  implicit def primitiveGenericEncoder[X <: Bare, Y](implicit
    gen: Generic.Aux[X, Y],
    encoder: Lazy[MTEncoder[Y]]
  ): MTEncoder[X] =
    (t: X) => encoder.value.encode(gen.to(t))

  implicit def hashableGenericEncoder[X <: Boxed, Y](implicit
    gen: Generic.Aux[X, Y],
    encoder: Lazy[MTEncoder[Y]],
    hash: FieldType[X, Int]
  ): MTEncoder[X] =
    (t: X) => Chain.concat(MTEncoder[Int].encode(hash), encoder.value.encode(gen.to(t)))
}
