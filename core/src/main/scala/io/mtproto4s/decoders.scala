package io.mtproto4s

import cats.data.NonEmptyList
import cats.data.NonEmptyChain
import shapeless.{::, HList, HNil}
import shapeless.LabelledGeneric
import shapeless.Lazy
import shapeless.Witness
import shapeless.labelled.{FieldType, field}
import shapeless.tag

import io.mtproto4s.tags._

case class ParserError(str: String)

sealed trait DecodedResult[+A] {
  def map[B](func: A => B): DecodedResult[B]

  def mapBytes(func: Int => Int): DecodedResult[A]

  def mapError(func: NonEmptyList[ParserError] => NonEmptyList[ParserError]): DecodedResult[A]
}

case class Success[+A](result: A, bytesConsumed: Int) extends DecodedResult[A] {
  override def map[B](func: A => B): DecodedResult[B] =
    Success(func(result), bytesConsumed)

  override def mapBytes(func: Int => Int): DecodedResult[A] =
    Success(result, func(bytesConsumed))

  override def mapError(func: NonEmptyList[ParserError] => NonEmptyList[ParserError]): DecodedResult[A] =
    this
}

case class Failure(es: NonEmptyList[ParserError]) extends DecodedResult[Nothing] {
  override def map[B](func: Nothing => B): DecodedResult[B] = this

  override def mapBytes(func: Int => Int): DecodedResult[Nothing] = this

  override def mapError(func: NonEmptyList[ParserError] => NonEmptyList[ParserError]): DecodedResult[Nothing] =
    Failure(func(es))
}

object Failure {
  def apply(error: ParserError): DecodedResult[Nothing] =
    Failure(NonEmptyList.one(error))
}

trait MTDecoder[A] {
  self =>

  def decode(bytes: Vector[Byte]): DecodedResult[A]

  def map[B](func: A => B): MTDecoder[B] =
    (bytes: Vector[Byte]) => self.decode(bytes).map(func)

  def flatMap[B](func: A => MTDecoder[B]): MTDecoder[B] =
    (bytes: Vector[Byte]) => {
      self.decode(bytes) match {
        case Success(result, bytesConsumed) =>
          func(result).decode(bytes.drop(bytesConsumed)).mapBytes(_ + bytesConsumed)
        case f: Failure =>
          f
      }
    }

  def ~[B](other: MTDecoder[B]): MTDecoder[(A, B)] =
    this.flatMap(a => other.map(b => (a, b)))

  def filter(func: A => Boolean, fallback: A => Failure): MTDecoder[A] =
    (bytes: Vector[Byte]) => {
      self.decode(bytes) match {
        case s @ Success(result, _) if func(result) => s
        case Success(result, _) => fallback(result)
        case f: Failure => f
      }
    }

  def *>[B](other: => MTDecoder[B]): MTDecoder[B] =
    self.flatMap(_ => other)

  def repeat(n: Int): MTDecoder[List[A]] =
    (bytes: Vector[Byte]) => {
      (0 until n).foldLeft[DecodedResult[List[A]]](Success(Nil, 0)) { case (acc, _) =>
        acc match {
          case Success(list, accConsumed) =>
            self.decode(bytes.drop(accConsumed)) match {
              case Success(a, bytesConsumed) =>
                Success(a :: list, accConsumed + bytesConsumed)
              case f @ Failure(_) => f
            }
          case f @ Failure(_) => f
        }
      } map (_.reverse)
    }
}

object MTDecoder {
  implicit def apply[A : MTDecoder]: MTDecoder[A] = implicitly[MTDecoder[A]]

  def pure[A](a: => A): MTDecoder[A] =
    (_: Vector[Byte]) => Success(a, 0)
}

object MTDecoders {

  implicit class ByteChainOps(val bytes: Vector[Byte]) extends AnyVal {

    def as[A: MTDecoder]: DecodedResult[A] =
      MTDecoder[A].decode(bytes)
  }

  implicit val bigEndianIntDecoder: MTDecoder[BigEndianInt] =
    (bytes: Vector[Byte]) => {
      if (bytes.size < 4) {
        Failure(ParserError("Not enough bytes to parse BigEndianInt"))
      } else {
        val result =
           (bytes(0).toInt << 24) |
          ((bytes(1).toInt & 0xff) << 16) |
          ((bytes(2).toInt & 0xff) << 8) |
           (bytes(3).toInt & 0xff)
        Success(tag[BigEndianTag][Int](result), 4)
      }
    }

  implicit val littleEndianIntDecoder: MTDecoder[Int] =
    (bytes: Vector[Byte]) => {
      if (bytes.size < 4) {
        Failure(ParserError("Not enough bytes to parse Int"))
      } else {
        val result =
           (bytes(0).toInt & 0xff) |
          ((bytes(1).toInt & 0xff) << 8) |
          ((bytes(2).toInt & 0xff) << 16) |
           (bytes(3).toInt << 24)
        Success(result, 4)
      }
    }

  implicit val bigEndianLongDecoder: MTDecoder[BigEndianLong] =
    (bytes: Vector[Byte]) => {
      if (bytes.size < 8) {
        Failure(ParserError("Not enough bytes to parse BigEndianLong"))
      } else {
        val result =
           (bytes(0).toLong << 56) |
          ((bytes(1).toLong & 0xff) << 48) |
          ((bytes(2).toLong & 0xff) << 40) |
          ((bytes(3).toLong & 0xff) << 32) |
          ((bytes(4).toLong & 0xff) << 24) |
          ((bytes(5).toLong & 0xff) << 16) |
          ((bytes(6).toLong & 0xff) << 8) |
           (bytes(7).toLong & 0xff)
        Success(tag[BigEndianTag][Long](result), 8)
      }
    }

  implicit val littleEndianLongDecoder: MTDecoder[Long] =
    (bytes: Vector[Byte]) => {
      if (bytes.size < 8) {
        Failure(ParserError("Not enough bytes to parse Long"))
      } else {
        val result =
           (bytes(0).toLong & 0xff) |
          ((bytes(1).toLong & 0xff) << 8) |
          ((bytes(2).toLong & 0xff) << 16) |
          ((bytes(3).toLong & 0xff) << 24) |
          ((bytes(4).toLong & 0xff) << 32) |
          ((bytes(5).toLong & 0xff) << 40) |
          ((bytes(6).toLong & 0xff) << 48) |
           (bytes(7).toLong << 56)
        Success(result, 8)
      }
    }

  def padToFour(i: Int) =
    if (i % 4 == 0) i else (i >> 2 << 2) + 4

  implicit val stringDecoder: MTDecoder[MtString] =
    (bytes: Vector[Byte]) => {
      if (bytes.isEmpty) {
        Failure(ParserError("Can not parse string from empty stream"))
      } else {
        val firstByte = bytes.head.toInt & 0xff
        if (firstByte == 254) {
          if (bytes.size <= 4) {
            Failure(ParserError("Not enough bytes to extract length"))
          } else {
            val size = (bytes(1).toInt & 0xff) | ((bytes(2).toInt & 0xff) << 8) | ((bytes(3).toInt & 0xff) << 16)
            val fullSize = padToFour(size + 4)
            if (bytes.size < fullSize) {
              Failure(ParserError(s"Not enough bytes to extract $size character string. Only ${bytes.size} available."))
            } else {
              Success(MtString(bytes.take(size + 4).drop(4).toArray), fullSize)
            }
          }
        } else if (firstByte <= 253) {
          val size = firstByte
          val fullSize = padToFour(size + 1)
          if (bytes.size < fullSize) {
            Failure(ParserError(s"Not enough bytes to parse $size character string"))
          } else {
            Success(MtString(bytes.take(size + 1).drop(1).toArray), fullSize)
          }
        } else {
          Failure(ParserError(s"Unexpected first string byte $firstByte"))
        }
      }
    }

  implicit def vectorDecoder[X : MTDecoder]: MTDecoder[List[X]] =
    MTDecoder[Int].filter(
      _ == 0x1cb5c415,
      currentHash => Failure(NonEmptyList.one(ParserError(s"Unexpected hash $currentHash for list")))
    ) *> MTDecoder[Int].flatMap(n => MTDecoder[X].repeat(n))

  implicit val hnilDecoder: MTDecoder[HNil] =
    (_: Vector[Byte]) => Success(HNil, 0)

  implicit def hlistDecoder[K <: Symbol, H, T <: HList : MTDecoder](implicit
    witness: Witness.Aux[K],
    hDecoder: Lazy[MTDecoder[H]]
  ): MTDecoder[FieldType[K, H] :: T] =
    (bytes: Vector[Byte]) => {
      hDecoder.value.decode(bytes) match {
        case Success(head, consumedHead) =>
          MTDecoder[T].decode(bytes.drop(consumedHead)) match {
            case Success(tail, tailConsumed) =>
              Success(field[K](head) :: tail, consumedHead + tailConsumed)
            case excp: Failure =>
              excp
          }
        case excp: Failure =>
          excp.mapError(ParserError(s"Can not decode field ${witness.value.name}") :: _)
      }
    }

  import scala.reflect.runtime.universe._
  implicit def primitiveGenericDecoder[X <: Bare, Y <: HList](implicit
    gen: LabelledGeneric.Aux[X, Y],
    decoder: Lazy[MTDecoder[Y]],
    typeTag: TypeTag[X]
  ): MTDecoder[X] =
    (bytes: Vector[Byte]) => {
      decoder.value.decode(bytes).map(gen.from).mapError(ParserError(s"Can not decode class ${typeTag.tpe.typeSymbol.name}") :: _)
    }

  implicit def hashableGenericDecoder[X <: Boxed, Y <: HList](implicit
    gen: LabelledGeneric.Aux[X, Y],
    decoder: Lazy[MTDecoder[Y]],
    hash: FieldType[X, Int],
    typeTag: TypeTag[X]
  ): MTDecoder[X] =
    MTDecoder[Int].filter(
      _ == hash,
      currentHash => Failure(NonEmptyList.one(ParserError(s"Unexpected hash $currentHash. Expected $hash.")))
    ) *> ((bytes: Vector[Byte]) => {
      decoder.value.decode(bytes).map(gen.from).mapError(ParserError(s"Can not decode class ${typeTag.tpe.typeSymbol.name}") :: _)
    })
}
