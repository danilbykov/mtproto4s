package io.mtproto4s

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.Checkers

class SchemaTest extends DefaultSpec with Checkers {

  import MTEncoders._
  import MTDecoders._

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 500)

  "Encoders & Decoders" must {
    "encode int128" in {
      val result = MTEncoder[Int128].encode(Int128(123123L, 9876543210L)).toList
      result shouldBe List(0xf3, 0xe0, 0x01, 0, 0, 0, 0, 0, 0xea, 0x16, 0xb0, 0x4c, 0x02, 0, 0, 0).map(_.toByte)
    }

    /*
    "encode & decode big endian ints" in {
      val encoder = MTEncoder[BigEndianInt]
      val decoder = MTDecoder[BigEndianInt]
      check(forAll { int: Int =>
        decoder.decode(encoder.encode(tag[BigEndianTag][Int](int)).toVector) == Success(int, 4)
      })
    }
    */

    "encode & decode little endian ints" in {
      val encoder = MTEncoder[Int]
      val decoder = MTDecoder[Int]
      check(forAll { int: Int =>
        decoder.decode(encoder.encode(int).toVector) == Success(int, 4)
      })
    }

    /*
    "encode & decode big endian longs" in {
      val encoder = MTEncoder[BigEndianLong]
      val decoder = MTDecoder[BigEndianLong]
      check(forAll { long: Long =>
        decoder.decode(encoder.encode(tag[BigEndianTag][Long](long)).toVector) == Success(long, 8)
      })
    }
    */

    "encode & decode little endian longs" in {
      val encoder = MTEncoder[Long]
      val decoder = MTDecoder[Long]
      check(forAll { long: Long =>
        decoder.decode(encoder.encode(long).toVector) == Success(long, 8)
      })
    }

    "encode & decode strings" in {
      val encoder = MTEncoder[String]
      val decoder = MTDecoder[String]
      check(forAll { str: String =>
        val result = decoder.decode(encoder.encode(str).toVector)
        result.asInstanceOf[Success[String]].result == str
      })
    }

    "encode & decode ReqPqMulti" in {
      val encoder = MTEncoder[ReqPqMulti]
      val decoder = MTDecoder[ReqPqMulti]
      check(forAll { (left: Long, right: Long) =>
        val req = ReqPqMulti(Int128(left, right))
        val bytes = encoder.encode(req).toVector
        val result = decoder.decode(bytes)
        result == Success(req, bytes.size)
      })
    }

    "encode & decode Vector[Long]" in {
      val encoder = MTEncoder[List[Long]]
      val decoder = MTDecoder[List[Long]]
      check(forAll { longs: List[Long] =>
        val bytes = encoder.encode(longs).toVector
        val result = decoder.decode(bytes)
        result == Success(longs, bytes.size)
      })
    }

    "encode & decode Vector[ReqPqMulti]" in {
      val encoder = MTEncoder[List[ReqPqMulti]]
      val decoder = MTDecoder[List[ReqPqMulti]]
      check(forAll { lefts: List[Long] =>
        val rights = lefts.reverse
        val reqs = lefts.zip(rights).map(Int128.tupled).map(ReqPqMulti)
        val bytes = encoder.encode(reqs).toVector
        val result = decoder.decode(bytes)
        result == Success(reqs, bytes.size)
      })
    }
  }
}
