package io.mtproto4s

import cats.data.Chain

object dump {

  def dumpBytes(bytes: Chain[Byte]): String = {
    val sb = new StringBuilder
    bytes.iterator
      .grouped(8)
      .grouped(2)
      .zipWithIndex
      .foreach { case (subgroups, idx) =>
        val row = 16 * idx
        sb.append(f"$row%04X  |")
        subgroups.foreach { subgroup =>
          sb.append(" ")
          subgroup.foreach(b => sb.append(f" $b%02X"))
        }
        sb.append("\n")
      }
    sb.toString
  }

  implicit class ChainOps(val bytes: Chain[Byte]) extends AnyVal {
    def dump(header: String): String =
      s"$header\n${dumpBytes(bytes)}"

    def dump: String =
      dumpBytes(bytes)
  }
}
