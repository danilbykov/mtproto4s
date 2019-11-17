package io.mtproto4s.math

import scala.util.Random

object PqFactorization {

  def factorize(pq: Long): Option[(Long, Long)] =
    (0 until 3).iterator.flatMap { i =>
      val q = (Random.nextInt(128) & 15) + 17
      var x = Random.nextInt(1000000000).toLong + 1
      var y = x
      val lim = 1 << (i + 18)
      (1 until lim).iterator map { j =>
        var a = x
        var b = x
        var c = q.toLong
        while (b != 0) {
          if ((b & 1) != 0) {
            c += a
            if (c >= pq) c -= pq
          }
          a += a
          if (a >= pq) a -= pq
          b >>= 1
        }
        x = c
        val z = (x - y).abs
        if ((j & (j - 1)) == 0) y = x
        gcd(z, pq)
      } dropWhile (_ == 1)
    } find (_ > 1) map { q =>
      val p = pq / q
      (p min q, p max q)
    }

  def gcd(arg1: Long, arg2: Long): Long = {
    var a = arg1
    var b = arg2
    while (a != 0 && b != 0) {
      while ((b & 1) == 0) {
        b >>= 1
      }
      while ((a & 1) == 0) {
        a >>= 1
      }
      if (a > b) {
        a -= b
      } else {
        b -= a
      }
    }
    if (b == 0) a else b
  }
}
