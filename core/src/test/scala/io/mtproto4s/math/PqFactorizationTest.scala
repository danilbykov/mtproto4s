package io.mtproto4s.math

import io.mtproto4s.DefaultSpec

class PqFactorizationTest extends DefaultSpec {

  "PqFactorization" must {

    "factorize" in {
      val res = PqFactorization.factorize(0x17ED48941A08F981L)
      val exp = (0x53911073, 0x494C553B)
      res should (be(Some(exp)) or be(Some(exp.swap)))
    }
  }
}
