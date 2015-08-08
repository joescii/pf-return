package com.joescii.pf

import org.scalatest. { WordSpec, ShouldMatchers }
class ParseSpec extends WordSpec with ShouldMatchers {
  "The parse function" should {
    "parse" in {
      val file =
        """
          |AttributeSet my attributes
          |language scala
          |ComplexSet my numbers
          |3.14 0
          |42 8.8
          |hey buddy!!
        """.stripMargin

      val expected = List(
        AttributeSet("my attributes"),
        Attribute("language", "scala"),
        ComplexSet("my numbers"),
        Complex(3.14, 0),
        Complex(42, 8.8),
        Wtf("hey buddy!!")
      )

      val actual = parse(file)

      actual.length shouldEqual expected.length

      (actual zip expected) foreach { case (a, e) => a shouldEqual e }
    }
  }
}
