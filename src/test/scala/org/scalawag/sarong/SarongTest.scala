package org.scalawag.sarong

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SarongTest extends AnyFunSpec with Matchers {

  it("should work with no expressions") {
    sarong"""
      1 2 3
    """ shouldBe """
      1 2 3
    """.boundingBox
  }

  it("should pop in simple expressions") {
    val a = "a"
    sarong"""
      1 $a 2
    """ shouldBe """
      1 a 2
    """.boundingBox
  }

  it("should pop in multiple simple expressions per line") {
    val a = "a"
    val b = "b"
    sarong"""
      1 $a 2 $b 3
    """ shouldBe """
      1 a 2 b 3
    """.boundingBox
  }

  it("should handle multiline string insertion") {
    val a = "this\nis\nmy\nblock"

    sarong"""
      1 $a 2
    """ shouldBe """
      1 this
        is
        my
        block 2
    """.boundingBox
  }

  it("should handle multiple multiline string insertions on one line") {
    val a = "this\nis\nmy\nblock"
    val b = "yet\n another\n  multiline\n   block"

    sarong"""
      1 $a 2 $b 3
    """ shouldBe """
      1 this
        is
        my
        block 2 yet
                 another
                  multiline
                   block 3
    """.boundingBox
  }

  it("should handle iterables") {
    val a = Iterable("a", "b", "c", "d")

    sarong"""
      1 ${a.iterate} 2
    """ shouldBe """
      1 a 2
      1 b 2
      1 c 2
      1 d 2
    """.boundingBox
  }

  it("should handle iterable bullets") {
    val a = Iterable("a", "b", "c", "d")

    sarong"""
      - ${a.iterate}
    """ shouldBe """
      - a
      - b
      - c
      - d
    """.boundingBox
  }

  it("should handle multiline template bullets") {
    val b = "yet\n another\n  multiline\n   block"
    val a = Iterable("a", b, "c", "d")

    sarong"""
      Here's the thing. There's going to be a bulleted list under
      here that contains all of the items.

      - ${a.iterate}

      That's all there is to say.
    """ shouldBe """
      Here's the thing. There's going to be a bulleted list under
      here that contains all of the items.

      - a
      - yet
         another
          multiline
           block
      - c
      - d

      That's all there is to say.
    """.boundingBox
  }

  it("should only treat a string as an when requested") {
    val a = "abc"
    sarong"""
      1 ${a.toIterable.iterate} 2
    """ shouldBe """
      1 a 2
      1 b 2
      1 c 2
    """.boundingBox
  }

  it("should treat iterables like normal unless they're alone") {
    val a = Iterable("a", "b", "c", "d")

    sarong"""
      1 ${a.iterate}, ${""}2
    """ shouldBe """
      1 List(a, b, c, d), 2
    """.boundingBox
  }

  it("should preserve blank lines in bounding box") {
    """
      
      1
      
      2
      
      
    """.boundingBox shouldBe "1\n\n2"
  }
}
