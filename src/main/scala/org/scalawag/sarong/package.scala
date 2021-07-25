package org.scalawag

import scala.annotation.tailrec

package object sarong {
  // We need to split strings into lines this way to ensure that we get all the lines, including empty ones.
  private def findNewLines(s: String): List[Element] = {
    @tailrec
    def go(remains: String, acc: List[Element]): List[Element] =
      remains.indexOf('\n') match {
        case n if n < 0 =>
          Literal(remains) :: acc
        case n =>
          go(remains.drop(n+1), NewLine :: Literal(remains.take(n)) :: acc)
      }

    go(s, Nil).reverse
  }

  private def lines(s: String): List[String] = {
    @tailrec
    def go(remains: String, acc: List[String]): List[String] =
      remains.indexOf('\n') match {
        case n if n < 0 =>
          remains :: acc
        case n =>
          go(remains.drop(n+1), remains.take(n+1) :: acc)
      }


    go(s, Nil).reverse
  }

  private sealed trait Element
  private sealed trait RawElement extends Element {
    val render: String
  }
  private final case class Literal(s: String) extends RawElement {
    override val render: String = s
  }
  private final case class Expression(x: Any) extends RawElement {
    override val render: String = x.toString
  }
  private case object NewLine extends Element

  private final case class Line(head: Literal, tail: List[(Expression, Literal)]) {
    def toList = head :: tail.flatMap { case (e, l) => e :: l :: Nil }
    override def toString: String = toList.mkString(" :: ")
  }

  implicit class Sarong(s: StringContext) {

    private def interleave[A](l: Iterable[A], r: Iterable[A]): Iterable[A] =
      l.zip(r).foldRight(List(l.last)) { case ((a, b), acc) => a :: b :: acc }

    private def toLines(before: String, expressionLines: String, after: String): List[String] = {
      val prefix = " " * before.length
      val elines = findNewLines(expressionLines).collect { case Literal(l) => l }
      if ( elines.length == 1 )
        List(s"$before$expressionLines$after")
      else {
        val firstLine = elines.take(1).map(x => s"$before$x")
        val middleLines = elines.drop(1).dropRight(1).map(x => s"$prefix$x")
        val lastLine = elines.takeRight(1).map(x => s"$prefix$x$after")
        firstLine ::: middleLines ::: lastLine
      }
    }

    @tailrec
    private def linify(elements: List[Element], cur: Option[Line] = None, acc: List[Line] = Nil): Iterable[Line] =
      (elements, cur) match {
        case (Nil, l) =>
          acc ::: l.toList
        case (NewLine :: t, Some(l)) =>
          linify(t, None, acc ::: List(l))
        case ((h: Literal) :: t, None) =>
          linify(t, Some(Line(h, Nil)), acc)
        case ((he: Expression) :: (hl: Literal) :: t, Some(l)) =>
          linify(t, Some(l.copy(tail = l.tail ::: List((he, hl)))), acc)
      }


    def sarong(args: Any*): String = {

      // Interleave the incoming expressions with their interstitial text.

      val rawElements: Iterable[RawElement] = interleave(s.parts.map(Literal), args.map(Expression)).toList

      // Expand strings into individual lines (strings without newlines) and their newlines.

      val elements: Iterable[Element] = rawElements flatMap {
        case Literal(s) => findNewLines(s)
        case e: Expression => List(e)
      }

      // Now, group the flattened stream of elements into lines (grouped by newline).

      val lines = linify(elements.toList)

      // Then, expand each of the lines into a string without newlines. One line can become many strings if it has
      // multiline or iterable expressions. Each line has to be represented as a different string here.

      val expanded = lines.map(_.toList).flatMap {
        case List(Literal(s)) => List(s)
        case l @ List(Literal(b), Expression(e), Literal(a)) =>
          e match {
            case xx: Iterable[_] => xx.map(_.toString).flatMap(toLines(b, _, a))
            case x => toLines(b, x.toString, a)
          }
        case l =>
          l.foldLeft(List("")) {
            case (h :: t, Literal(s)) =>
              ( h + s ) :: t
            case (h :: t, Expression(e)) =>
              toLines(h, e.toString, "").reverse ::: t
          }.reverse
      }

      // Slam it all together and trim it up into a nice bounding box.

      expanded.mkString("\n").boundingBox
    }
  }

  implicit class BoundingBox(s: String) {
    def boundingBox: String = {
      @tailrec
      def dropTrailingEmptyLines(
                                  in: Iterable[String],
                                  buffer: Iterable[String] = Iterable.empty,
                                  acc: Iterable[String] = Iterable.empty
                                ): Iterable[String] =
        if (in.isEmpty)
          acc
        else {
          val h = in.head
          val t = in.tail
          if (h.trim.isEmpty)
            dropTrailingEmptyLines(t, buffer ++ Iterable(h), acc)
          else
            dropTrailingEmptyLines(t, Iterable.empty, acc ++ buffer ++ Iterable(h))
        }

      val allLines = dropTrailingEmptyLines(lines(s.trim).dropWhile(_.trim.isEmpty))
      val nonEmptyLines = allLines.filterNot(_.trim.isEmpty)

      if (nonEmptyLines.isEmpty)
        ""
      else {
        val prefixLength = nonEmptyLines.map(_.prefixLength(_ == ' ')).min
        val prefix = " " * prefixLength
        allLines.map(_.stripPrefix(prefix)).mkString("\n")
      }
    }
  }
}

