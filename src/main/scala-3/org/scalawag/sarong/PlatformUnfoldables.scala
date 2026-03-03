package org.scalawag.sarong

private[sarong] trait PlatformUnfoldables {

  implicit class IterableOnceOps(me: IterableOnce[_]) {
    def unfold: Unfoldable = IterableOnceUnfoldable(me)
  }

  final case class IterableOnceUnfoldable(me: IterableOnce[_]) extends Unfoldable {
    override val iterator: Iterator[_] = me.iterator
    override def toString: String = me.toString
  }
}
