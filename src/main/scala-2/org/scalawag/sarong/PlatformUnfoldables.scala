package org.scalawag.sarong

private[sarong] trait PlatformUnfoldables {

  implicit class TraversableOnceOps(me: TraversableOnce[_]) {
    def unfold: Unfoldable = TraversableOnceUnfoldable(me)
  }

  final case class TraversableOnceUnfoldable(me: TraversableOnce[_]) extends Unfoldable {
    override val iterator: Iterator[_] = me.toIterator
    override def toString: String = me.toString
  }
}
