package day18

import scala.util.parsing.combinator.RegexParsers
import scala.annotation.tailrec

object Solution extends RegexParsers {

  sealed trait SnailfishNumber {
    def +(other: SnailfishNumber): SnailfishNumber = Pair(this, other).reduce
    def split: Option[Pair]
    def explode: Option[SnailfishNumber] = this.exploded(0).map(_.newNumber)
    def exploded(depth: Int): Option[ExplodeResult]
    def add(value: Int, isLeft: Boolean): SnailfishNumber

    final def reduce: SnailfishNumber = this.explode match {
      case Some(number) => number.reduce
      case None =>
        this.split match {
          case Some(number) => number.reduce
          case None         => this
        }
    }

    def magnitude: Int
  }

  case class Number(n: Int) extends SnailfishNumber {
    override def split: Option[Pair] =
      if (n >= 10)
        Some(Pair(Number((this.n.toFloat / 2).floor.toInt), Number((this.n.toFloat / 2).ceil.toInt)))
      else
        None

    override def exploded(depth: Int): Option[ExplodeResult] = None

    override def add(value: Int, isLeft: Boolean): SnailfishNumber = this.copy(n + value)

    override def magnitude: Int = n
  }

  case class Pair(l: SnailfishNumber, r: SnailfishNumber) extends SnailfishNumber {
    override def split: Option[Pair] = l.split.map(Pair(_, r)) orElse r.split.map(Pair(l, _))

    override def exploded(depth: Int): Option[ExplodeResult] = (this, depth) match {
      case (Pair(Number(left), Number(right)), 4) => Some(ExplodeResult(Some(left), Number(0), Some(right)))
      case (Pair(left, right), _) =>
        left
          .exploded(depth + 1)
          .map { case ExplodeResult(al, nn, ar) =>
            ExplodeResult(al, Pair(nn, ar.map(right.add(_, true)).getOrElse(right)), None)
          } orElse
          right
            .exploded(depth + 1)
            .map { case ExplodeResult(al, nn, ar) =>
              ExplodeResult(None, Pair(al.map(left.add(_, false)).getOrElse(left), nn), ar)
            }
    }

    override def add(value: Int, isLeft: Boolean): SnailfishNumber =
      if (isLeft) Pair(l.add(value, true), r) else Pair(l, r.add(value, false))

    override def magnitude: Int = 3 * l.magnitude + 2 * r.magnitude
  }

  case class ExplodeResult(addToLeft: Option[Int], newNumber: SnailfishNumber, addToRight: Option[Int])

  def parse(s: String): SnailfishNumber = {
    def number: Parser[SnailfishNumber] = (
      "\\d+".r ^^ (_.toInt) ^^ Number.apply |
        "[" ~> number ~ "," ~ number <~ "]" ^^ { case left ~ _ ~ right => Pair(left, right) }
    )
    parseAll(number, s).get
  }

  def sum(numbers: Seq[SnailfishNumber]): SnailfishNumber = numbers.reduce(_ + _)

}
