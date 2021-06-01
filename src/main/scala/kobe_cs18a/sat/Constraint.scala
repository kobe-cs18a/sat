package kobe_cs18a.sat

abstract class Constraint

class CardinalityConstraint extends Constraint

case class ExactOne(xs: Seq[String]) extends CardinalityConstraint {
  override def toString = s"ExactOne(${xs.mkString(",")})"
}



