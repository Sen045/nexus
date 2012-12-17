package nexus
package predicate

sealed abstract class Predicate
{  
  def toPropagator(D:Dictionary) : Dictionary = {
    this match {
      case Prefix(s) =>
	Dictionary.prefix(s)(D)
      case Suffix(s) =>
	Dictionary.suffix(s)(D)
      case Equals(s) =>
	Dictionary.equals(s)(D)
      case Has(s) =>
	Dictionary.has(s)(D)
      case LenEq(n) =>
	Dictionary.leneq(n)(D)
      case LenNeq(n) =>
	Dictionary.lenneq(n)(D)
      case LenGeq(n) =>
	Dictionary.lengeq(n)(D)
      case LenLeq(n) =>
	Dictionary.lenleq(n)(D)
      case LenGth(n) =>
	Dictionary.length(n)(D)
      case LenLth(n) =>
	Dictionary.lenlth(n)(D)
      case DrawFrom(s) =>
	Dictionary.drawFrom(s)(D)
      case Matches(r) =>
	Dictionary.matches(r)(D)      
      case And(p1,p2) =>
	p1 toPropagator (p2 toPropagator D)
      case Or(p1,p2) =>
	(p1 toPropagator D) union (p2 toPropagator D)
      case Not(p) =>
	D complement (p toPropagator D)
    }
  }
}
case class Prefix(s: Seq[Char]) extends Predicate
case class Suffix(s: Seq[Char]) extends Predicate
case class Equals(s: Seq[Char]) extends Predicate
case class Has(s: Seq[Char]) extends Predicate
case class LenEq(n: Int) extends Predicate
case class LenNeq(n: Int) extends Predicate
case class LenGeq(n: Int) extends Predicate
case class LenLeq(n: Int) extends Predicate
case class LenGth(n: Int) extends Predicate
case class LenLth(n: Int) extends Predicate
case class Matches(r: Regexp) extends Predicate
case class DrawFrom(s: Seq[Char]) extends Predicate
case class And(P1: Predicate, P2: Predicate) extends Predicate
case class Or(P1: Predicate, P2: Predicate) extends Predicate
case class Not(P: Predicate) extends Predicate

object Predicate {
  def comp(s:String)(n:Int) = {
    s match {
      case ">=" => LenGeq(n)
      case ">" => LenGth(n)
      case "=" => LenEq(n)
      case "<=" => LenLeq(n)
      case "<" => LenLth(n)
      case "<>" => LenNeq(n)
      case "!=" => LenNeq(n)
      case _ => LenEq(-1) /* raise exception */
    }
  }
  def compInv(s:String)(n:Int) = {
    s match {
      case ">=" => comp("<=")(n)
      case ">" => comp("<")(n)
      case "<=" => comp(">=")(n)
      case "<" => comp(">")(n)
      case _ => comp(s)(n)
    }
  }
}
