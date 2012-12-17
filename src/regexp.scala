/*
 * Hembakta reguljära uttryck eftersom jag vill kunna bygga automater som accepterar
 * deras produktioner. scala.util.automata verkar kunna åstadkomma detsamma men är
 * markerade som deprecated, FML!
 * En automat representeras som en lista av reguljära uttryck. Ett accepterande tillstånd
 * är en automat som innehåller åtminstone ett uttryck som matchar den tomma strängen
 */

package nexus

sealed abstract class Regexp {
  private def consumeAux(c : Char) : List[Option[Regexp]] = {
    this match {
      case Empty => List()
      case Fix(d) =>
	if(c==d)
	  List(Some(Empty))
	else
	  Nil
      case Any => List(Some(Empty))
      case Disj(r1,r2) =>
	(r1 consumeAux c) ++ (r2 consumeAux c)
      case Concat(r1,r2) =>
	{
	  ((r1 consumeAux c) map
	   {
	     case Some(Empty) => List(Some(r2))
	     case None => r2 consumeAux c
	     case Some(r) => List(Some(Concat(r,r2)))
	   }) flatten
	}
      case Iter(r) =>
	{
	  None :: (((r consumeAux c) map
	   {
	     case Some(Empty) => List(Some(Empty),Some(Iter(r:Regexp)))
	     case None => List()
	     case Some(r2) => List(Some(r2),Some(Concat(r2,Iter(r))))
	   }) flatten)
	}
    }    
  }

  def consume(c : Char) : List[Regexp] = {
    (((this consumeAux c) map (_ toList)) flatten) distinct
  }

  def accepting : Boolean = {
    this match {
      case Empty => true
      case Iter(_) => true
      case Concat(r1,r2) =>
	if(r1 accepting)
	  r2 accepting
	else
	  false
      case Disj(r1,r2) =>
	if(r1 accepting)
	  true
	else
	  r2 accepting
      case _ => false
    }
  }

  def distance : Int = {
    this match {
      case Empty => 0
      case Iter(_) => 0
      case Concat(r1,r2) =>
	(r1 distance) + (r2 distance)
      case Disj(r1,r2) =>
	(r1 distance) min (r2 distance)
      case Any => 1
      case Fix(_) => 1
    }
  }

  def automaton : Automaton = new Automaton(List(this))
}

case object Empty extends Regexp
case object Any extends Regexp
case class Fix(c:Char) extends Regexp
case class Iter(r:Regexp) extends Regexp
case class Disj(r1:Regexp,r2:Regexp) extends Regexp
case class Concat(r1:Regexp,r2:Regexp) extends Regexp

object Regexp {
  def fromString(s:String) : Option[Regexp] = {
    val p = new RegexpParser()
    p.parseAll(p.regexp,s) match {
      case p.Success(p,_) => Some(p)
      case _ => None
    }
  }
}
