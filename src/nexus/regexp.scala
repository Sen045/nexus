/** This file is part of Nexus, which is Copyright 2012 Johannes Åman Pohjola.
 *
 *  Nexus is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, version 3.
 *
 *  Nexus is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Nexus.  If not, see <http://www.gnu.org/licenses/>.
 */
package nexus

/** A class for regular expressions, whose primary intended use is to
 *  construct an [[nexus.Automaton]] for consuming its productions.
 *
 *  The reason why I have chosen to reinvent this particular wheel is that I
 *  can't figure out how to use the regular expressions of the standard
 *  library to construct automatons, so that I can consume characters one at
 *  a time.
 * 
 *  While the WordBerrySethi class of the standard library could possibly be
 *  used for this purpose, it is unfortunately deprecated, so I would rather
 *  not depend on it.
 */
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

  /** Consume a character.
   *  @param p character to consume.
   *  @return a list of regular expression such that for every string s that
   *          one of them can produce, c^s can be produced by this regular
   *          expression, and vice versa.
   */
  def consume(c : Char) : List[Regexp] = {
    (((this consumeAux c) map (_ toList)) flatten) distinct
  }

  /** Determine if this regular expression can produce the empty string.
   *  @return true iff this can produce the empty string.
   */
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

  /** Compute the length of the shortest string this regular expression can
   *  produce.
   *  @return length of the shortest string this regular expression can produce.
   */
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

  /** Construct the corresponding automaton.
   *  @return an automaton which accepts the productions of p.
   */
  def automaton : Automaton = new Automaton(List(this))
}

/** Empty is a [[nexus.Regexp]] which can produce only the empty string.
 */
case object Empty extends Regexp
/** Any is a [[nexus.Regexp]] which can produce any single-character string.
 */
case object Any extends Regexp
/** Fix(c) is a [[nexus.Regexp]] which can produce only the string consisting
 *  of the single-character string c.
 */
case class Fix(c:Char) extends Regexp
/** Iter(r) is a [[nexus.Regexp]] which can produce either the empty string,
 *  or s^s', where r can produce s and Iter(r) can produce s'.
 */
case class Iter(r:Regexp) extends Regexp
/** Disj(r1,r2) is a [[nexus.Regexp]] which can produce all strings such that
 *  either r1 or r2 can produce them.
 */
case class Disj(r1:Regexp,r2:Regexp) extends Regexp
/** Concat(r1,r2) is a [[nexus.Regexp]] which can produce all strings s1^s2 such
 *  that r1 can produce s1 and r2 can produce s2.
 */
case class Concat(r1:Regexp,r2:Regexp) extends Regexp

object Regexp {
  /** Construct a regular expression from a string. See [[nexus.RegexpParser]]
   *  for details.
   *  @return the regular expression which s represents, or NONE if it cannot
   *          be interpreted as a regular expression.
   */
  def fromString(s:String) : Option[Regexp] = {
    val p = new RegexpParser()
    p.parseAll(p.regexp,s) match {
      case p.Success(p,_) => Some(p)
      case _ => None
    }
  }
}
