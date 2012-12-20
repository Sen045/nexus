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

import scala.util.Random

/** A multiset is a partial function from its element type to the natural numbers.
 *  Intuitively, this represents a count of the number of copies of each element that are
 *  in the multiset.
 *
 *  Multiset(m) contains n copies of a if m(a) = Some(n).
 *  If m(a) = None, a is outside the domain of the multiset.
 *
 *  As an invariant, We require that if m(a) = Some(n), n > 0.
 */
case class Multiset[A] private (b : Map[A,Int]) {
  /** Size of the multiset.
   *  @return the number of elements in the multiset.
   */
  def size : Int = this.b.foldRight(0)(_._2 + _)

  /** Add an element to the multiset.
   *  @param t element to be added.
   *  @return the multiset with one more copy of t.
   */
  def +(t:A) = {
    this.b get t match {
      case None => Multiset(this.b + (t -> 1))
      case Some(n) => Multiset(this.b + (t -> (n+1)))
    }
  }

  /** Adds n elements to the multiset.
   *  @param t element to be added.
   *  @param n number of elements to add.
   *  @return the multiset with one more copy of t.
   */
  def +++(t:A, n:Int) : Multiset[A] = {
    if(n>=0)
      this.b get t match {
	case None => Multiset(this.b + (t -> n))
	case Some(m) => Multiset(this.b + (t -> (n+m)))
      }
    else
      this
  }

  /** Adds the elements of a traversable collection to this multiset.
   *  @param t collection to be added.
   *  @return the multiset with the elements of t added.
   */
  def ++(t:Traversable[A]) = {
    t.foldLeft(this)(_ + _)
  }

  /** Removes an element to the multiset.
   *  @param t element to be removes.
   *  @return the multiset with one less copy of t if there is at least one, None otherwise.
   */
  def -(t:A) : Option[Multiset[A]] = {
    this.b get t match {
      case None => None
      case Some(0) => None
      case Some(1) => Some(Multiset(this.b - t))
      case Some(n) => Some(Multiset(this.b + (t -> (n-1))))
    }
  }

  /** Removes the elements of a traversable collection to this multiset.
   *  @param t collection to remove.
   *  @return the multiset with the elements of t removed if they exist, None otherwise.
   */
  def --(t:Traversable[A]) : Option[Multiset[A]] = {
    t.foldLeft(Some(this):Option[Multiset[A]]){case (b, t) => b flatMap (_ - t)}
  }

  /** Get the number of copies of an element.
   *  @param t the element
   *  @return the number of copies of t, or None if t is outside the domain
   */
  def get(t:A) = {
    this.b get t
  }

  /** Get the n:th element of the multiset. Note that we enforce no particular ordering on
   *  the elements, so which element is returned may depend on implementation details.
   *  @param n number of the element.
   *  @return the n:th elemnt of the multiset.
   */
  private def nth(n:Int) : Option[A] = {
    this.b headOption match {
      case None => None
      case Some((a,m)) => {
	if(n < m)
	  Some(a)
	else
	  Multiset(this.b - a) nth (n-m)
      }
    }
  }

  /** Get a random element from the multiset.
   *  @return a random element from the multiset, or None if there are no elements.
   */
  def random : Option[A] = {
    size match {
      case 0 => None
      case n => nth(Random.nextInt(n))
    }
  }

  /** Get a number of random elements from the multiset. If not enough elements exist,
   *  get as many as there are.
   *  @param n number of elements to get.
   *  @return n random elements from the multiset, and the multiset without said elements.
   */
  def random(n:Int) : (List[A], Multiset[A]) = {
    if(n>0)
      random match {
	case Some(e) => {
	  val (l,m) = ((this - e) get) random (n-1)
	  (e::l,m)
	}
	case None => (Nil,this)
      }
    else
      (Nil,this)
  }

  /** All elements of the multiset in a list.
   *  @return list of all elements in this.
   */
  def toList : List[A] =
    b.keys.foldLeft(Nil:List[A]){case (l,k) => List.fill(b apply k)(k) ++ l}

  /** Determine whether this multiset is a superset of t.
   *  @param t a traversable collection
   *  @return true iff this multiset is a superset of t.
   */
  def supersetOf(t:Traversable[A]) : Boolean = {
    if(t.isEmpty)
      true
    else
      get(t head) match {
	case None => false
	case Some(n) =>
	  if(n>0)
	    (this - t.head).get supersetOf t.tail
	  else
	    false
      }
  }
}

object Multiset {
  /** Construct the empty multiset.
   *  @return the empty multiset.
   */
  def empty[A] : Multiset[A] = Multiset(Map.empty : Map[A,Int])

  /** Construct a multiset from a traversable collection.
   *  @param t collection to construct from.
   *  @return t as a multiset.
   */
  def fromTraversable[A](t:Traversable[A]) = {
    empty ++ t
  }

  /** Construct a tile bag from a traversable collection of characters.
   *  @param t collection to construct from.
   *  @return t as tile bag.
   */
  def fromChars(t:Traversable[Char]) : Multiset[DrawTile]= {
    empty ++ (t map (DrawTile.fromChar _))
  }
}
