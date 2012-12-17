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

/** A class for non-deterministic finite automata which transition between states by consuming
 * sequences of characters. States are either accepting or not.
 * 
 *  An automaton (and its current state) is represented as a list of regular expressions. An
 *  automaton is in an accepting state if one of its elements match the empty string. An
 *  automaton in a junk state is represented by the empty list.
 */
class Automaton(l:List[Regexp]) {
  /** Consume a character.
   *  @param c the character to consume
   *  @return an automaton whose state is the
   *          state of this automaton after
   *          consuming c.
   */
  def consume(c:Char) : Automaton = {
    new Automaton(this.l map (_ consume c) flatten)
  }

  /** Check whether the automaton is in an accepting state.
   *  @return true iff the automaton is in an accepting state.
   */
  def accepting : Boolean =
    this.l exists (_ accepting)

  /** Checks whether the automaton is in a junk state.
   *  @return true iff the automaton is in a junk state.
   */
  def junk : Boolean =
    this.l isEmpty

  /** Get the smallest distance to an accepting state.
   *  @return None iff this automaton is in a junk state.
   *          Some(n), where n is the least number of
   *          characters that must be consumed before an
   *          accepting state is reached.
   */
  def distance : Option[Int] = {
    def myMin(r:Regexp,n:Option[Int]) = {
      n match {
	case None => Some(r distance)
	case Some(m) => Some((r distance) min m)
      }
    }
    this.l.foldRight(None:Option[Int])(myMin _)
  }
}
