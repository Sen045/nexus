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

import nexus.trie._

case class Dictionary private (dic : Vector[Trie]) {
  def +(w:Seq[Char]) : Dictionary = {
    val d = this.dic
    val n = d(w.size) + w
    Dictionary(d.updated(w.size,n))
  }

  def +(w:String) : Dictionary = {
    val z : Seq[Char] = w
    this + z
  }

  def ++(ws:TraversableOnce[Seq[Char]]) : Dictionary = {
    ws.foldLeft(this)(_ + _)
  }

  def +++(ws:TraversableOnce[String]) : Dictionary = {
    ws.foldLeft(this)(_ + _)
  }

  def toCharListList : List[List[Char]] = {
    this.dic.map(_ toCharListList).flatten.toList
  }

  def toStringList : List[String] = {
    this.dic.map(_ toStringList).flatten.toList
  }

  def union(d: Dictionary) = {
    Dictionary(this.dic.zipAll(d.dic, TrieBottom, TrieBottom) map (x => x._1 union x._2))
  }

  def intersection(d: Dictionary) = {
    Dictionary(this.dic.zipAll(d.dic, TrieBottom, TrieBottom) map (x => x._1 intersection x._2))
  }

  def complement(d: Dictionary) = {
    Dictionary(this.dic.zipAll(d.dic, TrieBottom, TrieBottom) map (x => x._1 complement x._2))
  }

  def query(s : Seq[Char]) = {
    this.dic(s.size) query s
  }
}

object Dictionary {
  type Propagator = Dictionary => Dictionary

  val empty = Dictionary(Vector.empty)

  def empty(maxsize : Int = 29) = {
    Dictionary(Vector.fill(maxsize)(TrieBottom))
  }

  def id(d:Dictionary) : Dictionary = d

  def leneq(l:Int)(d:Dictionary) : Dictionary = {
    val a = d.dic.zipWithIndex.map(z => if(z._2 != l) TrieBottom else z._1)    
    Dictionary(a)
  }

  def lenneq(l:Int)(d:Dictionary) : Dictionary = {
    val a = d.dic.zipWithIndex.map(z => if(z._2 == l) TrieBottom else z._1)    
    Dictionary(a)
  }

  def lengeq(l:Int)(d:Dictionary) : Dictionary = {
    val a = d.dic.zipWithIndex.map(z => if(z._2 < l) TrieBottom else z._1)    
    Dictionary(a)
  }

  def lenleq(l:Int)(d:Dictionary) : Dictionary = {
    val a = d.dic.zipWithIndex.map(z => if(z._2 > l) TrieBottom else z._1)    
    Dictionary(a)
  }

  def length(l:Int)(d:Dictionary) : Dictionary = {
    val a = d.dic.zipWithIndex.map(z => if(z._2 <= l) TrieBottom else z._1)    
    Dictionary(a)
  }

  def lenlth(l:Int)(d:Dictionary) : Dictionary = {
    val a = d.dic.zipWithIndex.map(z => if(z._2 >= l) TrieBottom else z._1)    
    Dictionary(a)
  }

  def prefix(s:Seq[Char])(d:Dictionary) : Dictionary = {
    /* PRE: |s| <= depth of t */
    val l = s.length
    val a = d.dic.zipWithIndex.map(z => if(z._2 < l) TrieBottom else Trie.prefix(s)(z._1))    
    Dictionary(a)
  }

  def equals(s:Seq[Char])(d:Dictionary) : Dictionary = {
    /* PRE: |s| = depth of t */
    val l = s.length
    val a = d.dic.zipWithIndex.map(z => if(z._2 != l) TrieBottom else Trie.equals(s)(z._1))    
    Dictionary(a)
  }

  def suffix(s:Seq[Char])(d:Dictionary) : Dictionary = {
    val l = s.length
    val a = d.dic.zipWithIndex.map(z => if(z._2 < l) TrieBottom else Trie.suffix(s)(z._1)(z._2 - l))
    Dictionary(a)
  }

  def has(s:Seq[Char])(d:Dictionary) : Dictionary = {
    val l = s.length
    val a = d.dic.zipWithIndex.map(z => if(z._2 < l) TrieBottom else Trie.has(s)(z._1)(z._2 - l))
    Dictionary(a)
  }

  def matches(r:Regexp)(d:Dictionary) : Dictionary = {
    val a = r automaton
    val dic = d.dic.zipWithIndex.map(z => Trie.matches(a)(z._1)(z._2))
    Dictionary(dic)
  }

  def drawFrom(s:Traversable[Char])(d:Dictionary) : Dictionary = { 
    val l = Multiset.fromChars(s)
    val n = l size
    val a = d.dic.zipWithIndex.map(z => if(z._2 > n) TrieBottom else Trie.drawFrom(l)(z._1))
    Dictionary(a)
  }
}
