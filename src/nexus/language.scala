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

class Language private (d:Dictionary, t:Multiset[DrawTile], v:Map[DrawTile,Int],c:Option[Multiset[Char]], i : String => String, x : String => String,n:String,de:String) {
  val dic = d
  val tileset = t
  val tilevalue = v
  def tilevalue(d: Tile) : Int = v apply (d toDrawTile)
  val lettercount = c
  val name = n
  val description = de
  def internalise(s:String) = i(s)
  def externalise(s:String) = x(s)
}

object Language {
  def make(d:Dictionary, t:Multiset[DrawTile], v:Map[DrawTile, Int], n:String, de:String, i: String => String = identity, x : String => String = identity) = {
    new Language(d,t,v,None,i,x,n,de)
  }
  def trivial =
    new Language(Dictionary.empty,Multiset.empty,Map.empty,None,identity,identity,"","")
}
