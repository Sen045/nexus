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

class Game(b:Board, l:String, d:Multiset[DrawTile], p : List[(List[DrawTile],Int)], pass:Int, o:Boolean, rs:Int, c:Int) {
  val board = b
  val bag = d
  val racks = p map (_._1)
  val scores = p map (_._1)
  val players = p length
  val over = o
  val racksize = rs
  def language = LanguageBank get l
  val current = c
}

object Game {
  def newGame(b:Board,lang:String,p : Int = 2, rs : Int = 7) = {
    val l = LanguageBank get lang
    val (bag,pl) = (0 to (p-1)).toList.foldLeft((l tileset, Nil:List[(List[DrawTile], Int)])){
      case ((b,l),n) => {val (l2,b2) = b random rs
			 (b2,(l2,0)::l)
		       }
    }
    new Game(b,lang,bag,pl,0,false,rs,0)
  }
/*  def trivial =
    newGame(new Board(0,0),"",0,0)*/
}

