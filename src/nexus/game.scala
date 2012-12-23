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
  def board = b
  def bag = d
  val racks = p map (_._1)
  val scores = p map (_._2)
  val players = p length
  def over = o
  def racksize = rs
  def language = LanguageBank get l
  def currentPlayer = c
  def passes = pass

  def nextPlayer : Int =
    (currentPlayer + 1) % players

  def finalise : Game = {
    if(over)
      this
    else if(passes >= 6)
      new Game(board,l,bag,p map {case (l,i) => (l,i - l.foldLeft(0)(_ + language.tilevalue(_)))},passes,true,racksize,nextPlayer)
    else /* todo: om spelet slutar inte genom passar */
      this
  }

  def validateMove(m:SemiMove) : Boolean = {
    if(over)
      false
    else {
      m match {
	case SPass => true
	case SSwap(tiles) => {
	  (bag.size >= racksize) &&
	  (Multiset.fromTraversable(racks(currentPlayer)) supersetOf tiles)
	}
	case SPlay(horizontal,coordinate,tiles) => {
	  (SPlay(horizontal,coordinate,tiles) valid board) &&
	  (Multiset.fromTraversable(racks(currentPlayer)) supersetOf (tiles map (_.toDrawTile)))
	}
      }    
    }
  }

  def validateMove(m:Move) : Boolean = {
    if(over)
      false
    else {
      m move match {
	case SPass => {
	  (Multiset.fromTraversable(m leave) ==
	    Multiset.fromTraversable(racks(currentPlayer))) &&
	  m.draw.isEmpty
	}
	case SSwap(tiles) => {
	  (Multiset.fromTraversable(m.leave ++ tiles) ==
	    Multiset.fromTraversable(racks(currentPlayer))) &&
	  (bag -- m.draw).isDefined &&
	  (m.draw.size == tiles.size)
	  bag.size >= racksize
	}
	case SPlay(horizontal,coordinate,tiles) => {
	  (SPlay(horizontal,coordinate,tiles) valid board) &&
	  (Multiset.fromTraversable(m.leave ++ (tiles.map(_ toDrawTile))) ==
	    Multiset.fromTraversable(racks(currentPlayer))) &&
	  (m.draw.size == (tiles.size min bag.size))
	}
      }    
    }
  }

  def applyMove(m:Move) : Option[Game] = {
    if(validateMove(m))
      Some((m move match {
	case SPass => new Game(board,l,bag,p,passes+1,over,racksize,nextPlayer)
	case SSwap(tiles) => {
	  new Game(board,
		   l,
		   (bag -- m.draw).get ++ tiles,
		   p updated (currentPlayer,(m.leave ++ m.draw,scores(currentPlayer))),
		   passes+1,
		   over,
		   racksize,
		   nextPlayer)
	}
	case SPlay(horizontal,coordinate,tiles) => {
	  new Game(board applyMove m.move,
		   l,
		   (bag -- m.draw) get,
		   p updated (currentPlayer,
			      (m.leave ++ m.draw,
			       scores(currentPlayer) + (m.move score (board,language,racksize)))),
		   0,
		   over,
		   racksize,
		   nextPlayer)
	}
      }) finalise)
    else
      None
  }

  def applyMove(m:SemiMove) : Option[Game] = {
    if(validateMove(m))
      applyMove(m toMove this)
    else
      None
  }
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
}

