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

/** A SemiMove represents a move, with the information about said move that is available
 *  to the player in the moment he/she commits to the move.
 */
sealed abstract class SemiMove {
  /** Get the squares upon which new tiles are placed by this move.
   *  @param b board on which the move is played.
   *  @return the squares upon which new tiles are placed by this move.
   */
  def touches(b:Board) : List[Square]

  /** Check if move is played on a start tile.
   *  @param b board on which the move is played.
   *  @return true iff this move is played on a start square.
   */
  def onStart(b:Board) : Boolean =  (this touches b) exists {_.start}

  /** This move including a randomly selected tile draw which may result from it.
   *  @param b board on which the move is played.
   *  @return the move.
   */
  def toMove(g:Game) : Move

  def score(b:Board,l:Language,rs:Int) : Int = 0
}

case object SPass extends SemiMove {
  override def touches(b:Board) : List[Square] = Nil
  override def toMove(g:Game) : Move =
    Move(SPass,g.racks(g.currentPlayer),Nil)
}

case class SSwap(tiles: List[DrawTile]) extends SemiMove {
  override def touches(b:Board) : List[Square] = Nil
  override def toMove(g:Game) : Move =
    Move(this,
	 (Multiset.fromTraversable(g.racks(g.currentPlayer)) -- tiles).get.toList,
	 g.bag.random(tiles length)._1
	 )
}

case class SPlay(horizontal : Boolean, coordinate : (Int,Int), tiles : List[Tile])  extends SemiMove {
  override def touches(b:Board) : List[Square] = {
    val line = {
      if(horizontal)
	b.horizontalLine(coordinate._2).drop(coordinate._1)
      else
	b.verticalLine(coordinate._1).drop(coordinate._2)}
    line.filter{_._2.isEmpty}.take(tiles length).map{_._1}
  }

  private def touchesTilesAux(b:Board) : List[Tile] = {    
    b tile coordinate match {
      case None =>
	if(tiles isEmpty)
	  Nil
	else
	  SPlay(horizontal,
		if(horizontal)
		  (coordinate._1 + 1, coordinate._2)
		else
		  (coordinate._1, coordinate._2 + 1),
		tiles tail) touchesTilesAux b
      case Some(t) =>
	t::(SPlay(horizontal,
		  if(horizontal)
		    (coordinate._1 + 1, coordinate._2)
		  else
		    (coordinate._1, coordinate._2 + 1),
		  tiles) touchesTilesAux b)
    }
  }

  def touchesTiles(b:Board) : List[Tile] = {
    val previousCoord = {
      if(horizontal)
	(coordinate._1 - 1, coordinate._2)
      else
	(coordinate._1, coordinate._2 -1)}
    if((b tile previousCoord) isDefined)
      SPlay(horizontal,previousCoord, tiles) touchesTiles b
    else
      this touchesTilesAux b
  }

  def touchesCoords(b:Board) : List[(Int,Int)] = {    
    (tiles,b square coordinate, b tile coordinate) match {
      case (Nil,_,_) => Nil
      case (_,None,_) => Nil
      case (_::r,Some(_),None) =>
	coordinate :: (SPlay(horizontal,
			     if(horizontal)
			       (coordinate._1 +1,coordinate._2)
			     else
			       (coordinate._1, coordinate._2 + 1),
			     r) touchesCoords b)
      case (l,_,Some(_)) =>
	SPlay(horizontal,
	      if(horizontal)
		(coordinate._1 +1,coordinate._2)
	      else
		(coordinate._1, coordinate._2 + 1),
	      l) touchesCoords b
    }
  }

  override def toMove(g:Game) : Move =
    Move(this,
	 (Multiset.fromTraversable(g.racks(g.currentPlayer)) -- (tiles map (_ toDrawTile))).get.toList,
	 g.bag.random(tiles length)._1
	 )

  private def hull(b:Board) = {
    if(horizontal) {
      b.horizontalLine(coordinate._2).drop(coordinate._1 - 1).take((tiles length) +
								   (2 min (coordinate._1 +1))) ++
      b.horizontalLine(coordinate._2 + 1).drop(coordinate._1).take(tiles length) ++
      b.horizontalLine(coordinate._2 - 1).drop(coordinate._1).take(tiles length)
    }
    else {
      b.verticalLine(coordinate._1).drop(coordinate._2 - 1).take((tiles length) +
								 (2 min (coordinate._2 + 1))) ++
      b.verticalLine(coordinate._1 + 1).drop(coordinate._2).take(tiles length) ++
      b.verticalLine(coordinate._1 - 1).drop(coordinate._2).take(tiles length)
    }
  }

  /** Check if move connects to already played tiles.
   *  @param b board on which the move is played.
   *  @return true iff this move is connected to already played tiles.
   */
  def hooks(b:Board) : Boolean = {
    hull(b) exists {_._2 isDefined}
  }

  def valid(b:Board) = {
    if(hooks(b))
      (touches(b).length == tiles.length) && b.tile(coordinate).isEmpty
    else
      onStart(b) && (tiles.length > 1) && (touches(b).length == tiles.length) && b.tile(coordinate).isEmpty
  }

  override def score(b:Board,l:Language,rs:Int) : Int = {
    val hookscores = (tiles zip (this touchesCoords b)).foldRight(0){
      case ((t,c),n) =>
	SPlay(!horizontal,c,t::Nil) touchesTiles b match {
	  case Nil => n
	  case tiles2 => {
	    val oldtilescore =  tiles2.foldRight(0){case (t,n) => n + (l.tilevalue(t))}
	    val newtilescore = b.square(c).get.tilebonus * l.tilevalue(t)
	    val multiplier = b.square(c).get.wordbonus
	    n + ((oldtilescore+newtilescore)*multiplier)
	  }
	}
    }
    val ttiles = touchesTiles(b)
    if((tiles.length > 1) || !(touchesTiles(b).isEmpty)) {
      val tsquares = touches(b)
      val newtilescore = (tiles zip tsquares).foldRight(0){case ((t,s),n) => n + (s.tilebonus * l.tilevalue(t))}
      val oldtilescore =  ttiles.foldRight(0){case (t,n) => n + (l tilevalue t)}
      val multiplier = tsquares.foldRight(1){case (s,n) => n * s.wordbonus}
      val bingo = if(tiles.size == rs) 50 else 0
      ((newtilescore+oldtilescore)*multiplier) + bingo + hookscores
    } else
      hookscores
  }
}

object SemiMove {
  def swap(tiles: List[DrawTile]) =
    SSwap(tiles sortWith (_ < _))
}
