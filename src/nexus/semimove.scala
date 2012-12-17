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
}

case object SPass extends SemiMove {
  override def touches(b:Board) : List[Square] = Nil
}

case class SSwap(tiles: List[DrawTile]) extends SemiMove {
  override def touches(b:Board) : List[Square] = Nil
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

  private def hull(b:Board) = {
    if(horizontal) {
      b.horizontalLine(coordinate._2).drop(coordinate._1 - 1).take((tiles length) + 2) ++
      b.horizontalLine(coordinate._2 + 1).drop(coordinate._1).take(tiles length) ++
      b.horizontalLine(coordinate._2 - 1).drop(coordinate._1).take(tiles length)
    }
    else {
      b.verticalLine(coordinate._1).drop(coordinate._2 - 1).take((tiles length) + 2) ++
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
}

object SemiMove {
  def swap(tiles: List[DrawTile]) =
    SSwap(tiles sortWith (_ < _))
}
