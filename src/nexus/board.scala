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

/** A class which represents a game board.
 */
class Board private (board: Vector[Vector[(Square,Option[Tile])]],len : Int, hei : Int) {
  /** Get board width.
   *  @return width of the board.
   */
  def width = len

  /** Get board height.
   *  @return height of the board.
   */
  def height = hei

  /** Constructs a board consisting of only empty squares, none of whom are bonus or start
   *  squares.
   *  @param width width of the board, must be non-negative
   *  @param height height of the board, must be non-negative
   *  @return a width*height sized empty board.
   */
  def this (width:Int, height:Int) = {
    this(Vector.fill(height,width)(Square(1,1,false),None),width, height)
  }

  /** Get a square from the board.
   *  @param x x-coordinate of the square
   *  @param y x-coordinate of the square
   *  @return the square at (x,y), or NONE if (x,y) is outside the board
   */
  def square(x:Int, y:Int) : Option[Square] = {
    if(0<= x && x < width && 0 <= y && y < height)
      Some(this.board(y)(x)._1)
    else
      None
  }

  /** Get a square from the board.
   *  @param coordinate coordinate of the square
   *  @return the square at coordinate if one exists, NONE otherwise
   */
  def square(coordinate:(Int,Int)) : Option[Square] = {
    square(coordinate._1,coordinate._2)
  }

  /** Get a tile from the board.
   *  @param x x-coordinate of the square
   *  @param y x-coordinate of the square
   *  @return the tile at (x,y) if one exists, NONE otherwise
   */
  def tile(x:Int, y:Int) : Option[Tile] = {
    if(0<= x && x < width && 0 <= y && y < height)
      this.board(y)(x)._2
    else
      None
  }

  /** Get a tile from the board.
   *  @param coordinate coordinate of the square
   *  @return the tile at coordinate if one exists, NONE otherwise
   */
  def tile(coordinate:(Int,Int)) : Option[Tile] = {
    tile(coordinate._1,coordinate._2)
  }

  /** Get the board contents along a horizontal line.
   *  @param y y-coordinate of the line.
   *  @return the squares (and their contents) whose y-coordinate is y, in order.
   */
  def horizontalLine(y:Int) : List[(Square,Option[Tile])] = {
    if(0 <= y && y < height)
      this.board(y).toList
    else
      Nil
  }

  /** Get the board contents along a vertical line.
   *  @param x x-coordinate of the line
   *  @return the squares (and their contents) whose y-coordinate is y, in order.
   */
  def verticalLine(x:Int) : List[(Square,Option[Tile])] = {
    if(0 <= x && x < width)
      List.tabulate(this.height){n => this.board(n)(x)}
    else
      Nil
  }

  /** Place a tile on the board.
   *  @param t tile to place.
   *  @param c coordinate to place tile on.
   *  @return the board with tile t on position c.
   */
  def placeTile(t:Tile,c:(Int,Int)) : Board = {
    new Board(board.updated(c._2,board(c._2).updated(c._1,(square(c) get,Some(t)))),width,height)
  }

  /** Change a square on the bord
   *  @param s new square
   *  @param c coordinate of the square
   *  @return the board with the square s on position c
   */
  def changeSquare(s:Square,c:(Int,Int)) = {
    new Board(board.updated(c._2,board(c._2).updated(c._1,(s,tile(c)))),width,height)
  }

  /** Apply a move to the board.
   *  @param m move to apply.
   *  @return the board with m applied to it.
   */
  def applyMove(m:SemiMove) : Board = {
    m match {
      case SPlay(true,(x,y),t::r) => {
	if(tile(x,y).isEmpty)
	  placeTile(t,(x,y)) applyMove SPlay(true,(x+1,y),r)
	else
	  this applyMove SPlay(true,(x+1,y),t::r)
      }
      case SPlay(false,(x,y),t::r) => {
	if(tile(x,y).isEmpty)
	  placeTile(t,(x,y)) applyMove SPlay(false,(x,y+1),r)
	else
	  this applyMove SPlay(false,(x,y+1),t::r)
      }
      case _ => this
    }
  }

  /** Pretty-print the board.
   *  @return a string representation of the board
   */
  def mkString : String = {    
    board.map{_.map{case(_,None) => '-' case(_,Some(t)) => t mkString}.mkString(" ")}.mkString("\n")
  }
}
