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

sealed abstract class Tile {
  def letter : Char
  def toDrawTile: DrawTile

  def mkString : Char
}

case class Letter(c:Char) extends Tile {
  override def letter = c
  override def mkString = c.toUpper
  override def toDrawTile = DrawLetter(c)
}
case class Blank(c:Char) extends Tile {
  override def letter = c
  override def mkString = c.toLower
  override def toDrawTile = DrawBlank
}

object Tile {
  def fromChar(c:Char) = {
    if(c isLower)
      Blank(c)
    else
      Letter(c toLower)
  }
}
