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

sealed abstract class DrawTile {
  def >(d:DrawTile) : Boolean
  def <(d:DrawTile) : Boolean
  def <=(d:DrawTile) : Boolean
  def >=(d:DrawTile) : Boolean
}

case class DrawLetter(c:Char) extends DrawTile {
  override def > (d:DrawTile) = {
    d match {
      case DrawBlank => true
      case DrawLetter(c2) => this.c > c2
    }
  }

  override def < (d:DrawTile) = {
    d match {
      case DrawBlank => false
      case DrawLetter(c2) => this.c < c2
    }
  }

  override def >= (d:DrawTile) = {
    d match {
      case DrawBlank => true
      case DrawLetter(c2) => this.c >= c2
    }
  }

  override def <= (d:DrawTile) = {
    d match {
      case DrawBlank => false
      case DrawLetter(c2) => this.c <= c2
    }
  }
}
case object DrawBlank extends DrawTile {
  override def > (d:DrawTile) = {
    false
  }

  override def < (d:DrawTile) = {
    d match {
      case DrawBlank => false
      case DrawLetter(_) => true
    }
  }

  override def >= (d:DrawTile) = {
    d match {
      case DrawBlank => true
      case DrawLetter(_) => false
    }
  }

  override def <= (d:DrawTile) = {
    true
  }
}

object DrawTile {
  def fromChar(c:Char) =
    if(c=='?')
      DrawBlank
    else
      DrawLetter(c)
}
