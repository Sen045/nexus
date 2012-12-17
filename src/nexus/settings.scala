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

class Settings {
  var langdir = "lang/"
  var boarddir = "boards/"
  var language="lang/svenska.lang"
  var board="boards/standard.board"
  var separator="\n"
  var correct=true

  def parse(l:List[String]) : Unit = {
    l match {
      case Nil => {()}
      case "-l"::s::l => {
	this.parse(l)
	this.language=s
      }
      case "-ld"::s::l => {
	this.parse(l)
	this.langdir=s
      }
      case "-s"::s::l => {
	this.parse(l)
	this.separator=s
      }
      case "-b"::s::l => {
	this.parse(l)
	this.board=s
      }
      case "-bd"::s::l => {
	this.parse(l)
	this.boarddir=s
      }
      case _ => {
	this.correct=false
      }
    }
  }

  def this(l:List[String]) {
    this()
    parse(l)
  }
}
