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

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

class RegexpParser extends RegexParsers with PackratParsers {
  lazy val charParser : PackratParser[Char] = """[^\.\|\(\)\*]""".r ^^ {_ head}
  lazy val empty : PackratParser[Regexp] = "" ^^ {x => Empty}
  lazy val brackets : PackratParser[Regexp] = "(" ~> nregexp <~ ")"
  lazy val concat : PackratParser[Regexp] = nregexp ~ nregexp ^^ {case r1~r2 => Concat(r1,r2)}
  lazy val any : PackratParser[Regexp] = "." ^^ {x => Any}
  lazy val disj : PackratParser[Regexp] = nregexp ~ "|" ~ nregexp ^^ {case r1~"|"~r2 => Disj(r1,r2)}
  lazy val iter : PackratParser[Regexp] = nregexp <~ "*" ^^ {Iter(_)}
  lazy val nregexp : PackratParser[Regexp] = iter | disj | concat | brackets | any | (charParser  ^^ {Fix(_)})
  lazy val regexp : PackratParser[Regexp] = nregexp | empty
}
