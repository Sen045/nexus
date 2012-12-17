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
package predicate

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

class PredicateParser(l:Language) extends RegexParsers with PackratParsers {
  val language = l
  val stringQParser = """\'(\S+)\'""".r
  val stringDQParser = """\"(\S+)\"""".r
  val stringParser = """([^\)\s]+)""".r
  val regexLexer = """([^\(\)\s]+)""".r
  val regexDQLexer = """\"([^\s]+)\"""".r
  val regexQLexer = """'([^\s]+)'""".r
  lazy val intParser : PackratParser[Int] = """(0|[1-9]\d*)""".r ^^ {_.toInt}
  val comp = "<>" | ">=" | ">" | "=" | "<=" | "<" | "!="
  lazy val matches : PackratParser[Predicate] =
    "matches" ~>(regexQLexer|regexDQLexer|regexLexer) ^^ {case regexQLexer(s) => Matches(Regexp.fromString(language internalise s).get)
							  case regexDQLexer(s) => Matches(Regexp.fromString(language internalise s).get)
							  case s => Matches(Regexp.fromString(language internalise s).get)}
  lazy val prefix : PackratParser[Predicate] =
    "prefix" ~>(stringParser|stringQParser|stringDQParser) ^^ {case stringQParser(s) => Prefix(language internalise s)
							       case stringDQParser(s) => Prefix(language internalise s)
							       case s => Prefix(language internalise s)}
  lazy val suffix : PackratParser[Predicate] =
    "suffix" ~>(stringParser|stringQParser|stringDQParser) ^^ {case stringQParser(s) => Suffix(language internalise s)
							       case stringDQParser(s) => Suffix(language internalise s)
							       case s => Suffix(language internalise s)}
  lazy val equals : PackratParser[Predicate] =
    "equals" ~>(stringParser|stringQParser|stringDQParser) ^^ {case stringQParser(s) => Equals(language internalise s)
							       case stringDQParser(s) => Equals(language internalise s)
							       case s => Equals(language internalise s)}
  lazy val drawfrom : PackratParser[Predicate] =
    "drawfrom" ~>(stringParser|stringQParser|stringDQParser) ^^ {case stringQParser(s) => DrawFrom(language internalise s)
							       case stringDQParser(s) => DrawFrom(language internalise s)
							       case s => DrawFrom(language internalise s)}
  lazy val has : PackratParser[Predicate] =
    "has" ~>(stringParser|stringQParser|stringDQParser) ^^ {case stringQParser(s) => Has(language internalise s)
							       case stringDQParser(s) => Has(language internalise s)
							       case s => Has(language internalise s)}
  lazy val length : PackratParser[Predicate] =
    {
      ("length" ~> intParser) ^^ {LenEq(_)} |
      ("length" ~> (comp ~ intParser)) ^^ {case c~n => Predicate.comp(c)(n)} |
      ((intParser ~ comp) <~ "length") ~ (comp ~ intParser) ^^
      {
	case (n~c)~(c2~n2) => And(Predicate.compInv(c)(n),Predicate.comp(c2)(n2))
      } |
      ((intParser ~ comp) <~ "length") ^^
      {
	case n~c => Predicate.compInv(c)(n)	
      }
    }
  lazy val and : PackratParser[Predicate] =
    (predicate <~ "and") ~ predicate ^^ {case p~q => And(p,q)}
  lazy val or : PackratParser[Predicate] =
    (predicate <~ "or") ~ predicate ^^ {case p~q => Or(p,q)}
  lazy val not : PackratParser[Predicate] =
    "not" ~> predicate ^^ {Not(_)}
  lazy val brackets : PackratParser[Predicate] = "(" ~> predicate <~ ")"
  lazy val predicate : PackratParser[Predicate] = or | and | brackets | not | prefix | suffix | equals | has | length | matches | drawfrom
}
