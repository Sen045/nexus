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
import nexus._
import nexus.predicate._

object subdic extends App {
  val settings = new Settings(args toList)
  val l = LanguageParser.parse(settings language)
  var flag = true
  val Pred = new PredicateParser(l)

  while (flag) {
    readLine(if(Console.in.ready()) "" else "- ") match {
      case null => flag = false
      case "." => flag = false
      case s =>
	Pred.parseAll(Pred.predicate,s) match {
	  case Pred.Success(p,_) => println(p.toPropagator(l dic).toStringList.map(l externalise _).mkString(settings separator))
	  case _ => println("Parsern failade :(")
	}
    }
  }
}
