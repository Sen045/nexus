import nexus._

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
