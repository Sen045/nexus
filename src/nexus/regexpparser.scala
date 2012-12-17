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
