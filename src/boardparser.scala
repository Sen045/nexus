package nexus

/** A board parser is used to read a [[nexus.Board]] from a file of the .board format. The
 *  .board file defines the extent of the board and where special tiles are located.
 *
 *  A user should not need to construct board parsers - rather, use the methods in the
 *  companion object.
 */
class BoardParser extends LanguageParser {
  private def boardPostParser : Parser[(String,(Map[String,String],Map[String,Int]))] =
    (stringParser ~ ("{" ~> entriesParser)) ^^ {case stringParser(tag)~e => (tag,e)}
  private def boardPostsParser : Parser[List[(String,(Map[String,String],Map[String,Int]))]] =
    ((boardPostParser ~ boardPostsParser) ^^ {case e~m => e::m}) | ("" ^^ {x => Nil})
  /** A parser for the board file format.
   *  @return the parser
   */
  def boardParser : Parser[Board] = boardPostsParser ^^ BoardParser.boardMaker
}

object BoardParser {
  /** Parses a board file.
   *  @param path path of the file.
   *  @return the board represented by the file.
   */
  def parse(path : String) : Board = {
    val bd = new BoardParser()
    val y = io.Source.fromFile(path) mkString;
    bd.parseAll(bd.boardParser,y) match {
      case bd.Success(p,_) => p
    }
  }

  private def boardMaker(posts:List[(String,(Map[String,String],Map[String,Int]))]) : Board = {
    val (_,(bs,bi)) = (posts find {_._1 == "board"}) getOrElse (("board",(Map.empty:Map[String,String],Map.empty:Map[String,Int])))
    val width = bi.getOrElse("width",15)
    val height = bi.getOrElse("height",width)
    val eb = new Board(width,height)
    posts.foldLeft(eb){
      case (b,("square",(s,i))) => b changeSquare (Square(i getOrElse("wordbonus",1),
							  i getOrElse("tilebonus",1),
							  s.getOrElse("start","false").toBoolean),
						   (i getOrElse("x",-1),i getOrElse("y",-1)))
      case (b,_) => b
    }
  }
}
