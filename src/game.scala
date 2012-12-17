package nexus

class Game(b:Board, l:String, d:Multiset[DrawTile], p : List[(List[DrawTile],Int)], pass:Int, o:Boolean, rs:Int, c:Int) {
  val board = b
  val bag = d
  val racks = p map (_._1)
  val scores = p map (_._1)
  val players = p length
  val over = o
  val racksize = rs
  def language = LanguageBank get l
  val current = c
}

object Game {
  def newGame(b:Board,lang:String,p : Int = 2, rs : Int = 7) = {
    val l = LanguageBank get lang
    val (bag,pl) = (0 to (p-1)).toList.foldLeft((l tileset, Nil:List[(List[DrawTile], Int)])){
      case ((b,l),n) => {val (l2,b2) = b random rs
			 (b2,(l2,0)::l)
		       }
    }
    new Game(b,lang,bag,pl,0,false,rs,0)
  }
/*  def trivial =
    newGame(new Board(0,0),"",0,0)*/
}

