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
import scala.swing._
import scala.swing.event._
import collection.mutable.ArrayBuffer

/** The event BoardClick(p,id) signals that the p:th square of the board id was clicked.
 */
case class BoardClick(position:Int, id:Int) extends Event

/** The event ReorientMove(pos,id) signals that the move orientation on board id should be changed, and
 *  that the pointer is in the p:th square.
 */
case class ReorientMove(position:Int, id:Int) extends Event

/** The event BoardEnterSquare(p,id) signals that the p:th square of the board id was entered.
 */
case class BoardEnterSquare(position:Int, id:Int) extends Event

class GameFrame(game : Game, i : Int) extends Publisher {
  val id = i
  var g = game
  var horizontalOrientation = true
  var squares = (0 to ((g.board.width * g.board.height)-1) map (n => GameFrame.drawSquare((g.board.square(n % g.board.width,n / g.board.width).get,g.board.tile(n % g.board.width, n / g.board.width)), g.language, n, id))).toArray
  var squaresPanel = new GridPanel(g.board.height,g.board.width) {background=new java.awt.Color(60,100,60)
								  contents ++= squares
								}
  var rack = new GridPanel(1,g racksize) {background=new java.awt.Color(60,100,60)
					  contents ++= GameFrame.drawRack(g.racks(0),g language)
					}
  val dirt : ArrayBuffer[(Int,Int)] = new ArrayBuffer((g racksize)*2)
  val closegamebutton = new Button("Close game")
  val passbutton = new Button("Pass")
  val swapbutton = new Button("Swap")
  val tilefield = new TextField()
  var testlabel = new Label("test")
  var contents = new BoxPanel(Orientation.Horizontal){
    contents += new BoxPanel(Orientation.Vertical){
      contents += squaresPanel
      contents += new BoxPanel(Orientation.Horizontal){
	contents += new BorderPanel{add(rack,BorderPanel.Position.South)}
	contents += tilefield
      }
      contents += new BoxPanel(Orientation.Horizontal){
	contents += passbutton
	contents += closegamebutton
	contents += swapbutton
      }
    }
    contents += new BorderPanel{add(testlabel,BorderPanel.Position.East)}
  }

  def cleanDirt : Unit = {
    dirt.foreach {case (x,y) => {
      val n = y*g.board.width + x
      this deafTo squares(n)
      squares(n) = GameFrame.drawSquare((g.board.square(x,y).get,
					 g.board.tile(x,y))
					,g language,n,id)
      this listenTo squares(n)
      squaresPanel.contents.update(n,squares(n))
    }}
    dirt.clear
  }

  listenTo(closegamebutton, passbutton, swapbutton, tilefield)
  squares map (this listenTo _)
  reactions += {
    case ButtonClicked(`closegamebutton`) => BoardGUI.closeGame(id)
    case ButtonClicked(`passbutton`) => {
      g applyMove SPass match {
	case None =>
	  Dialog.showMessage(title = "Nope.", message = "Game is over.")
	case Some(game) => {
	  g = game	  
	}
      }
    }
    case ButtonClicked(`swapbutton`) => {
      val chars = (g.language internalise tilefield.text.toLowerCase).toList map DrawTile.fromChar
      g applyMove SSwap(chars) match {
	case None =>
	  Dialog.showMessage(title = "Nope.", message = "Illegal move." + (SSwap(chars)))
	case Some(game) => {
	  g = game
	  tilefield.text = ""
	  rack.contents.clear
	  rack.contents ++= GameFrame.drawRack(g.racks(0),g language)
	  rack.revalidate
	}
      }
    }
    case BoardClick(pos,`id`) =>
      if(tilefield.text != "") {
	cleanDirt
	val tiles = (g.language internalise tilefield.text).toList map Tile.fromChar
	val move = SPlay(horizontalOrientation,(pos % g.board.width,pos / g.board.width), tiles)
	g applyMove move match {
	  case None =>
	    Dialog.showMessage(title ="Nope.", message = "Illegal move.")
	  case Some(game) => {
	    val coords = move touchesCoords g.board
	    coords map {
	      case (x,y) => {
		val n = y*g.board.width + x
		this deafTo squares(n)
		squares(n) = GameFrame.drawSquare((game.board.square(x,y).get,
						   game.board.tile(x,y))
						  ,g language,n,id)
		this listenTo squares(n)
		squaresPanel.contents.update(n,squares(n))
	      }
	    }
	    squaresPanel.revalidate
	    g = game
	    tilefield.text = ""
	    rack.contents.clear
	    rack.contents ++= GameFrame.drawRack(g.racks(0),g language)
	    rack.revalidate
	    testlabel.text=g.scores(0).toString
	  }
	}
      }
    case ReorientMove(pos, `id`) => {
      horizontalOrientation = !horizontalOrientation
      if(tilefield.text != "") {
	cleanDirt
	val tiles = (g.language internalise tilefield.text).toList map Tile.fromChar
	val move = SPlay(horizontalOrientation,(pos % g.board.width,pos / g.board.width), tiles)
	val coords = move touchesCoords g.board
	val movelisten = false :: (List.fill(tiles.length - 1)(true))
	(coords zip tiles) zip movelisten map {
	  case (((x,y),t),ml) => {
	    val n = y*g.board.width + x
	    this deafTo squares(n)
	    squares(n) = GameFrame.drawSquare((g.board.square(x,y).get,
					       Some(t))
					      ,g language,n,id,ml,true)
	    this listenTo squares(n)
	    squaresPanel.contents.update(n,squares(n))
	  }
	}
	dirt ++= coords
	squaresPanel.revalidate
      }
      squaresPanel.revalidate
    }
    /* fixme: code duplication */
    case BoardEnterSquare(pos, `id`) => {
      if(tilefield.text != "") {
	cleanDirt
	val tiles = (g.language internalise tilefield.text).toList map Tile.fromChar
	val move = SPlay(horizontalOrientation,(pos % g.board.width,pos / g.board.width), tiles)
	val coords = move touchesCoords g.board
	val movelisten = false :: (List.fill(tiles.length - 1)(true))
	(coords zip tiles) zip movelisten map {
	  case (((x,y),t),ml) => {
	    val n = y*g.board.width + x
	    this deafTo squares(n)
	    squares(n) = GameFrame.drawSquare((g.board.square(x,y).get,
					       Some(t))
					      ,g language,n,id,ml,true)
	    this listenTo squares(n)
	    squaresPanel.contents.update(n,squares(n))
	  }
	}
	dirt ++= coords
	squaresPanel.revalidate
      } else if(!dirt.isEmpty) {
	cleanDirt
	squaresPanel.revalidate
      }
    }
    case ValueChanged(`tilefield`) => {
      if((tilefield.text == "") || !dirt.isEmpty) {
	cleanDirt
	squaresPanel.revalidate
      }
    }
  }
}

object GameFrame {
  def squareColor(s:Square) = {
    if(s start)
      new Color(255,192,0)
    else if(s.wordbonus == 2 && s.tilebonus == 1)
      new Color(235,180,200)
    else if(s.wordbonus == 3 && s.tilebonus == 1)
      new Color(235,100,100)
    else if(s.wordbonus == 1 && s.tilebonus == 2)
      new Color(160,160,235)
    else if(s.wordbonus == 1 && s.tilebonus == 3)
      new Color(120,120,255)
    else if(s.wordbonus == 1 && s.tilebonus == 1)
      new Color(60,100,60)
    else
      new Color(100,100,100)
  }

  def colorMix(c1: java.awt.Color, c2: java.awt.Color, w : Double = 0.5) : java.awt.Color = {
    new java.awt.Color(
      ((c1.getRed * (1-w) + c2.getRed * w)/255) toFloat,
      ((c1.getGreen * (1-w) + c2.getGreen * w)/255) toFloat,
      ((c1.getBlue * (1-w) + c2.getBlue * w)/255) toFloat
    )
  }

  def drawDrawTile(t : DrawTile, l: Language) : Component = {
    t match {
      case DrawLetter(c) =>
	GameFrame.charSquare((l externalise c.toString) toUpperCase,
			     l.tilevalue(DrawLetter(c)).toString,
			     java.awt.Color.black)
      case DrawBlank =>
	new BoxPanel(Orientation.Vertical) {
	  preferredSize=new Dimension(40,40)
	  maximumSize=new Dimension(40,40)
	  border=Swing.EtchedBorder(Swing.Lowered)
	  background=java.awt.Color.white
	}
    }
  }

  def drawRack(l : List[DrawTile],lang:Language) : IndexedSeq[Component] =
    (l map {drawDrawTile(_,lang)}).toIndexedSeq

  def drawSquare(c:(Square,Option[Tile]),l:Language, pos:Int, id:Int,
		 movelisten : Boolean = true, dirty : Boolean = false) : Component = {
    val (s,t) = c
    t match {
      case Some(Letter(c)) =>
	GameFrame.charSquare((l externalise c.toString) toUpperCase,
			     l.tilevalue.getOrElse(DrawLetter(c),0).toString,/*todo:snyggare hantering av brickor som inte finns i språket */
			     java.awt.Color.black,Some(pos,id),movelisten,
			     if(dirty)
			       colorMix(java.awt.Color.white, squareColor(s),0.1)
			     else
			       java.awt.Color.white
			   )
      case Some(Blank(c)) =>
	GameFrame.charSquare((l externalise c.toString) toLowerCase,
			     " ",
			     new java.awt.Color(128,128,128),Some(pos,id),movelisten,
			     if(dirty)
			       colorMix(java.awt.Color.white, squareColor(s),0.1)
			     else
			       java.awt.Color.white)
      case None => {
	val ttext = {
	  if(s start)
	    ""
	  else if(s.wordbonus != 1 && s.tilebonus == 1)
	    s.wordbonus + "xO"
	  else if(s.tilebonus != 1 && s.wordbonus == 1)
	    s.tilebonus + "xB"
	  else if(s.wordbonus == 1 && s.tilebonus == 1)
	    ""
	  else
	    "o_O"
	}
	new BoxPanel(Orientation.Vertical){preferredSize=new Dimension(40,40)
					   maximumSize=new Dimension(40,40)
					   background=squareColor(s)
					   border=Swing.EtchedBorder(Swing.Raised)
					   contents += new Label(ttext){font = new Font("Arial",0,14)
									foreground = new Color(0,0,0)
									opaque = false
								      }
					   if(movelisten) {
					     listenTo(mouse.moves)
					   }
					   listenTo(mouse.clicks)
					   reactions += {	
					     case e : MouseClicked => {
					       if(javax.swing.SwingUtilities.isLeftMouseButton(e.peer))
						 publish(BoardClick(pos,id))
					       else if(javax.swing.SwingUtilities.isRightMouseButton(e.peer))
						 publish(ReorientMove(pos,id))
					     }
					     case e : MouseEntered => {
					       publish(BoardEnterSquare(pos,id))
					     }
					   }
					 }
      }
    }
  }

  def charSquare(char:String, score:String, col:java.awt.Color,pub : Option[(Int,Int)] = None,
		 movelisten : Boolean = false,
		 bgcolor : java.awt.Color = java.awt.Color.white) : Component = {
    new BoxPanel(Orientation.Vertical) {
      preferredSize=new Dimension(40,40)
      maximumSize=new Dimension(40,40)
      border=Swing.EtchedBorder(Swing.Lowered)
      background=bgcolor
      contents += Swing.VStrut(5)
      contents += new BorderPanel {
	opaque=false
	add(new Label(char){
	  font=new Font("Arial",
			java.awt.Font.BOLD,
			18)
	  opaque=false
	  foreground=col
	}, BorderPanel.Position.Center)
      }
      contents += new BorderPanel {
	opaque=false
	add(new Label(score){
	  font = new Font("Arial",0,9)
	  opaque = false
	  foreground=java.awt.Color.black
	}, BorderPanel.Position.East)
	add(Swing.VStrut(2),BorderPanel.Position.South)
      }
      pub match {
	case Some((pos,id)) => {
	  if(movelisten) {
	    listenTo(mouse.moves)
	  }
	  listenTo(mouse.clicks)
	  reactions += {
	    case e:MouseClicked => {
	      if(javax.swing.SwingUtilities.isLeftMouseButton(e.peer))
		publish(BoardClick(pos,id))
	      else if(javax.swing.SwingUtilities.isRightMouseButton(e.peer))
		publish(ReorientMove(pos,id))
	    }
	    case e : MouseEntered => {
	      publish(BoardEnterSquare(pos,id))
	    }
	  }
	}
	case _ =>
      }
    }
  }
}

object BoardGUI extends SimpleSwingApplication {
  var ids = 0
  var settings = new Settings()
  var games : IndexedSeq[GameFrame] = Vector()
  var tabbedpane : TabbedPane =
    new TabbedPane {
    }
  var newgamebuttons : IndexedSeq[Button] = Vector()
  def newGame(b:Board,l:String,p:Int = 1,rs:Int = 7) : Unit = {
    val g = new GameFrame(Game.newGame(b,l,1,7),ids)
    ids = ids + 1
    games = this.games :+ g
    tabbedpane.pages += (new TabbedPane.Page("Game ("+g.g.language.name+")",g.contents))
  }
  def closeGame(id:Int) : Unit = {
    val i = games indexWhere{_.id == id}
    if(i != -1) {
      games = games filterNot {_.id == id}
      tabbedpane.pages.remove(i)
    }
  }
  override def main(args: Array[String]) = {
    this.settings = new Settings(args toList)
    LanguageBank.set(settings)
    val b = BoardParser.parse(settings board)
    newGame(b,LanguageBank defaultName)
    super.main(args)
    newgamebuttons = (LanguageBank.languageNames map {s => new Button("New game ("+s+")")}).toIndexedSeq
    newgamebuttons foreach (listenTo(_))
  }

  def top = new swing.MainFrame {
    title = "Nexus"
    contents =
      new BorderPanel{
	add(new BoxPanel(Orientation.Horizontal){
	  contents ++= newgamebuttons
	}, BorderPanel.Position.North)
	add(tabbedpane, BorderPanel.Position.Center)
      }
  }

  reactions += {
    /* TODO: sjukt fult hack för att ta reda på vilken knapp som trycktes */
    case ButtonClicked(b) => {
      val reg = """New game \((.*)\)""".r
      b text match {
	case reg(s) => newGame(BoardParser.parse(settings board),s)
	case _ => ()
      }
    }
  }
}
