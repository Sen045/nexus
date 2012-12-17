import nexus._
import scala.swing._
import scala.swing.event._

class GameFrame(game : Game, i : Int) extends Publisher {
  var id = i
  var g = game
  var squares = 0 to ((g.board.width * g.board.height)-1) map (n => GameFrame.drawSquare((g.board.square(n % g.board.width,n / g.board.width).get,g.board.tile(n % g.board.width, n / g.board.width)), g.language))
  var rack = GameFrame.drawRack(g.racks(0),g language)
  val closegamebutton = new Button("Close game")
  var contents = new BoxPanel(Orientation.Horizontal){
    contents += new BoxPanel(Orientation.Vertical){
      contents += new GridPanel(g.board.height,g.board.width) {background=new java.awt.Color(60,100,60)
							       contents ++= squares
							     }
      contents += new BoxPanel(Orientation.Horizontal){
	contents += new BorderPanel{add(new GridPanel(1,g racksize) {background=new java.awt.Color(60,100,60)
								     contents ++= rack
								   },BorderPanel.Position.South)}
	contents += new TextField()
	contents += closegamebutton
      }
    }
    contents += new BorderPanel{add(new Label("test"),BorderPanel.Position.East)}
  }

  listenTo(closegamebutton)
  reactions += {
    case ButtonClicked(`closegamebutton`) => BoardGUI.closeGame(id)
  }
}

object GameFrame {
  def drawDrawTile(t : DrawTile, l: Language) : Component = {
    t match {
      case DrawLetter(c) =>
	GameFrame.charSquare((l externalise c.toString) toUpperCase,
			     l.tilevalue(DrawLetter(c)).toString,
			     new java.awt.Color(0,0,0))
      case DrawBlank =>
	new BoxPanel(Orientation.Vertical) {
	  preferredSize=new Dimension(40,40)
	  maximumSize=new Dimension(40,40)
	  border=Swing.EtchedBorder(Swing.Lowered)
	  background=new java.awt.Color(255,255,255)
	}
    }
  }

  def drawRack(l : List[DrawTile],lang:Language) : IndexedSeq[Component] =
    (l map {drawDrawTile(_,lang)}).toIndexedSeq

  def drawSquare(c:(Square,Option[Tile]),l:Language) : Component = {
    val (s,t) = c
    t match {
      case Some(Letter(c)) =>
	GameFrame.charSquare((l externalise c.toString) toUpperCase,
			     l.tilevalue(DrawLetter(c)).toString,
			     new java.awt.Color(0,0,0))
      case Some(Blank(c)) =>
	GameFrame.charSquare((l externalise c.toString) toLowerCase,
			     " ",
			     new java.awt.Color(128,128,128))
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
	val tcolor = {
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
	new BoxPanel(Orientation.Vertical){preferredSize=new Dimension(40,40)
					   maximumSize=new Dimension(40,40)
					   background=tcolor
					   border=Swing.EtchedBorder(Swing.Raised)
					   contents += new Label(ttext){font = new Font("Arial",0,14)
									foreground = new Color(0,0,0)
									opaque = false
								      }
					 }
      }
    }
  }

  def charSquare(char:String, score:String, col:java.awt.Color) : Component = {
    new BoxPanel(Orientation.Vertical) {
      preferredSize=new Dimension(40,40)
      maximumSize=new Dimension(40,40)
      border=Swing.EtchedBorder(Swing.Lowered)
      background=new java.awt.Color(255,255,255)
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
	  foreground=new java.awt.Color(0,0,0)
	}, BorderPanel.Position.East)
	add(Swing.VStrut(2),BorderPanel.Position.South)
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
