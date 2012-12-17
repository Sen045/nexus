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
