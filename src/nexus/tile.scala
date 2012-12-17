package nexus

sealed abstract class Tile {
  def letter : Char

  def mkString : Char
}

case class Letter(c:Char) extends Tile {
  override def letter = this.c

  override def mkString = this.c.toUpper
}
case class Blank(c:Char) extends Tile {
  override def letter = this.c

  override def mkString = this.c.toLower
}
