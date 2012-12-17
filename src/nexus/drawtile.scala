package nexus

sealed abstract class DrawTile {
  def >(d:DrawTile) : Boolean
  def <(d:DrawTile) : Boolean
  def <=(d:DrawTile) : Boolean
  def >=(d:DrawTile) : Boolean
}

case class DrawLetter(c:Char) extends DrawTile {
  override def > (d:DrawTile) = {
    d match {
      case DrawBlank => true
      case DrawLetter(c2) => this.c > c2
    }
  }

  override def < (d:DrawTile) = {
    d match {
      case DrawBlank => false
      case DrawLetter(c2) => this.c < c2
    }
  }

  override def >= (d:DrawTile) = {
    d match {
      case DrawBlank => true
      case DrawLetter(c2) => this.c >= c2
    }
  }

  override def <= (d:DrawTile) = {
    d match {
      case DrawBlank => false
      case DrawLetter(c2) => this.c <= c2
    }
  }
}
case object DrawBlank extends DrawTile {
  override def > (d:DrawTile) = {
    false
  }

  override def < (d:DrawTile) = {
    d match {
      case DrawBlank => false
      case DrawLetter(_) => true
    }
  }

  override def >= (d:DrawTile) = {
    d match {
      case DrawBlank => true
      case DrawLetter(_) => false
    }
  }

  override def <= (d:DrawTile) = {
    true
  }
}

object DrawTile {
  def fromChar(c:Char) =
    if(c=='?')
      DrawBlank
    else
      DrawLetter(c)
}
