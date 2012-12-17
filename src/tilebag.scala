/* I väntan på att scalas standardbibliotek får multisets...
 * överflödig, använd Multiset[Drawtile] istället!
 */
package nexus

case class TileBag private (b : Map[DrawTile,Int]) {
  def +(t:DrawTile) = {
    this.b get t match {
      case None => TileBag(this.b + (t -> 1))
      case Some(n) => TileBag(this.b + (t -> (n+1)))
    }
  }

  def ++(t:Traversable[DrawTile]) = {
    t.foldLeft(this)(_ + _)
  }

  def -(t:DrawTile) : Option[TileBag] = {
    this.b get t match {
      case None => None
      case Some(0) => None
      case Some(1) => Some(TileBag(this.b - t))
      case Some(n) => Some(TileBag(this.b + (t -> (n-1))))
    }
  }

  def --(t:Traversable[DrawTile]) = {
    t.foldLeft(Some(this):Option[TileBag]){case (b, t) => b flatMap (_ - t)}
  }

  def get(t:DrawTile) = {
    this.b get t
  }

  def tiles : Int =
    this.b.foldRight(0)(_._2 + _)
}

object TileBag {
  val empty = TileBag(Map.empty)
  def fromTiles(t:Traversable[DrawTile]) = {
    empty ++ t
  }

  def fromChars(t:Traversable[Char]) = {
    empty ++ (t map (DrawTile.fromChar _))
  }
}
