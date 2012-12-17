package nexus

class Language private (d:Dictionary, t:Multiset[DrawTile], v:Map[DrawTile,Int],c:Option[Multiset[Char]], i : String => String, x : String => String,n:String,de:String) {
  val dic = d
  val tileset = t
  val tilevalue = v
  val lettercount = c
  val name = n
  val description = de
  def internalise(s:String) = i(s)
  def externalise(s:String) = x(s)
}

object Language {
  def make(d:Dictionary, t:Multiset[DrawTile], v:Map[DrawTile, Int], n:String, de:String, i: String => String = identity, x : String => String = identity) = {
    new Language(d,t,v,None,i,x,n,de)
  }
  def trivial =
    new Language(Dictionary.empty,Multiset.empty,Map.empty,None,identity,identity,"","")
}
