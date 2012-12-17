package nexus

import scala.util.parsing.combinator.RegexParsers

sealed abstract class Post
case class PBlank(score : Int, count : Int) extends Post
case class PTile(tile : Char, score : Int, count : Int, external: String) extends Post
case class PLanguage(name : String, description: String) extends Post
case class PDictionary(enc : String, file: String) extends Post

object Post {
  def externaliser(m:Map[Char,String])(s:String) : String = {
    (s map {c => (m get c) get}).flatten.mkString
  }
  def internaliser(m:Map[String,Char],maxlength : Int)(s:String) : String = {
    def internaliserAux(n:Int)(s:String) : String = {
      if(s isEmpty)
	""
      else if(n==1) {
	m get (s take 1) match {
	  case None => (s take 1) + internaliserAux(maxlength)(s drop n)
	  case Some(c) => c + internaliserAux(maxlength)(s drop n)
	}
      }
      else {
	m get (s take n) match {
	  case None => internaliserAux(n-1)(s:String)
	  case Some(c) => c + internaliserAux(maxlength)(s drop n)
	}
      }
    }
    internaliserAux(maxlength)(s)
  }
/* TODO: massor med kodduplicering här */
  def makeLanguageModuloDictionary(p:List[Post]) : Language = {
    val PBlank(s,c) = (p filter {case PBlank(_,_) => true case _ => false}) head
    val PLanguage(lname,ldesc) = (p filter {case PLanguage(_,_) => true case _ => false}) head
    val dic = Dictionary.empty()
    val tiles = p filter {case PTile(_,_,_,_) => true case _ => false}
    val tilebag = tiles.foldLeft(Multiset.empty[DrawTile] +++ (DrawBlank,c)){case (m,PBlank(_,c)) => m +++ (DrawBlank,c)
										   case (m,PTile(t,_,c,_)) => m +++ (DrawLetter(t),c)
										   case (m,_) => m}
    val scores = (PBlank(s,c)::tiles).foldLeft(Map.empty[DrawTile,Int]){case (map,PBlank(s,_)) => map + (DrawBlank -> s)
										  case (map,PTile(t,s,_,_)) => map + (DrawLetter(t) -> s)
										  case (map,_) => map}
    val (externaliserTable,internaliserTable) = tiles.foldLeft((Map.empty[Char,String],Map.empty[String,Char])){case ((map1,map2),PTile(c,_,_,s)) => (map1 + (c -> s),map2 + (s -> c)) case (t,_) => t}

    if(externaliserTable forall {case (c,s) => (c toString) == s})
      Language.make(dic,tilebag,scores,lname,ldesc)
    else {
      Language.make(dic,tilebag,scores,lname,ldesc,internaliser(internaliserTable,internaliserTable.keys.foldLeft(1){case (n,s) => n max (s length)}),externaliser(externaliserTable))
    }
  }

  def makeLanguage(p:List[Post]) : Language = {
    val PBlank(s,c) = (p filter {case PBlank(_,_) => true case _ => false}) head
    val PLanguage(lname,ldesc) = (p filter {case PLanguage(_,_) => true case _ => false}) head
    val PDictionary(denc,dfile) = (p filter {case PDictionary(_,_) => true case _ => false}) head
    val tiles = p filter {case PTile(_,_,_,_) => true case _ => false}
    var dics = io.Source.fromFile(dfile,denc)
    val dic = Dictionary.empty() +++ dics.getLines
    dics.close()
    val tilebag = tiles.foldLeft(Multiset.empty[DrawTile] +++ (DrawBlank,c)){case (m,PBlank(_,c)) => m +++ (DrawBlank,c)
										   case (m,PTile(t,_,c,_)) => m +++ (DrawLetter(t),c)
										   case (m,_) => m}
    val scores = (PBlank(s,c)::tiles).foldLeft(Map.empty[DrawTile,Int]){case (map,PBlank(s,_)) => map + (DrawBlank -> s)
										  case (map,PTile(t,s,_,_)) => map + (DrawLetter(t) -> s)
										  case (map,_) => map}
    val (externaliserTable,internaliserTable) = tiles.foldLeft((Map.empty[Char,String],Map.empty[String,Char])){case ((map1,map2),PTile(c,_,_,s)) => (map1 + (c -> s),map2 + (s -> c)) case (t,_) => t}

    if(externaliserTable forall {case (c,s) => (c toString) == s})
      Language.make(dic,tilebag,scores,lname,ldesc)
    else {
      Language.make(dic,tilebag,scores,lname,ldesc,internaliser(internaliserTable,internaliserTable.keys.foldLeft(1){case (n,s) => n max (s length)}),externaliser(externaliserTable))
    }
  }
}

class LanguageParser extends RegexParsers {
  val stringQParser = """\'([^\']*)\'""".r
  val stringDQParser = """\"([^\"]*)\"""".r
  val stringParser = """([^={}\s]+)""".r
  val intParser  = """(-?0|-?[1-9]\d*)""".r

  def entryParser : Parser[(Map[String,String],Map[String,Int])] =
    (stringParser ~ "=" ~ (stringQParser|stringDQParser|intParser)) ^^
    { case stringParser(tag) ~ _ ~ stringQParser(s) => (Map.empty + (tag -> s),Map.empty)
       case stringParser(tag) ~ _ ~ stringDQParser(s) => (Map.empty+(tag -> s),Map.empty)
       case stringParser(tag) ~ _ ~ intParser(s) => (Map.empty,Map.empty+(tag -> (s toInt)))
    }
  def entriesParser: Parser[(Map[String,String],Map[String,Int])] =
    (entryParser ~ ("," ~> entriesParser)) ^^
     { case e1 ~ e2 => ((e1 _1) ++ (e2 _1), (e1 _2) ++ (e2 _2))
    } | (entryParser <~ "}")
  def postParser : Parser[Post] =
    ("language" ~> ("{" ~> entriesParser) ^^
     {case (s,i) => PLanguage(s getOrElse("name",""),s getOrElse("description",""))
    }) |
    ("blank" ~> ("{" ~> entriesParser) ^^
     {case (s,i) => PBlank(i getOrElse("score",0),i getOrElse("count",0))
    }) |
    ("tile" ~> ("{" ~> entriesParser) ^^
     {case (s,i) =>
       PTile(((s get "char") get) head,i getOrElse("score",0),i getOrElse("count",0), s getOrElse("external",((s get "char") get)))
    }) |
    ("dictionary" ~> ("{" ~> entriesParser) ^^
     {case (s,i) =>
       PDictionary(s getOrElse("enc","ISO-8859-1"),s getOrElse("file",""))
    })

  def postsParser : Parser[List[Post]] =
    ((postParser ~ postsParser) ^^ {case a~b => a::b}) | ("" ^^ {x => List()})  
}

object LanguageParser {
  /* TODO: kodduplicering, kanske decoupla språkparsing från ordlisteinläsning? */
  def parse(path : String) : Language = {
    val Lang = new LanguageParser()
    val y = io.Source.fromFile(path) mkString;
    Lang.parseAll(Lang.postsParser,y) match {
      case Lang.Success(p,_) => Post.makeLanguage(p)
    }
  }

  def parseModuloDictionary(path : String) : Language = {
    val Lang = new LanguageParser()
    val y = io.Source.fromFile(path) mkString;
    Lang.parseAll(Lang.postsParser,y) match {
      case Lang.Success(p,_) => Post.makeLanguageModuloDictionary(p)
    }
  }
}
