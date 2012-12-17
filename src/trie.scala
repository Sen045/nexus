/*
 * mer joxxiga, mindre minnesätande tries
 * genom att försöka minimera antalet objekt i trädet
 *
 * en datastrukturinvariant är att all ord i en trie har samma längd
 */

package nexus
package trie

sealed abstract class Trie {
  def prefix : Option[Char]
  def child : Option[Trie]
  def sibling : Option[Trie]
  def isRoot : Boolean

  def + (s:Seq[Char]) : Trie =
    if(s.nonEmpty) {
      if(prefix isEmpty) {
	val t = TrieTop + s.tail
	TrieNodeTail(s.head,t)
      }
      else {
	if(s.head < this.prefix.get) {
	  val t = TrieTop + s.tail
	  TrieNode(s.head,t,this)
	}
	else if(s.head == this.prefix.get) {
	  this.sibling match {
	    case None =>
	      TrieNodeTail(this.prefix get, this.child.get + s.tail)
	    case Some(t) =>
	      TrieNode(this.prefix get, this.child.get + s.tail,t)
	  }
	  
	}
	else {
	  val t = this.sibling.getOrElse(TrieTop) + s
	  TrieNode(this.prefix get, this.child get,t)
	}
      }
    } else TrieTop

  /*
   * Tar bort alla ord med prefix s
   */
  def -(s:Seq[Char]) : Trie =
    if(s.nonEmpty) {
      this match {
	case TrieTop => TrieBottom /* raise exception? */
	case TrieBottom => TrieBottom /* raise exception? */
	case TrieNode(c,child,sibling) =>
	  if(s.head == c) {
	    child - s.tail match {
	      case TrieBottom => sibling
	      case t => TrieNode(c,t,sibling)
	    }
	  } else if(s.head < c) {
	    this
	  } else {
	    TrieNode(c,child,sibling - s)
	  }
	case TrieNodeTail(c,child) =>
	  if(s.head == c) {
	    child - s.tail match {
	      case TrieBottom => TrieBottom
	      case t => TrieNodeTail(c,t)
	    }
	  }
	  else
	    this
      }
    } else {
      TrieBottom
    }

  def toCharListList : List[List[Char]] =
    this match {
      case TrieTop => List(Nil)
      case TrieBottom => Nil
      case TrieNode(c,child,sibling) =>
	child.toCharListList.map(c :: _) ++ sibling.toCharListList
      case TrieNodeTail(c,child) =>
	child.toCharListList.map(c :: _)
    }

  def toStringList : List[String] =
    this match {
      case TrieTop => List("")
      case TrieBottom => Nil
      case TrieNode(c,child,sibling) =>
	child.toStringList.map(c + _) ++ sibling.toStringList
      case TrieNodeTail(c,child) =>
	child.toStringList.map(c + _)
    }

  def query(s:Seq[Char]) : Boolean = {
    if(s.nonEmpty) {
      this match {
	case TrieNode(c,child,sibling) =>
	  if(s.head == c) {
	    child query s.tail
	  } else if(s.head < c) {
	    false
	  } else {
	    sibling query s
	  }
	case TrieNodeTail(c,child) =>
	  if(s.head == c) {
	    child query s.tail
	  } else false
	case _ => false
      }
    } else {
      this match {
	case TrieTop => true
	case _ => false
      }
    }
  }

  def union(t : Trie) : Trie = {
    this match {
      case TrieBottom => t
      case TrieTop => TrieTop
      case TrieNode(c,child,sibling) =>
	t match {
	  case TrieBottom => this
	  case TrieTop => TrieBottom /* raise exception */
	  case TrieNode(c2,child2,sibling2) =>
	    if(c == c2) {
	      TrieNode(c,child union child2,sibling union sibling2)
	    } else if(c < c2) {
	      TrieNode(c,child,sibling union t)
	    } else {
	      TrieNode(c2,child2,this union sibling2)
	    }
	  case TrieNodeTail(c2,child2) =>
	    if(c == c2) {
	      TrieNode(c,child union child2,sibling)
	    } else if(c < c2) {
	      TrieNode(c,child,sibling union t)
	    } else {
	      TrieNode(c2,child2,this)
	    }
	}
      case TrieNodeTail(c,child) =>
	t match {
	  case TrieBottom => this
	  case TrieTop => TrieBottom /* raise exception */
	  case TrieNode(c2,child2,sibling2) =>
	    if(c == c2) {
	      TrieNode(c,child union child2,sibling2)
	    } else if (c < c2) {
	      TrieNode(c,child,t)
	    } else {
	      TrieNode(c2,child2,this union sibling2)
	    }
	  case TrieNodeTail(c2,child2) =>
	    if(c == c2) {
	      TrieNodeTail(c,child union child2)
	    } else if (c < c2) {
	      TrieNode(c,child,TrieNodeTail(c2,child2))
	    } else {
	      TrieNode(c2,child2,TrieNodeTail(c,child))
	    }
	}
    }
  }

  def intersection(t : Trie) : Trie = {
    this match {
      case TrieBottom => TrieBottom
      case TrieTop => t
      case TrieNode(c,child,sibling) =>
	t match {
	  case TrieBottom => TrieBottom
	  case TrieTop => TrieBottom /* raise exception */
	  case TrieNode(c2,child2,sibling2) =>
	    if(c == c2) {
	      Trie.make(c,child intersection child2, sibling intersection sibling2)
	    } else if(c < c2) {
	      sibling intersection t
	    } else {
	      this intersection sibling2
	    }
	  case TrieNodeTail(c2,child2) =>
	    if(c == c2) {
	      Trie.makeTail(c,child intersection child2)
	    } else if(c < c2) {
	      sibling intersection t
	    } else {
	      TrieBottom
	    }
	}
      case TrieNodeTail(c,child) =>
	t match {
	  case TrieBottom => this
	  case TrieTop => TrieBottom /* raise exception */
	  case TrieNode(c2,child2,sibling2) =>
	    if(c == c2) {
	      Trie.makeTail(c,child intersection child2)
	    } else if (c < c2) {
	      TrieBottom
	    } else {
	      this intersection sibling2
	    }
	  case TrieNodeTail(c2,child2) =>
	    if(c == c2) {
	      Trie.makeTail(c,child intersection child2)
	    } else {
	      TrieBottom
	    }
	}
    }
  }

  def complement(t : Trie) : Trie = {
    this match {
      case TrieBottom => TrieBottom
      case TrieTop =>
	t match {
	  case TrieBottom => TrieTop
	  case _ => TrieBottom
	}
      case TrieNode(c,child,sibling) =>
	t match {
	  case TrieBottom => this
	  case TrieTop => TrieBottom /* raise exception */
	  case TrieNode(c2,child2,sibling2) =>
	    if(c == c2) {
	      Trie.make(c,child complement child2, sibling complement sibling2)
	    } else if(c < c2) {
	      Trie.make(c,child,sibling complement t)
	    } else {
	      this complement sibling2
	    }
	  case TrieNodeTail(c2,child2) =>
	    if(c == c2) {
	      Trie.makeTail(c,child complement child2)
	    } else if(c < c2) {
	      Trie.make(c,child,sibling complement t)
	    } else {
	      sibling
	    }
	}
      case TrieNodeTail(c,child) =>
	t match {
	  case TrieBottom => this
	  case TrieTop => TrieBottom /* raise exception */
	  case TrieNode(c2,child2,sibling2) =>
	    if(c == c2) {
	      Trie.makeTail(c,child complement child2)
	    } else if (c < c2) {
	      this
	    } else {
	      this complement sibling2
	    }
	  case TrieNodeTail(c2,child2) =>
	    if(c == c2) {
	      Trie.makeTail(c,child complement child2)
	    } else {
	      this
	    }
	}
    }
  }

}

case class TrieNode private[trie] (p : Char, c : Trie, s : Trie) extends Trie {
  override def prefix = Some(p)
  override def child = Some(c)
  override def sibling = Some(s)
  override def isRoot = false
}

case class TrieNodeTail private[trie] (p : Char, c : Trie) extends Trie {
  override def prefix = Some(p)
  override def child = Some(c)
  override def sibling = None
  override def isRoot = false
}

case object TrieTop extends Trie {
  override def prefix = None
  override def child = None
  override def sibling = None
  override def isRoot = true
}

case object TrieBottom extends Trie {
  override def prefix = None
  override def child = None
  override def sibling = None
  override def isRoot = false
}


object Trie {
  /*
   * Returnerar det minsta trie som är ekvivalent med TrieNodeTail(c,child),
   * under antagandet att child är minimal.
   */
  private def makeTail(c:Char,child:Trie) =
    child match {
      case TrieBottom => TrieBottom
      case t => TrieNodeTail(c,t)
    }

  /*
   * Returnerar det minsta trie som är ekvivalent med TrieNode(c,child,sibling),
   * under antagandet att child och sibling är minimala.
   */
  private def make(c:Char,child:Trie,sibling:Trie) =
    child match {
      case TrieBottom => sibling
      case t =>
	sibling match {
	  case TrieBottom => TrieNodeTail(c,t)
	  case t2 => TrieNode(c,t,t2)
	}
    }

  def prefix(s:Seq[Char])(t:Trie) : Trie = {
    if(s.isEmpty) {
      t
    }
    else t match {
      case TrieNode(c,child,sib) =>
	if(c == s.head)
	  Trie.makeTail(c,prefix(s.tail)(child))
	else if(s.head < c)
	  TrieBottom
	else
	  prefix(s)(sib)
      case TrieNodeTail(c,child) =>
	if(c == s.head)
	  Trie.makeTail(c,prefix(s.tail)(child))
	else
	  TrieBottom
      case _ => TrieBottom /* should raise exception here */
    }
  }

  def equals(s:Seq[Char])(t:Trie) : Trie = {
    if(s.isEmpty) {
      t match {
	case TrieTop => TrieTop
	case _ => TrieBottom
      }
    }
    else t match {
      case TrieNode(c,child,sib) =>
	if(c == s.head)
	  Trie.makeTail(c,equals(s.tail)(child))
	else if(s.head < c)
	  TrieBottom
	else
	  equals(s)(sib)
      case TrieNodeTail(c,child) =>
	if(c == s.head)
	  Trie.makeTail(c,equals(s.tail)(child))
	else
	  TrieBottom
      case _ => TrieBottom /* should raise exception here */
    }
  }

  /* PRE: |s| <= depth of t */    
  def suffix(s:Seq[Char])(t:Trie)(n:Int) : Trie = {
    if(n==0) {
      equals(s)(t)
    }
    else t match {
      case TrieNode(c,child,sib) =>
	Trie.make(c,suffix(s)(child)(n-1),suffix(s)(sib)(n))
      case TrieNodeTail(c,child) =>
	Trie.makeTail(c,suffix(s)(child)(n-1))
      case _ => TrieBottom /* should raise exception here */
    }
  }

  /* PRE: |s| <= depth of t */    
  def has(s:Seq[Char])(t:Trie)(n:Int) : Trie = {
    if(n==0) {
      Trie.equals(s)(t)
    }
    else t match {
      case TrieNode(c,child,sib) =>
	Trie.prefix(s)(TrieNodeTail(c,child)) match {
	  case TrieBottom =>
	    Trie.make(c,has(s)(child)(n-1),has(s)(sib)(n))
	  case TrieNodeTail(c,t) =>
	    Trie.make(c,t,has(s)(sib)(n))
	  case _ => TrieBottom /* raise exception */
	}
      case TrieNodeTail(c,child) =>
	Trie.prefix(s)(t) match {
	  case TrieBottom =>
	    Trie.makeTail(c,has(s)(child)(n-1))
	  case t => t
	}
      case _ => TrieBottom /* should raise exception here */
    }
  }

  def matches(a:Automaton)(t:Trie)(n:Int) : Trie = {
    t match {
      case TrieBottom => TrieBottom
      case TrieTop => if(a accepting) TrieTop else TrieBottom
      case TrieNodeTail(c,child) =>
	{
	  val a2 = a consume c
	  a2 distance match {
	    case None => TrieBottom
	    case Some(m) =>
	      if(m >= n)
		TrieBottom
	      else
		Trie.makeTail(c,matches(a2)(child)(n-1))
	  }
	}
      case TrieNode(c,child,sibling) =>
	{
	  val a2 = a consume c
	  a2 distance match {
	    case None => matches(a)(sibling)(n)
	    case Some(m) =>
	      if(m >= n)
		matches(a)(sibling)(n)
	      else
		Trie.make(c,matches(a2)(child)(n-1),matches(a)(sibling)(n))
	  }
	}
    }
  }

  def drawFrom(l:Multiset[DrawTile])(t:Trie) : Trie = {
    t match {
      case TrieBottom => TrieBottom
      case TrieTop => TrieTop
      case TrieNodeTail(c,child) =>
	l - DrawLetter(c) match {
	  case None =>
	    l - DrawBlank match {
	      case None => TrieBottom
	      case Some(l2) =>
		Trie.makeTail(c,drawFrom(l2)(child))
	    }
	  case Some(l2) =>
	    Trie.makeTail(c,drawFrom(l2)(child))
	}
      case TrieNode(c,child,sibling) =>
	l - DrawLetter(c) match {
	  case None =>
	    l - DrawBlank match {
	      case None => drawFrom(l)(sibling)
	      case Some(l2) =>
		Trie.make(c,drawFrom(l2)(child),drawFrom(l)(sibling))
	    }
	  case Some(l2) =>
	    Trie.make(c,drawFrom(l2)(child),drawFrom(l)(sibling))
	}
    }
  }
}
