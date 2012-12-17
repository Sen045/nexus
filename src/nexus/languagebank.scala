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
package nexus
/** The language bank acts as a cache for languages, indexed by name.
 *
 *  To each language name we associate a tuple (f,s,l), where f is the
 *  path to the file containing the language, s is a description of the language and l
 *  is either the cached language or NONE. If l is NONE, this means that the dictionary
 *  associated with the language has not yet been constructed. This construction will be
 *  triggered when the dictionary is requested by a client, and will then be cached to speed
 *  up future requests.
 *
 *  The language bank also has a default dictionary.
 *
 *  While this approach sacrifices both immutability and some locality by storing languages
 *  in a globally accessible structure, the intention is to save memory by avoiding multiple
 *  copies of the same dictionary in memory, while also making sure that tries for each
 *  dictionary need not be constructed more than once.
 */
object LanguageBank {
  private val languages : collection.mutable.Map[String,(java.io.File,String,Option[Language])] =
    collection.mutable.Map()
  private var default = ""
  
  /** Initialise the language bank according to settings. The default language is cached.
   *  Other languages from the language directory are indexed but not cached.
   *  @param s settings to initialise by.
   *  @return ()
   */
  def set(s:Settings) : Unit = {
    val primlangfile = new java.io.File(s.language)
    val primlang = LanguageParser.parse(primlangfile.getPath)
    val langfiles = new java.io.File(s.langdir).listFiles.filter(_.getName.endsWith(".lang")).filterNot(_ equals primlang)
    languages+=(primlang.name -> (primlangfile,primlang.description,Some(primlang)))
    langfiles foreach {x => val l = LanguageParser.parseModuloDictionary(x.getPath)
		       languages+=(l.name -> (x,l.description,None))
		     }
    default = primlang.name
  }

  /** Get a language from the bank. If it has not previously been cached, it will be
   *  constructed - hence the run time will be significantly larger the first time a
   *  (non-default) dictionary is requested.
   *  @param s name of the language. Must correspond to a language in the bank.
   *  @return the language associated with s
   */
  def get(s:String) : Language = {
    languages get s match {
      case None => throw new IllegalArgumentException("No such language in the language bank: \"" + s + "\"");
      case Some((f,s2,Some(l))) => l
      case Some((f,s2,None)) => {
	val l = LanguageParser.parse(f.getPath)
	languages += (s -> (f,s2,Some(l)))
	l
      }
    }
  }

  /** Get the default language.
   *  @return the default language.
   */
  def get : Language =
    get(default)

  /** Get the names of the languages.
   *  @return the names of the languages.
   */
  def languageNames : Iterable[String] =
    languages keys

  /** Get the name of the default language.
   *  @return name of the default language
   */
  def defaultName : String =
    default
}
