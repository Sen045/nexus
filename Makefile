FILES = src/subdic.scala src/nexus/trie/trie.scala src/nexus/dictionary.scala src/nexus/predicate/predicate.scala src/nexus/predicate/predicateparser.scala src/nexus/regexp.scala src/nexus/regexpparser.scala src/nexus/automaton.scala src/nexus/drawtile.scala src/nexus/multiset.scala src/nexus/language.scala src/nexus/languageparser.scala src/nexus/tile.scala src/nexus/board.scala src/nexus/square.scala src/nexus/move.scala src/nexus/semimove.scala src/boardgui.scala src/nexus/boardparser.scala src/nexus/game.scala src/nexus/languagebank.scala src/nexus/settings.scala src/nexus/package.scala src/nexus/trie/package.scala src/nexus/predicate/package.scala

all:
	scalac -classpath /usr/share/java/scala-swing.jar $(FILES)
	scaladoc -classpath /usr/share/java/scala-swing.jar -d doc/ $(FILES)
nexus:
	scalac -classpath /usr/share/java/scala-swing.jar $(FILES)
documentation:
	scaladoc -classpath /usr/share/java/scala-swing.jar -d doc/ $(FILES)
clean:
	rm -r doc/*
	rm *.class
	rm -r nexus/*