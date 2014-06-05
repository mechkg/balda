import scala.util.Random
import scala.io.Codec
import java.io._

case class Node (isWord: Boolean, children: Map[Char, Trie])
  
object Trie {
  def empty = Trie (Node (false, Map[Char, Trie]()))
}

case class Trie (root: Node) {
  private def add (k: List[Char]): Trie = k match {
    case Nil => Trie (Node(true, root.children))
    case x :: xs => root.children.get(x) match {
      case Some(t) => Trie (Node (root.isWord, root.children + (x -> root.children(x).add(xs))))
      case None => Trie (Node (root.isWord, root.children + (x -> Trie.empty.add(xs))))
    }
  }

  private def cont (k: List[Char]): Boolean = k match {
    case Nil => root.isWord
    case x :: xs => root.children.get(x) match {
      case Some(t) => t.cont(xs)
      case None => false
    }
  }

  def + (s: String) = add(s.toList)

  def contains(s: String) = cont(s.toList)

  def continuations (ch: Char) = root.children.get(ch)
}


class Field (val size: Int) {
  val chars = new Array[Option[Char]] (size * size)
  
  for (i <- 0 to (size * size - 1)) {
    chars(i) = None
  }

  val indices = for ( i <- 0 to (size - 1); j <- 0 to (size - 1)) yield (i, j)

  def rangeCheck (col: Int, row: Int) = (col >= 0 && row >= 0 && col < size && row < size)
  def getCharAt (col: Int, row: Int) : Option[Char] = chars(row * size + col)
  def setCharAt (col: Int, row: Int, ch: Char) = chars(row * size + col) = Some(ch)

  def neighbours (i: Int, j: Int) =
    List ((i-1, j), (i+1, j), (i, j+1), (i, j-1)).flatMap {
      case (i, j) => if (rangeCheck(i,j))
			 Some (((i, j), getCharAt(i,j)))
		     else None
    }

  def usableBlank (i: Int, j: Int) = neighbours (i, j).exists(!_._2.isEmpty)

  def full = indices.forall { case (col, row) => !getCharAt(col, row).isEmpty }
}

case class Game (size: Int, dict: Trie) {
  val field = new Field (size)
  val rnd = new Random()
  val usedWords = scala.collection.mutable.Set[String]()

  case class Path (blank: Option[((Int, Int), Char)], path: List[(Int, Int)], prefix: Trie) {
    val word = path.foldLeft ("") { case (s, (i, j)) => {
      field.getCharAt (i,j) match {
	case Some (ch) => s + ch
	case None => s + blank.get._2
      }
    }}.reverse
  }

  def init (word: String) = {
    for (i <- (0 to field.size - 1)) 
      field.setCharAt (i, field.size / 2, word.charAt(i))
    usedWords += word
  }

  def findWords() = {
    val Q = scala.collection.mutable.Stack[Path]()
    val result = scala.collection.mutable.ListBuffer[(String, ((Int, Int), Char))]()

    val init = field.indices.flatMap { case (i, j) => field.getCharAt(i, j) match {
      // there is some character at (i, j)
      case Some (ch) => dict.continuations(ch) match { 
	// there are words starting with that character
	case Some(cont) => List(Path (None, List((i,j)), cont))
	// no words starting with that character
	case None => Nil
      }

      // the square at (i, j) is empty
      case None => 
	// empty square is next to a character i.e. not isolated and therefore useless
	if (field.usableBlank(i, j))
	  dict.root.children.keySet.toList.map(ch => 
	    Path(Some ((i,j), ch), List((i,j)), dict.continuations(ch).get))
					    
	// ignore this square
	else Nil
    }}

    init.foreach (Q.push(_))
    
    while (!Q.isEmpty) {
      val path = Q.pop()

      //  ps.println ("Popped " + path.word)

      // accept valid words that use a blank square and have not been used yet
      if (path.prefix.root.isWord && !path.blank.isEmpty && !usedWords.contains(path.word))
	result += ((path.word, path.blank.get))
      
      val (li, lj) = path.path.head

      //    println (path)
      //    println (neighbours(li, lj))

      val extensions = field.neighbours(li, lj).filterNot(a => path.path.contains(a._1)).flatMap {
	case ((i, j), sq) => sq match {
	  // square has a character
	  case Some (ch) => path.prefix.continuations(ch).map(t => Path (path.blank, (i, j) :: path.path, t))

	  // square is blank
	  case None => path.blank match {
	    case None if (field.usableBlank(i,j)) =>
	      path.prefix.root.children.keySet.map ( ch => 
		Path (Some ((i,j), ch), (i, j) :: path.path, path.prefix.continuations(ch).get))
	    case _ => List()
	  }
	}
      }

      extensions.foreach (Q.push(_))
    }
        
    result.toList
  }

  def choose (words: List[(String, ((Int, Int), Char))]) = {
    val byLen = words.groupBy(_._1.length)    
    val maxLen = byLen.keySet.max
    val index = rnd.nextInt (byLen(maxLen).length)
    
    byLen(maxLen)(index)
  }

  def apply ( w: (String, ((Int, Int), Char))) = w match { case (word, ((col, row), ch)) => {
    field.setCharAt (col, row, ch) 
    usedWords += word
  }}
}

class GameServer(wordList: File) {
  val words = scala.io.Source.fromFile(wordList)(Codec("utf-8")).getLines.filter(_.length >= 3).toIndexedSeq
  val dictionary = words.foldLeft (Trie.empty) { case (t, word) => t + word }
  val rnd = new Random()

  def createGame (size: Int) = {
    val initial = {
      val a = words.filter(_.length == size)
      a(rnd.nextInt(a.length))
    }

    val game = Game(size, dictionary)
    game.init (initial)

    game
  }
}

val ps = new PrintStream (System.out, true, "utf-8")

def draw (f: Field) {
  for (i <- 0 to f.size - 1) {
    for (j <- 0 to f.size - 1)
      ps.print (
	f.getCharAt(j,i) match {
	  case None => "* "
	  case Some(ch) => ch + " "
	}
      )
    ps.println()
  }
}

val srv = new GameServer (new File ("en_nouns.txt"))
val game = srv.createGame(5)

draw (game.field)

var quit = false

while (!quit) {
  ps.println ()

  val words = game.findWords()
 //  ps.println (words)
  
  if (words.isEmpty) {
    ps.println ("No more!")
    quit = true
  } else {

    val choice = game.choose (words)
    ps.println (choice)
    game.apply (choice)
    draw (game.field)
  }

  if (game.field.full) quit = true
}

ps.close()
