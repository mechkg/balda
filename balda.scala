import scala.util.Random

class Field (val size: Int) {
  val chars = new Array[Option[Char]] (size * size)

  for (i <- 0 to (size * size - 1)) {
    chars(i) = None
  }
    
  def getCharAt (x: Int, y: Int) : Option[Char] = {
    if (x < 0 || y < 0 || x > (size - 1) || y > (size - 1)) 
      None
    else
      chars(y * size + x)
  }
  def setCharAt (x: Int, y: Int, ch: Char) = chars(y * size + x) = Some(ch)
}

def draw (f: Field) {
  for (i <- 0 to f.size - 1) {
    for (j <- 0 to f.size -1)
      print (
	f.getCharAt(j,i) match {
	  case None => "* "
	  case Some(ch) => ch + " "
	}
      )
    println()
  }
}

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

  def forPrefix (ch: Char) = root.children.get(ch)
}

val words = scala.io.Source.fromFile ("ru_nouns_v1.txt").getLines.filter(_.length >= 3).toIndexedSeq
val dict = words.foldLeft (Trie.empty) { case (t, word) => t + word }

val f = new Field(5)
val rnd = new Random()

val initial = {
  val a = words.filter(_.length == f.size)
  a(rnd.nextInt(a.length))
}

for (i <- (0 to f.size - 1)) {
  f.setCharAt (i, f.size / 2, initial.charAt(i) )
}

draw (f)

def findAll = {
  val Q = scala.collection.mutable.Stack[(List[(Int, Int)], Trie)]()
  val result = scala.collection.mutable.ListBuffer[String]()

  def pathToWord (path : List[(Int, Int)]) = path.foldLeft(""){ case (s, (i, j)) => s + f.getCharAt(i,j).get }

  def neighbours (i: Int, j: Int) =
    List ((i-1, j), (i+1, j), (i, j+1), (i, j-1)).map { 
      case (i, j) => f.getCharAt (i, j).map ( ((i, j), _))
    }.flatten 

  for (i <- (0 to f.size - 1); j <- (0 to f.size -1))
      f.getCharAt(i, j).foreach(ch => dict.forPrefix(ch).foreach ( t => Q.push ( (List((i, j)), t))))

  while (!Q.isEmpty) {
    val (path, prefix) = Q.pop()

    println ("Popped " + pathToWord(path.reverse))

    if (prefix.root.isWord) result += pathToWord(path.reverse)

    val (li, lj) = path.head

    println (path)
    println (neighbours(li, lj))
    
    neighbours (li, lj).filterNot(a => path.contains(a._1)).foreach { case ((i, j), ch) => {
      prefix.forPrefix(ch).foreach (t => { 
	println ("Pushing " + pathToWord ( (i, j) :: path ).reverse)
	Q.push ( (((i, j) :: path), 
		  t)) })
    }}
  }

  result.toList
}

println (findAll)
