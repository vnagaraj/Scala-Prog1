type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

def wordOccurrences(w: Word): Occurrences =  {
  val l =  w.toLowerCase.groupBy((element: Char) => element).toList.sorted
  for {
    (char, string) <- l
  } yield (char, string.length)
}

wordOccurrences("Robert")
sentenceOccurrences(List("Robert","row", "ram"))

def sentenceOccurrences(s: Sentence): Occurrences = {
  val words = (s foldLeft "")(_  + _)
  wordOccurrences(words)
}

val dictionary: List[Word] = List("hello", "scala", "is", "john")
lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
  dictionary.groupBy((element: Word) => wordOccurrences(element))

/** Returns all the anagrams of a given word. */
def wordAnagrams(word: Word): List[Word] = {
  dictionaryByOccurrences(wordOccurrences(word))
}

val k = List(('a', 2), ('b', 2)) map pairToString
val s = (k foldLeft(""))(_ + _)

def pairToString(pair: (Char, Int)):String = {
  def pairToString_helper(occurance: Int, acc: String): String = {
    if (occurance == 0) acc
    else pairToString_helper(occurance-1, acc+ pair._1)
  }
  pairToString_helper(pair._2, "")
}

def subsets(str: String):List[Word] = {
  if (str.isEmpty) List()
  else {
    for {
      split <- 1 to str.length
      word <- str take split
      rest <- subsets(str drop split)
    } yield {
      print (word.toString)
      word.toString
    }
  }.toList
}

subsets("aabb")

