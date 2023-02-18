import scala.annotation.tailrec
//import Lexer._

case class Lexer (spec: String) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */

  def init(): (Dfa[Int], Map[Int, String]) ={
    val lines = spec.split("\n").toList

    def parseSpec(lines:List[String]): List[(Nfa[Int], String)] = {
      lines match {
        case ::(head, next) =>
          val words = head.split(": ")
          val lexeme = words.head
          val regex = words.tail.head.dropRight(1)
          val nfa = Nfa.fromPrenex(Regex.toPrenex(regex))
          (nfa, lexeme) :: parseSpec(next)
        case Nil => Nil
      }
    }

    val pairList:List[(Nfa[Int], String)] = parseSpec(lines)
    val nfaList:List[Nfa[Int]] = pairList.map(_._1)
    val initialState = Nfa.getNr
    val finalNfaStateToLexeme:List[(Int, String)] = pairList.map((p: (Nfa[Int], String))=> (p._1.finalStates.head, p._2))

    val mainNfa:Nfa[Int] = nfaList.map(_.initialState).foldLeft(Nfa(initialState,
                                                                    nfaList.map(_.finalStates).reduce(_ ++ _),
                                                                    nfaList.map(_.transitions).reduce(_ ++ _)
                                                                    )
                                                                )(_.addTransition(initialState, Nfa.eps, _))

    val partialDfa = Dfa.fromNfa(mainNfa)

    val mainDfa = partialDfa.map(Dfa.myMap)

    def findFirst(set: Set[Int]): (Int, String) = {
      for (p <- finalNfaStateToLexeme){
        if(set.contains(p._1))
          return (Dfa.myMap(set), p._2)
      }
      (-1, "")
    }
    val lexemeFinalDFAState: Map[Int, String] = partialDfa.finalStates.map(findFirst).toMap

    (mainDfa, lexemeFinalDFAState)
  }

  private val p: (Dfa[Int], Map[Int, String]) = init()
  val dfa: Dfa[Int] = p._1
  val finalStateToLexeme: Map[Int, String] = p._2
  def lex(word: String): Either[String,List[(String,String)]] = {
    val wordLen = word.length
    val endlCount = word.count(_ == '\n')
    @tailrec
    def aux(word: String, list: List[(String, String)]): Either[String,List[(String,String)]] = {

      findLexeme(word) match {
        case Left(value) => Left(value)
        case Right(value) => if(value._3 != "")aux(value._3, (value._2, value._1) :: list) else Right((value._2, value._1) :: list)
      }

    }
    def findLexeme(word: String): Either[String, (String, String, String)] = {
      @tailrec
      def aux(word: String, currentState: Int, currentToken: String, previousMatchLexeme: String, previousMatchToken: String, previousMatchWord : String): Either[String, (String,String,String)] = {


        val foundToken = dfa.isFinal(currentState)
        val matchLexeme:String = if(foundToken) finalStateToLexeme.getOrElse(currentState, "")
        else previousMatchLexeme
        val matchToken:String = if(foundToken) currentToken else previousMatchToken
        val matchWord:String = if(foundToken) word else previousMatchWord
        //println("W: \"" + word+ "\"")
        //println("S: " + currentState + " \"" + currentToken + "\"")
        //println("L: \"" + matchLexeme + "\" \"" + matchToken + "\" \"" + matchWord + "\"")
        if(dfa.sinkState == currentState) {
          //println("endSink")
          if(matchLexeme == "") {
            return Left("No viable alternative at character " + (wordLen - word.length - 1) + ", line " + (endlCount - word.count(_ == '\n')))
          } else return Right((matchLexeme, matchToken, matchWord))
        }
        if(word == ""){
          //println("endWord")
          if(previousMatchLexeme == "" && !foundToken) return Left("No viable alternative at character EOF, line " + endlCount)
          else return Right((matchLexeme, matchToken, matchWord))
        }





        val c = word.head
        val nextState = dfa.next(currentState, c)
        aux(word.tail, nextState, currentToken.appended(c), matchLexeme, matchToken, matchWord)
      }
      aux(word, dfa.initialState, "", "", "", word)
    }

    aux(word, Nil) match {
      case Left(value) => Left(value)
      case Right(value) => Right(value.reverse)
    }


  }


}

