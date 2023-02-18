

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */



  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    s match {
      case '['::st::'-'::dr::']'::next =>
        Right('(')::st.until(dr).toList
          .foldRight(List(Left(dr)): List[Either[Char, Char]])(
            (c: Char, l: List[Either[Char, Char]]) => Left(c) :: Right('|') :: l):::(Right(')')::preprocess(next))
      case '\\'::'w'::next => Right('(')::Left(' ')::Right('|')::Left('\n')::Right('|')::Left('\t')::Right(')')::Right('*')::preprocess(next) // whitespaces
      case '\\'::'n'::next => Left('\n')::preprocess(next)
      case '\''::'\\'::'n'::'\''::next => Left('\n')::preprocess(next)
      case '\''::c::'\''::next => Left(c)::preprocess(next)
      case 'e'::'p'::'s'::next => Left(0.toChar)::preprocess(next)
      case c::next => if ("|*+?()".contains(c))
                        Right(c)::preprocess(next)
                        else Left(c)::preprocess(next)
      case Nil => Nil
    }
  }


  def concat(s: List[Either[Char,Char]]): (List[Either[Char,Char]], String) ={

    def aux(s: List[Either[Char,Char]], l : List[String]): (List[Either[Char,Char]], List[String]) = {
      //println("concat " + s + "\n\t" + l)
      s match {
        case ::(head, next) =>
          head match {
            case Left(value) =>
              aux(next, ('\'' :: value :: '\'' :: Nil).mkString::l)
            case Right(op) =>  op match {
              case '|' => (next, l)
              case ')' => (s, l)
              case '(' =>
                val p = union(next)
                aux(p._1, p._2::l)
              case c if "+*?".contains(c) => aux(next, unary(c, l.head)::l.tail)
            }
          }
        case Nil => (Nil, l)
      }
    }
    val p = aux(s, Nil)
    val prenex = merge("CONCAT", p._2)
    //println("P " + prenex)
    (p._1, prenex)
  }

  def union(s: List[Either[Char,Char]]): (List[Either[Char,Char]], String) ={

    def aux(s: List[Either[Char,Char]], l : List[String]): (List[Either[Char,Char]], List[String]) = {
      //println("union " + s+ "\n\t" + l)
      s match {
        case ::(head, next) =>
          head match {
            case Left(value) =>
              val p = concat(s)
              aux(p._1, p._2::l)
            case Right(op) =>  op match {
              case '|' => aux(next, l)
              case ')' => (next, l)
              case '(' =>
                val p = concat(s)
                aux(p._1, p._2::l)
            }
          }
        case Nil => (Nil, l)
      }
    }
    val p = aux(s, Nil)
    val prenex = merge("UNION", p._2)
    //println("P " + prenex)
    (p._1, prenex)
  }

  def merge(op : String, words:List[String]):String = {
    //println("merge " + words)
    words.tail.foldLeft(words.head)((s:String, w:String) => op + " " + w + " " + s)
  }

  def unary(op : Char, prenex: String):String = {
    (op match {
      case '?' => "MAYBE "
      case '+' => "PLUS "
      case '*' => "STAR "
    } )+ prenex
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    val preproc = preprocess(str.toList)
    union(preproc)._2
  }

  def main(args: Array[String]): Unit = {
    val str = "\' \'\'a\'"
    //assert(Regex.toPrenex(str) == "CONCAT @ a")
    val s = Regex.toPrenex(str)
    assert(Dfa.fromPrenex(s).accepts(" a"))
    assert(!Dfa.fromPrenex(s).accepts("@a"))
  }
}
