

trait ExprTree {
}
case object Void extends ExprTree {
}

case object Epsilon extends ExprTree {
}

case class Constant(char: Char) extends ExprTree{
}

case class Star(expr : ExprTree) extends ExprTree{
}

case class Union(expr1 : ExprTree, expr2 : ExprTree) extends ExprTree{
}

case class Concat(expr1 : ExprTree, expr2 : ExprTree) extends ExprTree{
}

case class Plus(expr : ExprTree) extends ExprTree{
}

case class Maybe(expr : ExprTree) extends ExprTree{
}



object ExprTree{
  def buildNFA(exprTree: ExprTree):Nfa[Int] = {
    exprTree match {
      case Void => Nfa.void
      case Epsilon => Nfa.constant(Nfa.eps)
      case Constant(char) => Nfa.constant(char)
      case Plus(expr) => Nfa.plus(buildNFA(expr))
      case Star(expr) => Nfa.star(buildNFA(expr))
      case Maybe(expr) => Nfa.maybe(buildNFA(expr))
      case Concat(expr1, expr2) => Nfa.concat(buildNFA(expr1), buildNFA(expr2))
      case Union(expr1, expr2) => Nfa.union(buildNFA(expr1), buildNFA(expr2))
    }
  }

  def parse(expr : String): ExprTree = {

    val words = "('.')|[^ ]+".r.findAllMatchIn(expr).toList.map(_.toString())
    //println(words)
    def parseAux(words:List[String]): List[ExprTree] = {
      if(words.isEmpty)
        return Nil
      val stack = parseAux(words.tail)

      val word = words.head
      word match {
        case "STAR" => Star(stack.head) :: stack.tail
        case "PLUS" => Plus(stack.head) :: stack.tail
        case "MAYBE" => Maybe(stack.head) :: stack.tail
        case "UNION" => Union(stack.head, stack.tail.head) :: stack.tail.tail
        case "CONCAT" => Concat(stack.head, stack.tail.head) :: stack.tail.tail
        case "eps" => Epsilon :: stack
        case "void" => Void :: stack
        case c if c.head == '\'' && c.length == 3 => Constant(c.charAt(1)) :: stack // pt caractere intre ghilimele
        case c if c.length == 1 => Constant(c.head) :: stack // pt caractere alfanumerice
      }

    }
    parseAux(words).head
  }

}

