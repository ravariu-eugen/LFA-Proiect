case class Dfa[A] (initialState : A,
                   finalStates : Set[A],
                   sinkState : A,
                   transitions : Map[A, Map[Char, A]]/* TODO : define the constructor params */){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B]=
    Dfa(f(initialState), finalStates.map(f), f(sinkState),
      transitions.map({ case (a, mp) =>
        (f(a), mp.map({ case (c, state) => (c, f(state))}))
      })
    ) // TODO implement map

  def next(state:A, c: Char): A = transitions.getOrElse(state, Map()).getOrElse(c, sinkState) // TODO implement next

  def accepts(str: String): Boolean = {
    def acceptAux(str: String, state: A):Boolean = {
      //println(str + " " + state)
      if(state == sinkState) return false
      if(str == "" || str.isEmpty) return finalStates.contains(state)
      acceptAux(str.tail, next(state, str.head))
    }
    acceptAux(str, initialState)
  } // TODO implement accepts

  def getStates : Set[A] = transitions.keySet + sinkState // TODO implement getStates

  def isFinal(state: A): Boolean = finalStates.contains(state)  // TODO implement isFinal

  def addTransition(state1:A, c: Char, state2:A): Dfa[A] = {
    val trn = transitions.getOrElse(state1, Map())
    Dfa[A](initialState, finalStates, sinkState, transitions + (state1 -> (trn + (c-> state2))))
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {

  var nr = 0
  def getNr: Int = {
    nr = nr+1
    nr
  }
  def fromPrenex(str: String): Dfa[Int] = fromNfa(Nfa.fromPrenex(str)) // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa
  def fromNfa[A](nfa: Nfa[A]): Dfa[Int] ={
    val initialStates = nfa.epsilonClosure(nfa.initialState)
    def next(states: Set[A], char: Char): Set[A] = nfa.epsilonClosure(nfa.next(states, char))
    var foundStates:Set[Set[A]] = Set(initialStates)
    var transitions:Set[(Set[A], Char, Set[A])] = Set()
    def aux(states :Set[A]):Unit = {
      val letters = nfa.getChars(states)
      for(c <- letters){
        val nextState = next(states, c)
        val trn: (Set[A], Char, Set[A]) = (states, c, nextState)
        transitions = transitions + trn
        if(!foundStates.contains(nextState)) {
          foundStates = foundStates + nextState

          aux(nextState)
        }
      }

    }
    var coversionMap:Map[Set[A], Int] = Map()
    def myMap(set: Set[A]):Int = {
      coversionMap.get(set) match {
        case Some(value) => value
        case None =>
          val nr = getNr
          coversionMap = coversionMap + (set->nr)
          nr

      }
    }
    aux(initialStates)
    //println(foundStates.map(myMap))
    //println(transitions.map((f:(Set[A],Char,Set[A]))=> (myMap(f._1), f._2, myMap(f._3))))

    val initialState = myMap(initialStates)
    val finalStates = foundStates.filter(_.exists(nfa.isFinal)).map(myMap)
    val sinkState = 0
    val tempTrans = transitions.map((f:(Set[A],Char,Set[A]))=> (myMap(f._1), f._2, myMap(f._3)))

    val init = Dfa(initialState, finalStates, sinkState, Map())

    def ff(dfa: Dfa[Int], tuple: (Int, Char, Int)): Dfa[Int] = {
      dfa.addTransition(tuple._1, tuple._2, tuple._3)
    }
    tempTrans.foldLeft(init)(ff)


  }
  def main(args: Array[String]): Unit = {
    // write your tests here
    val str = "' '"
    println(Nfa.fromPrenex(str))
    val x = Dfa.fromPrenex(str)
    println(x)
    println(x.accepts(" "))
  }
  // You can add more methods to this object
}
