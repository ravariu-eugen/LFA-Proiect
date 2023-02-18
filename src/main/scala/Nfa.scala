import Nfa.eps

import scala.annotation.tailrec


case class Nfa[A](initialState : A, finalStates : Set[A], transitions : Map[A, Map[Char, Set[A]]]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] =
      Nfa(f(initialState),
          finalStates.map(f),
          transitions.map({ case (a, mp) =>
                                  (f(a), mp.map({case (c, set) =>
                                                      (c, set.map(f))
                                                })
                                  )
                          })
      ) // TODO implement map

  def next(state:A, c: Char): Set[A] = transitions.getOrElse(state, Map()).getOrElse(c, Set()) // TODO implement next

  def next(states: Set[A], c: Char): Set[A] = states.map((state:A) => next(state, c)).fold(Set())(_++_)
  def epsilonClosure(state:A) : Set[A] = {
    var states = Set[A](state)
    def aux(state:A):Unit = {
      val nextStates = next(state, eps)
      for(s <- nextStates){
        if(!states.contains(s)) {
          states = states + s
          aux(s)
        }

      }
    }
    aux(state)
    states
  }

  def epsilonClosure(states:Set[A]) : Set[A] = {
    states.map(epsilonClosure).fold(Set())(_++_)
  }

  def accepts(str: String): Boolean = {

    @tailrec
    def acceptAux(str: String, states : Set[A]):Boolean = {
      //println(str + " " + states)
      if(states.isEmpty) return false
      if( str == "") states.exists(isFinal)
      else acceptAux(str.tail, epsilonClosure(next(states, str.head)))
    }
    acceptAux(str, epsilonClosure(initialState))
  } // TODO implement accepts

  def addTransition(state1:A, c: Char, state2:A): Nfa[A] = {
    val trn = transitions.getOrElse(state1, Map())
    val set = trn.getOrElse(c, Set())
    Nfa[A](initialState, finalStates, transitions + (state1 -> (trn + (c->(set + state2)))))
  }
  def getChars : Set[Char] = transitions.map({ case (a, value) => value.keySet.filter(_!=eps)}).fold(Set())(_++_)
  def getChars(state : A): Set[Char] = {
    transitions.getOrElse(state, Map()).keySet.filter(_!=eps)
  }
  def getChars(states : Set[A]) : Set[Char] = {
    states.map(getChars).fold(Set())(_++_)
  }
  def getStates : Set[A] = transitions.keySet // TODO implement getStates

  def isFinal(state: A): Boolean = finalStates.contains(state)  // TODO implement isFinal
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  val eps: Char = 0.toChar
  def fromPrenex(str: String): Nfa[Int] = ExprTree.buildNFA(ExprTree.parse(str))// TODO implement Prenex -> Nfa transformation.

  var nr = 0

  def getNr: Int = {
    val aux = nr
    nr = nr+1
    aux
  }

  def constant(char: Char):Nfa[Int] = {
    val iNew = getNr
    val fNew = getNr
    Nfa(iNew, Set(fNew), Map()).
      addTransition(iNew, char, fNew)
  }

  def void:Nfa[Int] = {
    val iNew = getNr
    val fNew = getNr
    Nfa(iNew, Set(fNew), Map())
  }

  def plus[A](nfa: Nfa[Int]) : Nfa[Int] = {
    val iOld = nfa.initialState
    val fOld = nfa.finalStates
    if(fOld.size > 1) throw new IllegalArgumentException
    val iNew = getNr
    val fNew = getNr
    Nfa(iNew, Set(fNew), nfa.transitions).
      addTransition(iNew, eps, iOld).
      addTransition(fOld.head, eps, fNew).
      addTransition(fOld.head, eps, iOld)
  }
  def star[A](nfa: Nfa[Int]) : Nfa[Int] = {
    val iOld = nfa.initialState
    val fOld = nfa.finalStates
    if(fOld.size > 1) throw new IllegalArgumentException
    val iNew = getNr
    val fNew = getNr
    Nfa(iNew, Set(fNew), nfa.transitions).
      addTransition(iNew, eps, iOld).
      addTransition(fOld.head, eps, fNew).
      addTransition(fOld.head, eps, iOld).
      addTransition(iNew, eps, fNew)
  }
  def maybe[A](nfa: Nfa[Int]): Nfa[Int] = {
    val iOld = nfa.initialState
    val fOld = nfa.finalStates
    if(fOld.size > 1) throw new IllegalArgumentException
    val iNew = getNr
    val fNew = getNr
    Nfa(iNew, Set(fNew), nfa.transitions).
      addTransition(iNew, eps, iOld).
      addTransition(fOld.head, eps, fNew).
      addTransition(iNew, eps, fNew)
  }
  def concat[A](nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
    val iOld1 = nfa1.initialState
    val fOld1 = nfa1.finalStates
    if(fOld1.size > 1) throw new IllegalArgumentException
    val iOld2 = nfa2.initialState
    val fOld2 = nfa2.finalStates

    Nfa(iOld1, fOld2, nfa1.transitions ++ nfa2.transitions).
      addTransition(fOld1.head, eps, iOld2)
  }
  def union[A](nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
    val iOld1 = nfa1.initialState
    val fOld1 = nfa1.finalStates
    if(fOld1.size > 1) throw new IllegalArgumentException
    val iOld2 = nfa2.initialState
    val fOld2 = nfa2.finalStates
    if(fOld2.size > 1) throw new IllegalArgumentException
    val iNew = getNr
    val fNew = getNr
    Nfa(iNew, Set(fNew), nfa1.transitions ++ nfa2.transitions).
      addTransition(iNew, eps, iOld1).
      addTransition(iNew, eps, iOld2).
      addTransition(fOld1.head, eps, fNew).
      addTransition(fOld2.head, eps, fNew)
  }
    // You can add more methods to this object

  def main(args: Array[String]): Unit = {
    // write your tests here

    val nfa = fromPrenex("CONCAT a b")
    println(nfa)
    println(nfa.accepts("aba"))
    println(nfa.next(6, Nfa.eps))




  }
}