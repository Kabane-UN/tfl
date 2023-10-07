import scala.collection.mutable
import scala.io.Source.fromFile
import scala.util.Random
import scala.util.control.Breaks.*
import java.io.PrintWriter
import java.io.File
import scala.util.matching.Regex

class GenException extends RuntimeException{
  override def toString: String = "Некорректные параметры генерации"
}

class State(val number: Int, val isInitial: Boolean, val isAccepting: Boolean){
  private var fromTransitions: List[(Int, List[String])] = List()
  private var toTransitions: List[(Int, List[String])] = List()
  private var stringsToInitial: List[String] = List()
  def appendFromTransition(transition: (Int, List[String])): Unit = {
    fromTransitions = fromTransitions :+ transition
  }

  def appendToTransition(transition: (Int, List[String])): Unit = {
    toTransitions = toTransitions :+ transition
  }

  def appendStringsToInitial(strings: List[String]): Unit = {
    this.stringsToInitial = strings
  }
  def getStringsToInitial(): List[String] = this.stringsToInitial
  def getFromTransitions(): List[(Int, List[String])] = fromTransitions
  def getToTransitions(): List[(Int, List[String])] = toTransitions
  def getFromTransitionFor(number: Int): (Int, List[String]) = {
    var res: (Int, List[String]) = null
    for (i <- fromTransitions){
      if (i(0) == number){
        res = i
      }
    }
    res
  }
}
class FSM(initialState: Int, private var transitionsMatrix: Array[Array[List[String]]],
          acceptingStates: Array[Int], forRegex: Boolean, private var termsAlphabet: Set[String] = Set(),maxRegexLen: Int = 0, maxStarLen: Int = 0){
  private var alphabet = Set("(", ")", "a", "*", "|", "#")
  if (forRegex) {
    alphabet = alphabet ++ termsAlphabet
    genNewFoThisAlphabet()
  } else {
    alphabet = termsAlphabet
  }
  private var states: List[State] = List()
  transitionsMatrix = genTrap()
  private var noAcceptingStatesList: List[Int] = List()
  for (i <- transitionsMatrix.indices){
    if(!acceptingStates.contains(i)){
      noAcceptingStatesList = noAcceptingStatesList :+ i
    }
  }
  private val noAcceptingStates: Array[Int] = noAcceptingStatesList.toArray
  for (i <- transitionsMatrix.indices){
    states = states :+ State(i, i==initialState, acceptingStates.contains(i))
  }
  for (i <- states.indices){
    for (j <- states.indices){
      if (transitionsMatrix(i)(j).nonEmpty)
        states(j).appendFromTransition((i, transitionsMatrix(i)(j)))
    }
  }
  for (i <- states.indices) {
    for (j <- states.indices) {
      if (transitionsMatrix(i)(j).nonEmpty)
        states(i).appendToTransition((j, transitionsMatrix(i)(j)))
    }
  }
  for (i <- states.indices){
    states(i).appendStringsToInitial(genStrings(genTransitionsToInitialState(i)))
  }

  private def genNormalString(retStr: String, closeStack: mutable.Stack[String],
                              openStack: mutable.Stack[String]): String = {
    var rezStr = retStr.reverse
    for (_ <- closeStack.toList.indices){
      rezStr = "(" + rezStr
    }
    for (_ <- openStack.toList.indices){
      rezStr = rezStr + ")"
    }
    rezStr
  }

  private def genAvailableTransitions(retStr: String, currentState: Int, closeStack: mutable.Stack[String],
                                      openStack: mutable.Stack[String], starCount: Int): List[(Int, String)] = {
    var res: List[(Int, String)] = List()
    for (i <- this.states(currentState).getFromTransitions()){
      for (j <- i(1)){
        var available = false
        breakable {
          for (k <- states(i(0)).getStringsToInitial()) {
            var cloneStarCount = starCount
            var cloneRetStr = retStr
            val cloneCloseStack = closeStack.clone()
            val cloneOpenStack = openStack.clone()
            var availableByStars = true
            for (h: Char <- j + k) {
              val term = h.toString
              if (this.forRegex) {
                if (term == ")" && cloneRetStr.nonEmpty && cloneRetStr.last == '*' && cloneStarCount >= this.maxStarLen) {
                  availableByStars = false
                } else if (term == "*" && cloneStarCount >= this.maxStarLen) {
                  availableByStars = false
                }
                if (term == ")" && cloneRetStr.nonEmpty && cloneRetStr.last == '*') {
                  cloneStarCount += 1
                  cloneCloseStack.push("*")
                } else if (term == ")") {
                  cloneCloseStack.push(")")
                } else if (term == "(" && cloneCloseStack.nonEmpty) {
                  val term = cloneCloseStack.pop()
                  if (term == "*") {
                    cloneStarCount -= 1
                  }
                } else if (term == "(") {
                  cloneOpenStack.push("(")
                }
              }
              cloneRetStr += term
            }
            if (this.maxRegexLen >= genNormalString(cloneRetStr, cloneCloseStack.clone(), cloneOpenStack.clone()).length && availableByStars) {
              available = true
              break
            }
          }
        }
        if (available){
          res = res :+ (i(0), j)
        }
      }
    }
    res
  }
  private def isAcceptable(states: Array[Int]): Boolean = {
    var acceptable: Boolean = false
    for (i <- states){
      if (this.genAvailableTransitions("", i, mutable.Stack[String](), mutable.Stack[String](), 0).nonEmpty){
        acceptable = true
      }
    }
    acceptable
  }
  def randomStringInLanguage(): String = {
    if (!this.isAcceptable(acceptingStates)){
      throw GenException()
    }
    val random = Random()
    var res = ""
    var starCount = 0
    val closeStack = mutable.Stack[String]()
    val openStack = mutable.Stack[String]()
    var currentState = states(acceptingStates(random.nextInt(acceptingStates.length)))

    if (currentState.isInitial || this.genAvailableTransitions(res, currentState.number, closeStack.clone(), openStack.clone(), starCount).nonEmpty) {
      var prevState = currentState
      breakable {
        while (true) {
          if (currentState.isInitial) {
            if (random.nextBoolean()) {
              break
            }
          }
          val transitions = this.genAvailableTransitions(res, currentState.number, closeStack.clone(), openStack.clone(),starCount)
          if (transitions.nonEmpty) {
            val transition = transitions(random.nextInt(transitions.length))
            val newTerm = transition(1)
            if (this.forRegex) {
              if (newTerm == ")" && res.nonEmpty && res.last == '*' && starCount >= this.maxStarLen) {
                res = res.dropRight(1)
              } else if (newTerm == "*" && starCount >= this.maxStarLen) {
                currentState = prevState
              } else {
                if (newTerm == ")" && res.nonEmpty && res.last == '*') {
                  starCount += 1
                  closeStack.push("*")
                } else if (newTerm == ")") {
                  closeStack.push(")")
                } else if (newTerm == "(" && closeStack.nonEmpty) {
                  val term = closeStack.pop()
                  if (term == "*") {
                    starCount -= 1
                  }
                } else if (newTerm == "(") {
                  openStack.push("(")
                }
                prevState = currentState
                currentState = states(transition(0))
                res += newTerm
              }
            } else {
              currentState = states(transition(0))
              res += newTerm
            }
          } else if (currentState.isInitial) {
            break
          } else {
            throw GenException()
          }
        }
      }
      this.genNormalString(res, closeStack, openStack)
    } else {
      this.randomStringInLanguage()
    }
  }
  def randomStringNotInLanguage(): String = {
    if (!this.isAcceptable(noAcceptingStates)) {
      throw GenException()
    }
    val random = Random()
    var res = ""
    val starCount = 0
    val closeStack = mutable.Stack[String]()
    val openStack = mutable.Stack[String]()
    var currentState = states(noAcceptingStates(random.nextInt(noAcceptingStates.length)))
    if (currentState.isInitial || this.genAvailableTransitions(res, currentState.number, closeStack.clone(), openStack.clone(), starCount).nonEmpty) {
      breakable {
        while (true) {
          if (currentState.isInitial) {
            if (random.nextBoolean()) {
              break
            }
          }
          val transitions = this.genAvailableTransitions(res, currentState.number, closeStack.clone(), openStack.clone(), starCount)
          if (transitions.nonEmpty) {
            val transition = transitions(random.nextInt(transitions.length))
            val newTerm = transition(1)
            currentState = states(transition(0))
            res += newTerm
          } else if (currentState.isInitial) {
            break
          } else {
            throw GenException()
          }
        }
      }
      this.genNormalString(res, closeStack, openStack)
    } else {
      this.randomStringNotInLanguage()
    }
  }
  def matches(str: String): Boolean = {
    var currentState = states(initialState)
    var isBreak = false
    breakable {
      for (i <- str) {
        val term = i.toString
        var haveTransitions = false
        var nextState: State = null
        for (j <- currentState.getToTransitions()) {
          if (j(1).contains(term)) {
            nextState = states(j(0))
            haveTransitions = true
          }
        }
        if (haveTransitions) {
          currentState = nextState
        } else {
          isBreak = true
          break
        }
      }
    }
    !isBreak && currentState.isAccepting
  }
  private def genStrings(statesQueue: mutable.Queue[Int]): List[String] = {
    if (statesQueue.toList.length == 1){
      List("")
    } else if (!this.forRegex){
      val toState = statesQueue.dequeue()
      val fromState = statesQueue.front
      var res = this.states(toState).getFromTransitionFor(fromState)(1)(0)
      res += genStrings(statesQueue).head
      List(res)
    } else {
      val toState = statesQueue.dequeue()
      val fromState = statesQueue.front
      var res: List[String] = List()
      for (i <- this.states(toState).getFromTransitionFor(fromState)(1)){
        res = res :+ i
      }
      var rez: List[String] = List()
      val retRes = genStrings(statesQueue)
      for (i <- res.indices){
        for (j <- retRes.indices){
          rez = rez :+ (res(i)+retRes(j))
        }
      }
      rez
    }
  }

  private def genTransitionsToInitialState(fo: Int): mutable.Queue[Int] = {
    val queueOfStates: mutable.Queue[State] = mutable.Queue()
    queueOfStates.enqueue(this.states(fo))
    var paths: Map[Int, mutable.Queue[Int]] = Map()
    var visitedStates: Set[State] = Set()
    visitedStates = visitedStates + this.states(fo)
    paths += fo -> mutable.Queue(fo)
    var currentState = queueOfStates.dequeue()
    while (currentState.number != this.initialState) {
      for (i <- currentState.getFromTransitions()) {
        if (!visitedStates(this.states(i(0)))) {
          visitedStates = visitedStates + this.states(i(0))
          queueOfStates.enqueue(this.states(i(0)))
          paths += i(0) -> (paths(currentState.number) :+ i(0))
        }
      }
      currentState = queueOfStates.dequeue()
    }
    paths(currentState.number)
  }
  private def genNewFoThisAlphabet(): Unit = {
    for (i <- this.transitionsMatrix.indices){
      for (j <- this.transitionsMatrix.indices){
        if ((this.termsAlphabet & this.transitionsMatrix(i)(j).toSet).nonEmpty) {
          for (term <- this.termsAlphabet &~ this.transitionsMatrix(i)(j).toSet) {
            this.transitionsMatrix(i)(j) = this.transitionsMatrix(i)(j) :+ term
          }
        }
      }
    }
  }

  private def genTrap(): Array[Array[List[String]]]= {
    var needATrap = false
    for (i <- this.transitionsMatrix.indices){
      var transitionsAlphabet: Set[String] = Set()
      for (j <- this.transitionsMatrix(i).indices){
        for (term <- this.transitionsMatrix(i)(j)){
          transitionsAlphabet = transitionsAlphabet + term
        }
      }
      if ((alphabet &~ transitionsAlphabet).nonEmpty){
        needATrap = true
      }
    }
    if (needATrap) {
      val newTransitionsMatrix: Array[Array[List[String]]] = Array.fill(this.transitionsMatrix.length+1)(Array.fill(this.transitionsMatrix.length+1)(List()))
      for (i <- this.transitionsMatrix.indices) {
        val transitionsArray: Array[List[String]] = Array.fill(this.transitionsMatrix.length+1)(List())
        var transitionsAlphabet: Set[String] = Set()
        for (j <- this.transitionsMatrix.indices) {
          for (term <- this.transitionsMatrix(i)(j)) {
            transitionsAlphabet = transitionsAlphabet + term
          }
          transitionsArray(j) = this.transitionsMatrix(i)(j)
        }
        var trapTransition: List[String] = List()
        for (term <- alphabet &~ transitionsAlphabet) {
          trapTransition = trapTransition :+ term
        }
        transitionsArray(this.transitionsMatrix.length) = trapTransition
        newTransitionsMatrix(i) = transitionsArray
      }
      val transitionsArray: Array[List[String]] = Array.fill(this.transitionsMatrix.length+1)(List())
      for (i <- this.transitionsMatrix.indices){
        transitionsArray(i) = List()
      }
      transitionsArray(this.transitionsMatrix.length) = alphabet.toList
      newTransitionsMatrix(this.transitionsMatrix.length) = transitionsArray
      newTransitionsMatrix
    } else {
      transitionsMatrix
    }
  }
}

object main{
  private def parse(lines: List[String]): (List[(Int, Array[Array[List[String]]], Array[Int], Set[String])], List[Regex]) = {
    var res: List[(Int, Array[Array[List[String]]], Array[Int], Set[String])] = List()
    var resRegex: List[Regex] = List()
    var subRes: (Int, Array[Array[List[String]]], Array[Int], Set[String]) = (0, Array(), Array(), Set())
    var flag = ""
    for (line <- lines){
      if (line == "#states"){
        flag = "states"
      } else if (line == "#initial"){
        flag = "initial"
      } else if (line == "#accepting"){
        flag = "accepting"
      } else if (line == "#alphabet"){
        flag = "alphabet"
      } else if (line == "#transitions"){
        flag = "transitions"
      } else if (line == "-"){
        res = res :+ subRes
        flag = "-"
      } else if (line == "#regex"){
        flag = "regex"
      } else {
        if (flag == "states"){
          subRes = (0, Array(), Array(), Set())
          val n = line.toInt
          subRes = (subRes(0), Array.fill(n)(Array.fill(n)(List())), subRes(2), subRes(3))
        } else if (flag == "initial"){
          subRes = (line.toInt, subRes(1), subRes(2), subRes(3))
        } else if (flag == "accepting"){
          subRes = (subRes(0), subRes(1), subRes(2) :+ line.toInt, subRes(3))
        } else if (flag == "alphabet") {
          subRes = (subRes(0), subRes(1), subRes(2), subRes(3) + line)
        } else if (flag == "transitions"){
          var subLines = line.split(":")
          val fromState = subLines(0).toInt
          subLines = subLines(1).split(">")
          val toState = subLines(1).toInt
          val byTerm = subLines(0)
          val matrix = subRes(1)
          matrix(fromState)(toState) = matrix(fromState)(toState) :+ byTerm
          subRes = (subRes(0), matrix, subRes(2), subRes(3))
        } else if (flag == "regex"){
          resRegex = resRegex :+ line.r
        }
      }
    }
    if (flag != "-"){
      res = res :+ subRes
    }
    (res, resRegex)
  }
  def main(args: Array[String]): Unit = {
    if (args.length == 5 && args(0) == "-r"){
      val initialState = 0
      val transitionsMatrix: Array[Array[List[String]]] = Array(
        Array(List("("), List("a"), List[String](), List("*", ")", "#", "|")),
        Array(List("(", "#", "|"), List("a", ")"), List("*"), List[String]()),
        Array(List("(", "#", "|"), List("a", ")"), List[String](), List("*")),
        Array(List[String](), List[String](), List[String](), List("a", "(", ")", "*", "#", "|"))
      )
      val acceptingStates: Array[Int] = Array(1, 2)
      var regexStrings: List[String] = List()
      val maxAlphabet = args(1).toInt
      val maxRegexLen = args(2).toInt
      val maxStarLen = args(3).toInt
      val regexCount = args(4).toInt
      val requiredTerms = List("a", "b", "c", "d", "e")
      val termsAlphabet = requiredTerms.slice(0, maxAlphabet).toSet
      val fsm = FSM(initialState, transitionsMatrix, acceptingStates, true, termsAlphabet, maxRegexLen, maxStarLen)
      for (_ <- 0 until regexCount) {
        regexStrings = regexStrings :+ fsm.randomStringInLanguage()
      }
      val p = PrintWriter(File("regex.txt"))
      regexStrings.foreach(p.println)
      p.close()
    } else if (args.length == 4 && args(0) == "-w"){
      val lines = fromFile("fsm.txt").getLines()
      var linesList: List[String] = List()
      for (line <- lines) {
        linesList = linesList :+ line
      }
      val (resFsm, resRegex) = this.parse(linesList)
      val maxWordsLen = args(1).toInt
      val lenOfTests = args(2).toInt
      val useAdditionalAlphabet = args(3).toBoolean
      val lenOfInTests = lenOfTests/2
      val lenOfNotInTests = lenOfTests - lenOfInTests
      var fsms: List[FSM] = List()
      val additionalAlphabet = Set("a", "b", "c", "d", "e")
      for (i <- resFsm) {
        if (useAdditionalAlphabet) {
          fsms = fsms :+ FSM(i(0), i(1), i(2), false, i(3) ++ additionalAlphabet, maxWordsLen)
        } else {
          fsms = fsms :+ FSM(i(0), i(1), i(2), false, i(3), maxWordsLen)
        }
      }
      var fail = false
      breakable{
        for (i <- fsms.indices){
          breakable {
            for (_ <- 0 until lenOfInTests) {
              var word = ""
              var gen = true
              try {
                word = fsms(i).randomStringInLanguage()
              } catch {
                case _: GenException =>
                  gen = false
                  println(s"Can't generate word in FSM №$i")
                  println("Test skipped")
                  break
              }
              if (gen) {
                if (!fsms(i).matches(word)) {
                  println("FUZZ Test Error")
                  fail = true
                  break
                }
                if (!resRegex(i).matches(word)) {
                  println("FAIL")
                  println(s"In FSM №$i")
                  println(s"In word: $word")
                  println("Expected: match")
                  println("Got: don't match")
                  fail = true
                  break
                }
              }
            }
          }
          if (fail){
            break
          }
          breakable {
            for (_ <- 0 until lenOfNotInTests) {
              var word = ""
              var gen = true
              try {
                word = fsms(i).randomStringNotInLanguage()
              } catch {
                case _: GenException =>
                  gen = false
                  println(s"Can't generate word not in FSM №$i")
                  println("Test skipped")
                  break
              }
              if (gen) {
                if (fsms(i).matches(word)) {
                  println("FUZZ Test Error")
                  fail = true
                  break
                }
                if (resRegex(i).matches(word)) {
                  println("FAIL")
                  println(s"In FSM №$i")
                  println(s"In word: $word")
                  println("Expected: don't match")
                  println("Got: match")
                  fail = true
                  break
                }
              }
            }
          }
          if (fail){
            break
          }
        }
      }
      if (!fail){
        println("PASS")
      }
    }
  }
}