import scala.io.Source.fromFile
import java.io.File,java.io.PrintWriter

// тоже часть старого парсера
//class ParserException extends RuntimeException{
//  override def toString: String = {
//    "Parser Exception("
//  }
//}
package ArcAlgebra{
  sealed trait Exp
  class ArcAlgebraException extends RuntimeException{
    override def toString: String = {
      "Такого не может быть!!!"
    }
  }

  private final case class ArcAdd(args: List[Exp]) extends Exp {
    override def toString: String = {
      var polish = s"(arcAdd${args.length} "
      for (arg <- args) {
        polish += s"${arg.toString} "
      }
      polish = polish.dropRight(1)
      polish += ")"
      polish
    }
  }

  private final case class ArcMul(args: List[Exp]) extends Exp {
    override def toString: String = {
      var polish = s"(arcMul${args.length} "
      for (arg <- args) {
        polish += s"${arg.toString} "
      }
      polish = polish.dropRight(1)
      polish += ")"
      polish
    }
  }

  final case class Coefficient(name: String, exp: Exp) extends Exp {
    override def toString: String = {
      exp match {
        case null => name
        case ArcAdd(_) => exp.toString
        case ArcMul(_) => exp.toString
        case Coefficient(_, _) =>
          throw ArcAlgebraException()
      }
    }
  }

  def simplifyMul(exp: Exp, newCoefficient: Coefficient, firstStep: Boolean = true): Exp = exp match {
    case Coefficient(_, exp1) =>
      exp1 match {
        case ArcAdd(_) => Coefficient(null, simplifyMul(exp1, newCoefficient, false))
        case null =>
          if (firstStep) {
            Coefficient(null, ArcMul(List(exp, newCoefficient)))
          } else {
            ArcMul(List(exp, newCoefficient))
          }
        case _ =>
          throw ArcAlgebraException()
      }
    case ArcAdd(args) =>
      var newArgs: List[Exp] = List()
      for (arg <- args) {
        newArgs = newArgs :+ simplifyMul(arg, newCoefficient, firstStep)
      }
      ArcAdd(newArgs)
    case ArcMul(args) =>
      ArcMul(args :+ newCoefficient)
  }

  def simplifyAdd(exp1: Exp, exp2: Exp): Coefficient = (exp1, exp2) match {
    case (Coefficient(_, childExp1), Coefficient(_, childExp2)) =>
      (childExp1, childExp2) match {
        case (ArcAdd(args1), ArcAdd(args2)) =>
          var newArgs: List[Exp] = List()
          for (arg <- args1) {
            newArgs = newArgs :+ arg
          }
          for (arg <- args2) {
            newArgs = newArgs :+ arg
          }
          Coefficient(null, ArcAdd(newArgs))
        case (ArcAdd(args1), null) =>
          var newArgs: List[Exp] = List()
          for (arg <- args1) {
            newArgs = newArgs :+ arg
          }
          newArgs = newArgs :+ exp2
          Coefficient(null, ArcAdd(newArgs))
        case (ArcMul(_), ArcMul(_)) => Coefficient(null, ArcAdd(List(childExp1, childExp2)))
        case _ =>
          throw ArcAlgebraException()
      }
    case _ =>
      throw ArcAlgebraException()
  }
  def countMaxArcAddArgs(exp: Exp, count:Int = 2): Int = exp match{
    case Coefficient(_, exp1) =>
      exp1 match {
        case null => count
        case _ => countMaxArcAddArgs(exp1, count)

      }
    case ArcAdd(args) =>
      var maxCount = count max args.length
      for (arg <- args) {
        maxCount = maxCount max countMaxArcAddArgs(arg, maxCount)
      }
      maxCount
    case ArcMul(args) =>
      var maxCount = count
      for (arg <- args){
        maxCount = maxCount max countMaxArcAddArgs(arg, maxCount)
      }
      maxCount
  }
  def countMaxArcMulArgs(exp: Exp, count: Int = 2): Int = exp match {
    case Coefficient(_, exp1) =>
      exp1 match {
        case null => count
        case _ => countMaxArcMulArgs(exp1, count)

      }
    case ArcMul(args) =>
      var maxCount = count max args.length
      for (arg <- args) {
        maxCount = maxCount max countMaxArcMulArgs(arg, maxCount)
      }
      maxCount
    case ArcAdd(args) =>
      var maxCount = count
      for (arg <- args) {
        maxCount = maxCount max countMaxArcMulArgs(arg, maxCount)
      }
      maxCount
  }
}




final class Ent(fName:String, fVar: Ent, func: Boolean){
  import ArcAlgebra._

  var name: String = fName
  private val arg: Ent = fVar
  private val isFunc: Boolean = func

  var a: Coefficient = null
  var b: Coefficient = null
  var c: Coefficient = null
  var d: Coefficient = null
  var e: Coefficient = null
  var f: Coefficient = null


  private var nameA = ""
  private var nameB = ""
  private var nameC = ""
  private var nameD = ""
  private var nameE = ""
  private var nameF = ""


  def coefficientsToString(): String = {
    s"""a = ${this.a}
       |b = ${this.b}
       |c = ${this.c}
       |d = ${this.d}
       |e = ${this.e}
       |f = ${this.f}
       |""".stripMargin
  }

  private def countMaxArcAddArgsOfThis(): Int = {
    countMaxArcAddArgs(this.a) max countMaxArcAddArgs(this.b) max countMaxArcAddArgs(this.c) max countMaxArcAddArgs(this.d) max
      countMaxArcAddArgs(this.e) max countMaxArcAddArgs(this.f)
  }

  private def countMaxArcMulArgsOfThis(): Int = {
    countMaxArcMulArgs(this.a) max countMaxArcMulArgs(this.b) max countMaxArcMulArgs(this.c) max countMaxArcMulArgs(this.d) max
      countMaxArcMulArgs(this.e) max countMaxArcMulArgs(this.f)
  }

  private def setCoefficients(newA: Coefficient, newB: Coefficient, newC: Coefficient,
                              newD: Coefficient, newE: Coefficient, newF: Coefficient): Unit = {
    this.a = newA
    this.b = newB
    this.c = newC
    this.d = newD
    this.e = newE
    this.f = newF
    this.nameA = newA.toString
    this.nameB = newB.toString
    this.nameC = newC.toString
    this.nameD = newD.toString
    this.nameE = newE.toString
    this.nameF = newF.toString
  }



  private def makeTree(hist: Map[String, Ent], numOfCoefficients: Int): (Map[String, Ent], Int) = {
    var (newHist, newNumOfCoefficients) = (hist, numOfCoefficients)
    if (this.isFunc){
      if (this.arg.isFunc){
        val res = this.arg.makeTree(hist, numOfCoefficients)
        newHist ++= res(0)
        newNumOfCoefficients = res(1)
        var thisCoefficientA:Coefficient = null
        var thisCoefficientB:Coefficient = null
        var thisCoefficientC:Coefficient = null
        var thisCoefficientD:Coefficient = null
        var thisCoefficientE:Coefficient = null
        var thisCoefficientF:Coefficient = null
        if (newHist.contains(s"${this.name}")){
          val entFromHist = newHist(s"${this.name}")
          thisCoefficientA = Coefficient(s"${entFromHist.nameA}", null)
          thisCoefficientB = Coefficient(s"${entFromHist.nameB}", null)
          thisCoefficientC = Coefficient(s"${entFromHist.nameC}", null)
          thisCoefficientD = Coefficient(s"${entFromHist.nameD}", null)
          thisCoefficientE = Coefficient(s"${entFromHist.nameE}", null)
          thisCoefficientF = Coefficient(s"${entFromHist.nameF}", null)
        } else {
          newNumOfCoefficients += 1
          thisCoefficientA = Coefficient(s"a$newNumOfCoefficients", null)
          thisCoefficientB = Coefficient(s"b$newNumOfCoefficients", null)
          thisCoefficientC = Coefficient(s"c$newNumOfCoefficients", null)
          thisCoefficientD = Coefficient(s"d$newNumOfCoefficients", null)
          thisCoefficientE = Coefficient(s"e$newNumOfCoefficients", null)
          thisCoefficientF = Coefficient(s"f$newNumOfCoefficients", null)
        }
        this.nameA = thisCoefficientA.toString
        this.nameB = thisCoefficientB.toString
        this.nameC = thisCoefficientC.toString
        this.nameD = thisCoefficientD.toString
        this.nameE = thisCoefficientE.toString
        this.nameF = thisCoefficientF.toString

        this.a = simplifyAdd(simplifyMul(this.arg.a, thisCoefficientA),
          simplifyMul(this.arg.c, thisCoefficientB))
        this.b = simplifyAdd(simplifyMul(this.arg.b, thisCoefficientA),
          simplifyMul(this.arg.d, thisCoefficientB))
        this.c = simplifyAdd(simplifyMul(this.arg.a, thisCoefficientC),
          simplifyMul(this.arg.c, thisCoefficientD))
        this.d = simplifyAdd(simplifyMul(this.arg.b, thisCoefficientC),
          simplifyMul(this.arg.d, thisCoefficientD))

        this.e = simplifyAdd(simplifyAdd(simplifyMul(this.arg.e, thisCoefficientA),
          simplifyMul(this.arg.f, thisCoefficientB)), thisCoefficientE)
        this.f = simplifyAdd(simplifyAdd(simplifyMul(this.arg.e, thisCoefficientC),
          simplifyMul(this.arg.f, thisCoefficientD)), thisCoefficientF)
        if (!newHist.contains(s"${this.name}")) {
          newHist += (s"${this.name}" -> this)
        }
      } else {
        if (newHist.contains(s"${this.name}")){
          val entFromHist = newHist(s"${this.name}")
          this.setCoefficients(Coefficient(s"${entFromHist.nameA}", null), Coefficient(s"${entFromHist.nameB}", null), Coefficient(s"${entFromHist.nameC}", null),
            Coefficient(s"${entFromHist.nameD}", null), Coefficient(s"${entFromHist.nameE}", null), Coefficient(s"${entFromHist.nameF}", null))
        } else {
          newNumOfCoefficients+=1
          this.setCoefficients(Coefficient(s"a$newNumOfCoefficients", null),Coefficient(s"b$newNumOfCoefficients", null),
            Coefficient(s"c$newNumOfCoefficients", null), Coefficient(s"d$newNumOfCoefficients", null),
              Coefficient(s"e$newNumOfCoefficients", null),Coefficient(s"f$newNumOfCoefficients", null))
          newHist += (s"${this.name}" -> this)

        }
      }
    }
    (newHist, newNumOfCoefficients)
  }

  def makeTreeAndCountMaxArc(hist: Map[String, Ent], numOfCoefficients: Int): (Map[String, Ent], Int, Int, Int) = {
    val (resHist, resNumOfCoefficients) = this.makeTree(hist, numOfCoefficients)
    (resHist, resNumOfCoefficients, this.countMaxArcAddArgsOfThis(), this.countMaxArcMulArgsOfThis())
  }

  override def toString: String = {
    if (this.isFunc) {
      var resStr = s"${this.name}("
      resStr += s"${this.arg.toString},"
      resStr = resStr.dropRight(1)
      resStr :+ ')'
    } else {
      this.name
    }
  }
}


class TRS(lines: List[String]){
  import ArcAlgebra.Coefficient
  private val TRSLines = lines
  private var funcs: List[List[Ent]] = List()
  private var funcsCoefficientPairs: List[Map[Coefficient, Coefficient]] = List()
  private var hist: Map[String, Ent] = Map()
  private var numOfCoefficients = 0
  private var maxNumOfArgAddArgs = 2
  private var maxNumOfArgMulArgs = 2
  for (line <- TRSLines) {
    val lineTS = line.filterNot(_.isWhitespace)
    val lineSpArr = lineTS.split("->")
    val f1 = parser(lineSpArr(0))
    val f2 = parser(lineSpArr(1))
    var res = f1.makeTreeAndCountMaxArc(hist, numOfCoefficients)
    hist ++= res(0)
    numOfCoefficients = res(1)
    maxNumOfArgAddArgs = maxNumOfArgAddArgs max res(2)
    maxNumOfArgMulArgs = maxNumOfArgMulArgs max res(3)
    res = f2.makeTreeAndCountMaxArc(hist, numOfCoefficients)
    hist ++= res(0)
    numOfCoefficients = res(1)
    maxNumOfArgAddArgs = maxNumOfArgAddArgs max res(2)
    maxNumOfArgMulArgs = maxNumOfArgMulArgs max res(3)
    funcs = funcs :+ List(f1, f2)
    funcsCoefficientPairs = funcsCoefficientPairs :+ Map(f1.a -> f2.a,
      f1.b -> f2.b, f1.c -> f2.c,f1.d -> f2.d,f1.e -> f2.e,f1.f -> f2.f)
  }

//  тоже часть старого парсера
//  private def indexOfCB(line: String): Int = {
//    var index = -1
//    var OBCount = 0
//    var CBCount = 0
//    var CBIndex = 0
//    var isSet = false
//    for (i <- line){
//      index += 1
//      if (i == '('){
//        OBCount +=1
//      } else if (i == ')'){
//        CBCount +=1
//      }
//      if (OBCount == CBCount && !isSet){
//
//        CBIndex = index
//        isSet = !isSet
//      }
//    }
//    CBIndex
//  }
  private def parser(line: String): Ent= {
    if (line.length > 1){
      Ent(line(0).toString, parser(line.slice(1, line.length)), true)
    } else {
      Ent(line, Ent("x", null, false), true)
    }
  }

//  это старый парсер, я написал его когда еще не знал, что все конструкторый одноместны
//  решил оставить ввиде комента, чтобы работа не пропадала
//  private def Parser(line: String): Ent = {
//    if (line.length > 1 && line(1) == '('){
//      val fName = line(0).toString
//      var fVar: Ent = null
//      var infix = ""
//      var i = 2
//      while (i < line.length-1){
//        if (line(i) == '('){
//          val newI = indexOfCB(line.slice(i, line.length))
//          infix += line.slice(i, i+newI+1)
//          i += newI
//        } else if (line(i) == ','){
//          fVar = Parser(infix)
//          infix = ""
//        } else {
//          infix+=line(i)
//        }
//        i+=1
//      }
//      fVar = Parser(infix)
//      Ent(fName,fVar, true)
//    } else if (line.length == 1){
//      val vName = line(0).toString
//      Ent(vName, null, false)
//    } else {
//      throw ParserException()
//    }
//  }

  def makeSMT(): List[String] = {
    var resList: List[String] = List()
    for (i <- 2 to this.maxNumOfArgAddArgs){
      val args = for (j <- 1 to i) yield s"x$j"
      if (i == 2) {
        resList = resList :+ s"(define-fun arcAdd$i ((${args(0)} Int) (${args(1)} Int)) Int (ite (> ${args(0)} ${args(1)}) ${args(0)} ${args(1)}))"
      } else {
        var newDefine = s"(define-fun arcAdd$i ("
        for (arg <- args) {
          newDefine = newDefine + s"($arg Int) "
        }
        newDefine = newDefine.dropRight(1)
        resList = resList :+ newDefine + s") Int (ite (> ${args(0)} (arcAdd${i-1} ${args.slice(1, args.length).mkString(" ")})) ${args(0)}" +
          s" (arcAdd${i-1} ${args.slice(1, args.length).mkString(" ")})))"
      }
    }
    for (i <- 2 to this.maxNumOfArgMulArgs) {
      val args = for (j <- 1 to i) yield s"x$j"
      if (i == 2) {
        resList = resList :+ s"(define-fun arcMul$i ((${args(0)} Int) (${args(1)} Int)) Int (ite (= ${args(0)} (- 10000)) ${args(0)}" +
          s" (ite (= ${args(1)} (- 10000)) ${args(1)} (+ ${args(0)} ${args(1)}))))"
      } else {
        var newDefine = s"(define-fun arcMul$i ("
        for (arg <- args) {
          newDefine = newDefine + s"($arg Int) "
        }
        newDefine = newDefine.dropRight(1)
        resList = resList :+ newDefine + s") Int (ite (= ${args(0)} (- 10000)) ${args(0)}" +
          s" (ite (= (arcMul${i-1} ${args.slice(1, args.length).mkString(" ")}) (- 10000)) (arcMul${i-1} ${args.slice(1, args.length).mkString(" ")})" +
          s" (+ ${args(0)} (arcMul${i-1} ${args.slice(1, args.length).mkString(" ")})))))"
      }
    }
    for {
      i <- 1 to numOfCoefficients
      name <- 'a' to 'f'
    }{
      resList = resList :+ s"(declare-fun $name$i () Int)"
    }
    for (funcPair <- funcsCoefficientPairs){
      for ((firstCoefficient, secondCoefficient) <- funcPair) {
        resList = resList :+ s"(assert (>> $firstCoefficient $secondCoefficient))"
      }
    }
    for {
      i <- 1 to numOfCoefficients
      name <- 'a' to 'f'
    } {
      if (name != 'a' && name != 'e'){
        resList = resList :+ s"(assert (or (>= $name$i 0) (= $name$i (- 10000))))"
      } else {
        resList = resList :+ s"(assert (>= $name$i 0))"
      }
    }
    resList
  }
}


object main extends App{
  private val lines = fromFile("input.txt").getLines()
  private var linesList: List[String] = List()
  for (line <- lines){
    linesList = linesList :+ line
  }
  private val trs = TRS(linesList)
  private var smtStrings = trs.makeSMT()
  private val smtPatternLines = fromFile("SMTPattern.txt").getLines().toList
  private val indexOfSep =  smtPatternLines.indexWhere(_ == "-")
  smtStrings = smtPatternLines.slice(0, indexOfSep) ++: smtStrings
    :++ smtPatternLines.slice(indexOfSep+1, smtPatternLines.length)
  private val p = PrintWriter(File("input.smt2"))
  smtStrings.foreach(p.println)
  p.close()
}