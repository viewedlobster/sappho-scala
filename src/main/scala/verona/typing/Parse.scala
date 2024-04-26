package verona.typing

import scala.util.parsing.combinator._
import scala.util.parsing.input.{StreamReader}

class VASTParser extends RegexParsers {
  /* Keywords */ def kwType   = """type""".r
  def kwDef    = """def""".r
  def kwClass  = """class""".r
  def kwVar    = """var""".r
  def kwRaw    = """raw""".r
  def kwForall = """forall""".r
  def kwBox    = """box""".r

  def kw = kwType | kwDef | kwClass | kwVar | kwRaw | kwForall | kwBox

  def tokEquals = """=""".r
  def tokSub    = """<:""".r
  def tokAmp    = """&""".r
  def tokPip    = """\|""".r
  def tokLGull  = """\{""".r
  def tokRGull  = """\}""".r
  def tokArrow  = """->""".r
  def tokImpl   = """=>""".r
  def tokLBrac  = """\[""".r
  def tokRBrac  = """\]""".r
  def tokComma  = """,""".r
  def tokDot    = """.""".r
  def tokLParen = """\(""".r
  def tokRParen = """\)""".r

  def leftAssocComb[T, P <: Pass](
    comb: (a: VASTTypeExpr[P], b: T) => VASTTypeExpr[P],
    z : VASTTypeExpr[P],
    ts : List[T]) =
      ts.foldLeft(z)(comb)

  def vname: Parser[VName] = guard(not(kw)) ~> """[a-zA-Z]+""".r ^^ { _.toString }

  def typeParam: Parser[VASTDefParam[Parsed]] = vname ^^ { VASTDefParam(_, IEmpty()) }
  def typeParams: Parser[List[VASTDefParam[Parsed]]] = tokLBrac ~ typeParam ~ rep(""",""".r ~> typeParam) ~ tokRBrac ^^ {
    case _ ~ p ~ ps ~ _ => {p :: ps}
  }

  def param: Parser[VASTFunParam[Parsed]] = vname ~ """:""".r ~ vtexpr ^^ {
    case name ~ _ ~ texpr => VASTFunParam(name, texpr, IEmpty())
  }
  def params: Parser[List[VASTFunParam[Parsed]]] = tokLParen ~> opt(param) ~ rep(""",""".r ~> param) <~ tokRParen ^^ {
    case Some(p) ~ ps => p :: ps
    case None ~ ps => ps
  }

  def defType: Parser[VASTDefType[Parsed]] = kwType ~ vname ~ opt(typeParams) ~ opt(whereCond) ~ tokEquals ~ vtexpr ^^ {
    case _ ~ name ~ Some(params) ~ whr ~ _ ~ expr => VASTDefType(name, params, whr, expr, IEmpty())
    case _ ~ name ~ None ~ whr ~ _ ~ expr => VASTDefType(name, Nil, whr, expr, IEmpty())
  }

  def defClass: Parser[VASTDefClass[Parsed]] = kwClass ~ vname ~ opt(typeParams) ~ opt(whereCond) ~ tokEquals ~ vtexpr ^^ {
    case _ ~ name ~ Some(pars) ~ whr ~ _ ~ expr => VASTDefClass(name, pars, whr, expr, IEmpty())
    case _ ~ name ~ None ~ whr ~ _ ~ expr => VASTDefClass(name, Nil, whr, expr, IEmpty())
  }

  def whereCond = """where""".r ~> vtexpr

  def hasType: Parser[VASTTypeExpr[Parsed]] = """:""".r ~> vtexpr
  def defDef: Parser[VASTDefDef[Parsed]] =
    kwDef ~ vname ~ opt(typeParams) ~ opt(params) ~ hasType ~
    opt(whereCond) ~ opt(tokEquals ~> vexpr) ^^ {
    case _ ~ name ~ tpars ~ pars ~ tpe ~ whr ~ body => {
      val tpars1 = tpars match {
        case Some(tpars1) => tpars1
        case _ => Nil
      }
      val pars1 = pars match {
        case Some(pars1) => pars1
        case _ => Nil
      }
      VASTDefDef(name, tpars1, pars1, tpe, whr, body, IEmpty())
    }
  }

  def defVar: Parser[VASTDefVar[Parsed]] = kwVar ~> vname ~ hasType ~ opt(tokEquals ~> vexpr) ^^ {
    case name ~ tpe ~ body => VASTDefVar(name, tpe, body, IEmpty())
  }

  def defRaw: Parser[VASTDefRaw[Parsed]] = kwRaw ~> vname ~ hasType ^^ {
    case name ~ tpe => VASTDefRaw(name, tpe, IEmpty())
  }

  def vtexpr: Parser[VASTTypeExpr[Parsed]] = vtast
  def vtast: Parser[VASTTypeExpr[Parsed]] = vastTImpl


  def vastTArrow: Parser[VASTTypeExpr[Parsed]] = params ~ tokArrow ~ vtexpr ^^ {
    case params ~ _ ~ texpr => VASTTArrow(params, texpr, IEmpty())
  }

  def vastTImpl: Parser[VASTTypeExpr[Parsed]] = vastTSub ~ rep(tokImpl ~> vastTImpl) ^^ {
    case a ~ bs => leftAssocComb(VASTTImpl[Parsed](_, _, IEmpty()), a, bs)
  }
  def vastTSub: Parser[VASTTypeExpr[Parsed]] = vastTDisj ~ rep(tokSub ~> vastTSub) ^^ {
    case a ~ bs => leftAssocComb(VASTTSub[Parsed](_, _, IEmpty()), a, bs)
  }
  def vastTDisj: Parser[VASTTypeExpr[Parsed]] = vastTConj ~ rep(tokPip ~> vastTDisj) ^^ {
    case a ~ bs => leftAssocComb(VASTTDisj[Parsed](_, _, IEmpty()), a, bs)
  }
  def vastTConj: Parser[VASTTypeExpr[Parsed]] = vastTAppl ~ rep(tokAmp ~> vastTConj) ^^ {
    case a ~ bs => leftAssocComb(VASTTConj[Parsed](_, _, IEmpty()), a, bs)
  }
  def vastTAppl: Parser[VASTTypeExpr[Parsed]] = vastTDiscr ~ rep(vastTArgs) ^^ {
    case a ~ argss => leftAssocComb(VASTTAppl[Parsed](_, _, IEmpty()), a, argss)
  }

  def vastTArgs: Parser[List[VASTTypeExpr[Parsed]]] =
    tokLBrac ~>
    vtexpr ~ rep(tokComma ~> vtexpr)
    <~ tokRBrac ^^ {
      case a ~ bs => a :: bs
    }

  def vastTParen: Parser[VASTTypeExpr[Parsed]] = tokLParen ~> vtexpr <~ tokRParen

  def vastTDiscr: Parser[VASTTypeExpr[Parsed]] = vastTArrow | vastTParen | vastTDefBlock | vastTAbs | vastTBox | vastTParsedName

  def vastTParsedName: Parser[VASTTName[Parsed]] = vname ^^ { VASTTName(_, IEmpty()) }
  def vastTAbs: Parser[VASTTAbs[Parsed]] = kwForall ~> typeParams ~ tokDot ~ vtexpr ^^ {
    case pars ~ _ ~ tpe => VASTTAbs(pars, tpe, IEmpty())
  }
  def vastTBox: Parser[VASTTBox[Parsed]] = kwBox ~> vtexpr ^^ { VASTTBox(_, IEmpty()) }

  def vastTDefBlock: Parser[VASTTypeExpr[Parsed]] = tokLGull ~> rep(vastDef) <~ tokRGull ^^ {
    case defs => VASTTDefBlock(defs, IEmpty())
  }
  def vastDef = defType | defClass | defDef | defVar | defRaw

  def vastTDefFile(filePath : String): Parser[VASTTypeExpr[Parsed]] = rep(vastDef) ^^ { VASTTDefFile(_, IFile(filePath)) }

  def vastClassFile(filePath: String, cname: VName) = vastTDefFile(filePath) ^^ { VASTDefClass(cname, Nil, None, _, IEmpty()) }

  def vexpr: Parser[VASTExpr[Parsed]] = ???
}

object VASTParser extends VASTParser {
  def vastDefParsePrint(in: StreamReader) : Unit = {
    val res = parse(vastDef, in)
    var inn = res.next
    println(res)

    while (!inn.atEnd)
      val res = parse(vastDef, inn)
      inn = res.next
      println(res)
      if (!res.successful) {
        return ()
      }
  }

  def vastParseFile(cname: VName, in: StreamReader) : Option[VASTDefClass[Parsed]] = {
    parse(vastClassFile(cname, cname), in) match {
      case Success(result, _) => Some(result)
      case Error(msg, next) => println(msg); None
      case Failure(msg, next) => println(msg); None
    }
  }
}