package verona.typing

import scala.util.parsing.combinator._
import scala.util.parsing.input.{StreamReader}

class VTypeParser extends RegexParsers {
  def conjFromList: (VType, List[VType]) => VType = {
    case (a, b :: bs) => conjFromList(VTConj(a, b), bs)
    case (a, Nil) => a
  }
  def disjFromList: (VType, List[VType]) => VType = {
    case (a, b :: bs) => disjFromList(VTDisj(a, b), bs)
    case (a, Nil) => a
  }
  def funcFromList: (VType, List[VType]) => VType = {
    case (a, b :: bs) => funcFromList(VTFunc(a, b), bs)
    case (a, Nil) => a
  }
  def implFromList: (VType, List[VType]) => VType = {
    case (a, b :: bs) => implFromList(VTImpl(a, b), bs)
    case (a, Nil) => a
  }


  def vtype: Parser[VType] =  vtimpl

  def vtdiscr: Parser[VType] = paren | vtname | vtbox | vttrait | vtabs

  def paren: Parser[VType] = """\(""".r ~> vtype <~ """\)""".r
  def vtname: Parser[VTName] = vname ^^ { VTName(_) }

  def vtabs: Parser[VTAbs] = """forall""".r ~ vtname ~ """\.""".r ~ vtype ^^ {
    case _ ~ param ~ _ ~ body => VTAbs(param, body)
  }

  def vtappl: Parser[VType] = vtdiscr ~ opt("""\[""".r ~ vtype ~ """\]""".r) ^^ { 
    case a ~ Some(_ ~ b ~ _) => VTAppl(a, b)
    case a ~ None => a
  }
  def vtconj: Parser[VType] = vtappl ~ rep("""&""".r ~> vtconj) ^^ {
    case a ~ Nil => a
    case a ~ (b :: vtypes) => conjFromList(VTConj(a, b), vtypes)
  }
  def vtdisj: Parser[VType] = vtconj ~ rep("""\|""".r ~> vtdisj) ^^ {
    case a ~ Nil => a
    case a ~ (b :: vtypes) => disjFromList(VTDisj(a, b), vtypes)
  }
  def vtfunc: Parser[VType] = vtdisj ~ rep("""->""".r ~> vtfunc) ^^ {
    case a ~ Nil => a
    case a ~ (b :: vtypes) => funcFromList(VTFunc(a, b), vtypes)
  }
  def vtimpl: Parser[VType] = vtfunc ~ rep("""=>""".r ~> vtimpl) ^^ {
    case a ~ Nil => a
    case a ~ (b :: vtypes) => implFromList(VTImpl(a, b), vtypes)
  }

  def vtbox: Parser[VTBox] = """box\(""".r ~> vtype <~ """\)""".r ^^ { VTBox(_) }
  def vttrait: Parser[VType] = """\{""".r ~ vttraitinner
                                        ~ rep(""",""".r ~> vttraitinner)
                                  ~ """\}""".r ^^ {
    case _ ~ t ~ more ~ _ => conjFromList(t, more)
  }
  def vttraitinner: Parser[VTTrait] = fname ~ """:""".r ~ vtype ^^ {
    case fn ~ _ ~ t => VTTrait(fn, t)
  }

  def vname: Parser[VName] = """[a-zA-Z]+""".r ^^ { _.toString }
  def fname: Parser[FName] = """[a-z][a-zA-Z]*""".r ^^ { _.toString }
}

object VTParser extends VTypeParser {
  def vtparse(s: String) = parse(vtype, s)
}

case class VKWType() extends VAST
case class VKWDef() extends VAST
case class VKWClass() extends VAST
case class VKWVar() extends VAST
case class VTokEquals() extends VAST

class VASTParser extends RegexParsers {
  /* Keywords */
  def kwType   = """type""".r
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

  def leftAssocComb[T](comb: (a: VASTTypeExpr, b: T) => VASTTypeExpr,
    z : VASTTypeExpr,
    ts : List[T]) = ts.foldLeft(z)(comb)

  def vname: Parser[VName] = guard(not(kw)) ~> """[a-zA-Z]+""".r ^^ { _.toString }

  def typeParam: Parser[VASTTName] = vname ^^ { VASTTName(_) }
  def typeParams: Parser[List[VASTTName]] = tokLBrac ~ typeParam ~ rep(""",""".r ~> typeParam) ~ tokRBrac ^^ {
    case _ ~ p ~ ps ~ _ => {p :: ps}
  }

  def param: Parser[VASTParam] = vname ~ """:""".r ~ vtexpr ^^ {
    case name ~ _ ~ texpr => VASTParam(name, texpr)
  }
  def params: Parser[List[VASTParam]] = tokLParen ~> opt(param) ~ rep(""",""".r ~> param) <~ tokRParen ^^ {
    case Some(p) ~ ps => p :: ps
    case None ~ ps => ps
  }

  def defType: Parser[VASTDefType] = kwType ~ vname ~ opt(typeParams) ~ opt(whereCond) ~ tokEquals ~ vtexpr ^^ {
    case _ ~ name ~ Some(params) ~ whr ~ _ ~ expr => VASTDefType(name, params, whr, expr)
    case _ ~ name ~ None ~ whr ~ _ ~ expr => VASTDefType(name, Nil, whr, expr)
  }

  def defClass: Parser[VASTDefClass] = kwClass ~ vname ~ opt(typeParams) ~ opt(whereCond) ~ tokEquals ~ vtexpr ^^ {
    case _ ~ name ~ Some(pars) ~ whr ~ _ ~ expr => VASTDefClass(name, pars, whr, expr)
    case _ ~ name ~ None ~ whr ~ _ ~ expr => VASTDefClass(name, Nil, whr, expr)
  }

  def whereCond = """where""".r ~> vtexpr

  def hasType: Parser[VASTTypeExpr] = """:""".r ~> vtexpr
  def defDef: Parser[VASTDefDef] =
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
      VASTDefDef(name, tpars1, pars1, tpe, whr, body)
    }
  }

  def defVar: Parser[VASTDefVar] = kwVar ~> vname ~ hasType ~ opt(tokEquals ~> vexpr) ^^ {
    case name ~ tpe ~ body => VASTDefVar(name, tpe, body)
  }

  def defRaw: Parser[VASTDefRaw] = kwRaw ~> vname ~ hasType ^^ {
    case name ~ tpe => VASTDefRaw(name, tpe)
  }

  def vtexpr: Parser[VASTTypeExpr] = vtast
  def vtast: Parser[VASTTypeExpr] = vastTSub


  def vastTImpl: Parser[VASTTypeExpr] = vastTSub ~ rep(tokImpl ~> vastTImpl) ^^ {
    case a ~ bs => leftAssocComb(VASTTImpl.apply, a, bs)
  }
  def vastTSub: Parser[VASTTypeExpr] = vastTArrow ~ rep(tokSub ~> vastTSub) ^^ {
    case a ~ bs => leftAssocComb(VASTTSub.apply, a, bs)
  }
  def vastTArrow: Parser[VASTTypeExpr] = vastTDisj ~ rep(tokArrow ~> vastTArrow) ^^ {
    case a ~ bs => leftAssocComb(VASTTArrow.apply, a, bs)
  }
  def vastTDisj: Parser[VASTTypeExpr] = vastTConj ~ rep(tokPip ~> vastTDisj) ^^ {
    case a ~ bs => leftAssocComb(VASTTDisj.apply, a, bs)
  }
  def vastTConj: Parser[VASTTypeExpr] = vastTAppl ~ rep(tokAmp ~> vastTConj) ^^ {
    case a ~ bs => leftAssocComb(VASTTConj.apply, a, bs)
  }
  def vastTAppl: Parser[VASTTypeExpr] = vastTDiscr ~ rep(vastTArgs) ^^ {
    case a ~ argss => leftAssocComb(VASTTAppl.apply, a, argss)
  }

  def vastTArgs: Parser[List[VASTTypeExpr]] =
    tokLBrac ~>
    vtexpr ~ rep(tokComma ~> vtexpr)
    <~ tokRBrac ^^ {
      case a ~ bs => a :: bs
    }

  def vastTParen: Parser[VASTTypeExpr] = tokLParen ~> vtexpr <~ tokRParen

  def vastTDiscr: Parser[VASTTypeExpr] = vastTParen | vastTDefBlock | vastTAbs | vastTBox | vastTName

  def vastTName: Parser[VASTTName] = vname ^^ { VASTTName(_) }
  def vastTAbs: Parser[VASTTAbs] = kwForall ~> typeParams ~ tokDot ~ vtexpr ^^ {
    case pars ~ _ ~ tpe => VASTTAbs(pars, tpe)
  }
  def vastTBox: Parser[VASTTBox] = kwBox ~> vtexpr ^^ { VASTTBox(_) }

  def vastTDefBlock: Parser[VASTTypeExpr] = tokLGull ~> rep(vastDef) <~ tokRGull ^^ {
    case defs => VASTTDefBlock(defs)
  }
  def vastDef = defType | defClass | defDef | defVar | defRaw

  def vexpr: Parser[VASTExpr] = ???
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
}