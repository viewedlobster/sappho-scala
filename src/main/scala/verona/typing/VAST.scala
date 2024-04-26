package verona.typing

type VName = String

trait Pass

class BeforeAll extends Pass
class Parsed extends Pass
class Resolved extends Pass
class Typed extends Pass
class TC extends Pass

type PrevPass[P <: Pass] = P match {
  case Parsed => BeforeAll
  case Resolved => Parsed
}
type NextPass[P <: Pass] = P match {
  case Parsed => Resolved
}

sealed trait TypeTag
object TopTag extends TypeTag
object BotTag extends TypeTag
object UndefTag extends TypeTag

sealed trait NodeId
case class INodeId(id : Int) extends NodeId
object TopId extends NodeId
object BotId extends NodeId
object UndefId extends NodeId
object InvalidId extends NodeId

given NodeId = InvalidId

type NameEnv = Map[VName, VResolvedName]

trait PassInfo {
  type FromPasses[PI[_ <: Pass], P] = P match {
    case BeforeAll => Info
    case _ => PI[P] & FromPasses[PI, PrevPass[P]]
  }

  type _ResolvEnv[P <: Pass] = P match {
    case Parsed => Info
    case Resolved => Info { val renv : NameEnv }
  }
  type ResolvEnv[P <: Pass] = FromPasses[_ResolvEnv, P]

  type Def[P <: Pass] = ResolvEnv[P]

  type TypeExpr[P <: Pass] = ResolvEnv[P]

  type _Name[P <: Pass] = P match {
    case Parsed => Info
    case Resolved => Info { val rname : VResolvedName }
  }
  type TName[P <: Pass] = TypeExpr[P] & FromPasses[_Name, P]

  type _File[P <: Pass] = P match {
    case Parsed => Info { val filePath : String }
    case _ => Info
  }

  type TBlock[P <: Pass] = TypeExpr[P]
  type TFile[P <: Pass] = TypeExpr[P] & FromPasses[_File, P]
  type TBinOp[P <: Pass] = TypeExpr[P]
  type TAppl[P <: Pass] = TypeExpr[P]
  type TAbs[P <: Pass] = TypeExpr[P]
  type TBox[P <: Pass] = TypeExpr[P]
  type TArrow[P <: Pass] = TypeExpr[P]
  type FunParam[P <: Pass] = ResolvEnv[P]
}

trait VResolvedName(using id : NodeId)
case class VClass(name : VName)(using id : NodeId) extends VResolvedName
case class VAlias(name : VName)(using id : NodeId) extends VResolvedName
case class VParam(name : VName)(using id : NodeId)
object VTop extends VResolvedName(using TopId)
object VBot extends VResolvedName(using BotId)
object VUndef extends VResolvedName(using UndefId)


class Info(val fields : Map[String, Any]) extends Selectable {
  def selectDynamic(name: String): Any = fields(name)
  def this(elems : (String, Any)*) = {
    this(elems.toMap)
  }
}


object Info {
  def combine[A <: Info, B <: Info](a : A, b : B) : A & B = Info(a.fields ++ b.fields).asInstanceOf[A & B]
}

object IEmpty {
  type IEmpty = Info

  def apply() : IEmpty = Info()
}

object IFile {
  type IFile = Info { val filePath : String }

  def apply(fp : String) : IFile = Info("filePath" -> fp).asInstanceOf[IFile]
}

object INameEnv {
  type INameEnv = Info { val renv : NameEnv }

  def apply(e : NameEnv) : INameEnv = Info("nameEnv" -> e).asInstanceOf[INameEnv]
}
object IDud {
  type IDud = Info { val dudval : String }

  def apply() : IDud = Info("dudval" -> null).asInstanceOf[IDud]
}

val a = IFile("hello")
val b = INameEnv(Map("hhe" -> VTop))

val c = Info.combine(a, b)
val d = Info.combine(c, IDud())


val path = c.filePath
val dud = d.dudval


sealed trait VAST[P <: Pass](using id : NodeId) {
  type PassI[_ <: Pass]
  val info : PassI[P]
}


trait VASTTypeExpr[P <: Pass](using id : NodeId) extends VAST[P]

trait VASTExpr[P <: Pass](using id : NodeId) extends VAST[P]

case class VASTTName[P <: Pass](
  val name : VName,
  val info: PassInfo#TName[P]
  )(using id : NodeId) extends VASTTypeExpr[P] {
    type PassI[P <: Pass] = PassInfo#TName[P]
  }

case class VASTTDefBlock[P <: Pass](
  val inner: List[VASTDef[P]],
  val info : PassInfo#TBlock[P]
  )(using id : NodeId) extends VASTTypeExpr[P] {
    type PassI[P <: Pass] = PassInfo#TBlock[P]
  }
case class VASTTDefFile[P <: Pass](
  val inner: List[VASTDef[P]],
  val info : PassInfo#TFile[P]
  )(using id : NodeId) extends VASTTypeExpr[P] {
    type PassI[P <: Pass] = PassInfo#TFile[P]
  }


trait VASTTBinOp[P <: Pass] extends VASTTypeExpr[P] {
  type PassI[P <: Pass] = PassInfo#TBinOp[P]
}
case class VASTTConj[P <: Pass](
  val a: VASTTypeExpr[P],
  val b: VASTTypeExpr[P],
  val info: PassInfo#TBinOp[P]
  )(using id : NodeId) extends VASTTBinOp[P]
case class VASTTDisj[P <: Pass](
  val a: VASTTypeExpr[P],
  val b: VASTTypeExpr[P],
  val info: PassInfo#TBinOp[P]
  )(using id : NodeId) extends VASTTBinOp[P]
case class VASTTSub[P <: Pass](
  val a: VASTTypeExpr[P],
  val b: VASTTypeExpr[P],
  val info: PassInfo#TBinOp[P]
  )(using id : NodeId) extends VASTTBinOp[P]
case class VASTTImpl[P <: Pass](
  val a: VASTTypeExpr[P],
  val b: VASTTypeExpr[P],
  val info: PassInfo#TBinOp[P]
  )(using id : NodeId) extends VASTTBinOp[P]

case class VASTTAppl[P <: Pass](
  val hole: VASTTypeExpr[P],
  val args: List[VASTTypeExpr[P]],
  val info: PassInfo#TAppl[P]
  )(using id : NodeId) extends VASTTypeExpr[P] {
    type PassI[P <: Pass] = PassInfo#TAppl[P]
  }
case class VASTTAbs[P <: Pass](
  val params: List[VASTDefParam[P]],
  val body: VASTTypeExpr[P],
  val info: PassInfo#TAbs[P])(using id : NodeId) extends VASTTypeExpr[P] {
    type PassI[P <: Pass] = PassInfo#TAbs[P]
  }

case class VASTTBox[P <: Pass](
  val a: VASTTypeExpr[P],
  val info: PassInfo#TBox[P]
  )(using id : NodeId) extends VASTTypeExpr[P] {
    type PassI[P <: Pass] = PassInfo#TBox[P]
  }

case class VASTTArrow[P <: Pass](
  val a: List[VASTFunParam[P]],
  val b: VASTTypeExpr[P],
  val info: PassInfo#TArrow[P]
  )(using id : NodeId) extends VASTTypeExpr[P] {
    type PassI[P <: Pass] = PassInfo#TArrow[P]
  }

case class VASTFunParam[P <: Pass](
  val name: VName,
  val tpe: VASTTypeExpr[P],
  val info: PassInfo#FunParam[P]
  ) extends VAST[P] {
    type PassI[P <: Pass] = PassInfo#FunParam[P]
  }

sealed trait VASTDef[P <: Pass](using id : NodeId) extends VAST[P] {
  type PassI[P <: Pass] = PassInfo#Def[P]
}
case class VASTDefParam[P <: Pass](
  val name: VName,
  val info: PassInfo#Def[P])(using id : NodeId) extends VASTDef[P]
case class VASTDefRaw[P <: Pass](
  val name: VName,
  val tpe: VASTTypeExpr[P],
  val info: PassInfo#Def[P]
)(using id : NodeId) extends VASTDef[P]
case class VASTDefType[P <: Pass](
  val name: VName,
  val tparams: List[VASTDefParam[P]],
  val whr: Option[VASTTypeExpr[P]],
  val body: VASTTypeExpr[P],
  val info: PassInfo#Def[P]
  )(using id : NodeId) extends VASTDef[P]
case class VASTDefClass[P <: Pass](
  val name: VName,
  val tparams: List[VASTDefParam[P]],
  val whr: Option[VASTTypeExpr[P]],
  val body: VASTTypeExpr[P],
  val info: PassInfo#Def[P]
  )(using id : NodeId) extends VASTDef[P]
case class VASTDefDef[P <: Pass](
  val name: VName,
  val tparams: List[VASTDefParam[P]],
  val params: List[VASTFunParam[P]],
  val ret: VASTTypeExpr[P],
  val whr: Option[VASTTypeExpr[P]],
  val body : Option[VASTExpr[P]],
  val info: PassInfo#Def[P]
  )(using id : NodeId) extends VASTDef[P]
case class VASTDefVar[P <: Pass](
  val name: VName,
  val tpe : VASTTypeExpr[P],
  val body : Option[VASTExpr[P]],
  val info: PassInfo#Def[P]
  )(using id : NodeId) extends VASTDef[P]


case class VASTTDud[P <: Pass]() extends VASTTypeExpr[P] {
  type PassI[P <: Pass] = Info
  val info = IEmpty()
}

trait ASTPass[P1, T, P2] {
  extension (s : P1) def passWith(t : T): P2
}

given ASTPass[VASTTypeExpr[Parsed], NameEnv, VASTTypeExpr[Resolved]] with {
  extension (texp : VASTTypeExpr[Parsed]) def passWith(env: NameEnv): VASTTypeExpr[Resolved] = {
    texp match {
      case VASTTDefFile(inner, info) => println(info.filePath); VASTTDud()
      case _ => VASTTDud()
    }
  }
}

given ASTPass[VASTDef[Parsed], NameEnv, VASTDef[Resolved]] with {
  extension (d : VASTDef[Parsed]) def passWith(env: NameEnv): VASTDef[Resolved] = {
    d match {
      case VASTDefClass(name, tparams, whr, body, info) => VASTDefClass(name, Nil, whr.map(_.passWith(env)), body.passWith(env), Info.combine(info, INameEnv(env)))
    }
  }
}

