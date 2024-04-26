package verona.typing

type VName = String

trait Pass

trait Parsed extends Pass
trait Resolved extends Pass
trait Typed extends Pass
trait TC extends Pass

type PrevPass[P <: Pass] = P match {
  case Resolved => Parsed
}
type NextPass[P <: Pass] = P match {
  case Parsed => Resolved
}

//type ResolvedName
//
//type NameEnv = Map[VName, ResolvedName]
//
//trait VAST[P <: SimplyPass]
//
//trait VASTDef[P] extends VAST[P] {
//  var _nameEnv : Option[NameEnv]
//  var _rname : Option[ResolvedName]
//  var _tpe: Option[VType]
//}

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
  type FromPasses[PI[_], P] = P match {
    case Parsed => PI[P]
    case _ => PI[P] & FromPasses[PI, PrevPass[P]]
  }

  type _ResolvEnv[P <: Pass] = P match {
    case Parsed => {}
    case Resolved => { val renv : NameEnv }
  }
  type ResolvEnv[P] = FromPasses[_ResolvEnv, P]

  type _Def[P <: Pass] = P match {
    case Parsed => {}
    case Resolved => { val renv : NameEnv }
  }
  type Def[P <: Pass] = FromPasses[_Def, P]

  type _TypeExpr[P <: Pass] = P match {
    case Parsed => {}
    case Resolved => { val renv : NameEnv }
  }
  type TypeExpr[P <: Pass] = FromPasses[_TypeExpr, P]

  type _Name[P <: Pass] = P match {
    case Parsed => {}
    case Resolved => { val rname : VResolvedName }
  }
  type TName[P <: Pass] = TypeExpr[P] & FromPasses[_Name, P]

  type TBlock[P <: Pass] = TypeExpr[P]
  type TFile[P <: Pass] = TypeExpr[P]
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

class Info[+T](val fields: Map[String, Any]) extends Selectable {
  def selectDynamic(name: String): Any = fields(name)
  def this(elems : (String, Any)*) = {
    this(elems.toMap)
  }
  def combine[S](other : Info[S]): Info[S & T] = {
    Info[S & T](fields ++ other.fields)
  }

  def ++ = combine
}

class INameEnv(env : NameEnv) extends Info[{val env : NameEnv}]("env" -> env)
class IEmpty() extends Info[{}]()

sealed trait VAST[P <: Pass, PI[_ <: Pass]](using id : NodeId) {
  type PassI[_ <: Pass] = PI[P]
  val info : Info[PassI[P]]
}


trait VASTTypeExpr[P <: Pass, PI[_]](using id : NodeId) extends VAST[P, PI]

trait VASTExpr[P <: Pass, PI[_]](using id : NodeId) extends VAST[P, PI]

case class VASTTName[P <: Pass](
  val name : VName,
  val info: Info[PassInfo#TName[P]]
  )(using id : NodeId) extends VASTTypeExpr[P, PassInfo#TName]

case class VASTTDefBlock[P <: Pass](
  val inner: List[VASTDef[P]],
  val info : Info[PassInfo#TBlock[P]]
  )(using id : NodeId) extends VASTTypeExpr[P, PassInfo#TBlock]
case class VASTTDefFile[P <: Pass](
  val inner: List[VASTDef[P]],
  val info : Info[PassInfo#TFile[P]]
  )(using id : NodeId) extends VASTTypeExpr[P, PassInfo#TFile]


trait VASTTBinOp[P <: Pass] extends VASTTypeExpr[P, PassInfo#TBinOp]
case class VASTTConj[P <: Pass](
  val a: VASTTypeExpr[P, ?],
  val b: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#TBinOp[P]]
  )(using id : NodeId) extends VASTTBinOp[P]
case class VASTTDisj[P <: Pass](
  val a: VASTTypeExpr[P, ?],
  val b: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#TBinOp[P]]
  )(using id : NodeId) extends VASTTBinOp[P]
case class VASTTSub[P <: Pass](
  val a: VASTTypeExpr[P, ?],
  val b: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#TBinOp[P]]
  )(using id : NodeId) extends VASTTBinOp[P]
case class VASTTImpl[P <: Pass](
  val a: VASTTypeExpr[P, ?],
  val b: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#TBinOp[P]]
  )(using id : NodeId) extends VASTTBinOp[P]

case class VASTTAppl[P <: Pass](
  val hole: VASTTypeExpr[P, ?],
  val args: List[VASTTypeExpr[P, ?]],
  val info: Info[PassInfo#TAppl[P]]
  )(using id : NodeId) extends VASTTypeExpr[P, PassInfo#TAppl]
case class VASTTAbs[P <: Pass](
  val params: List[VASTDefParam[P]],
  val body: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#TAbs[P]])(using id : NodeId) extends VASTTypeExpr[P, PassInfo#TAbs]

case class VASTTBox[P <: Pass](
  val a: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#TBox[P]]
  )(using id : NodeId) extends VASTTypeExpr[P, PassInfo#TBox]

case class VASTTArrow[P <: Pass](
  val a: List[VASTFunParam[P]],
  val b: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#TArrow[P]]
  )(using id : NodeId) extends VASTTypeExpr[P, PassInfo#TArrow]

case class VASTFunParam[P <: Pass](
  val name: VName,
  val tpe: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#FunParam[P]]
  ) extends VAST[P, PassInfo#FunParam]

sealed trait VASTDef[P <: Pass](using id : NodeId) extends VAST[P, PassInfo#Def]
case class VASTDefParam[P <: Pass](name: VName)(using id : NodeId) extends VASTDef[P]
case class VASTDefRaw[P <: Pass](
  val name: VName,
  val tpe: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#Def[P]]
)(using id : NodeId) extends VASTDef[P]
case class VASTDefType[P <: Pass](
  val name: VName,
  val tparams: List[VASTDefParam[P]],
  val whr: Option[VASTTypeExpr[P, ?]],
  val body: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#Def[P]]
  )(using id : NodeId) extends VASTDef[P]
case class VASTDefClass[P <: Pass](
  val name: VName,
  val tparams: List[VASTDefParam[P]],
  val whr: Option[VASTTypeExpr[P, ?]],
  val body: VASTTypeExpr[P, ?],
  val info: Info[PassInfo#Def[P]]
  )(using id : NodeId) extends VASTDef[P]
case class VASTDefDef[P <: Pass](
  val name: VName,
  val tparams: List[VASTDefParam[P]],
  val params: List[VASTFunParam[P]],
  val ret: VASTTypeExpr[P, ?],
  val whr: Option[VASTTypeExpr[P, ?]],
  val body : Option[VASTExpr[P, ?]],
  val info: Info[PassInfo#Def[P]]
  )(using id : NodeId) extends VASTDef[P]
case class VASTDefVar[P <: Pass](
  val name: VName,
  val tpe : VASTTypeExpr[P, ?],
  val body : Option[VASTExpr[P, ?]],
  val info: Info[PassInfo#Def[P]]
  )(using id : NodeId) extends VASTDef[P]


trait Decorator[T, S, V] {
  extension (t : T) def decorate(v : V) : S
}

given Decorator[VASTDefClass[Parsed], VASTDefClass[Resolved], NameEnv] with {
  extension (cd : VASTDefClass[Parsed])
    def decorate(env: NameEnv) : VASTDefClass[Resolved] = ???
}

given Decorator[VASTDefParam[Parsed], VASTDefParam[Resolved], NameEnv] with {
  extension (t: VASTDefParam[Parsed]) def decorate(v: NameEnv): VASTDefParam[Resolved] = ???
}

given Decorator[List[VASTDefParam[Parsed]], List[VASTDefParam[Resolved]], NameEnv] with {
  extension (ds: List[VASTDefParam[Parsed]]) def decorate(env: NameEnv): List[VASTDefParam[Resolved]] = 
    ds.map(_.decorate(env))
}

//object NameEnvDecorator[K, V] extends Decorator[VAST[Parsed], VAST[Resolved], NameEnv] {
//  def decorate(ast: VAST[Parsed]) : VAST[Resoved] = {
//    val env = Map[VName, VASTTResolvedName]()
//
//    decorateRec()
//  }
//
//  def decorateRec(ast: VAST[Parsed], env : NameEnv) : VAST[Resolved] = {
//
//  }
//}


