package verona.typing

trait VAST

trait VASTExpr extends VAST

trait VASTTypeExpr extends VAST

case class VASTTName(name: VName) extends VASTTypeExpr
case class VASTTDefBlock(inner: List[VASTDef]) extends VASTTypeExpr

case class VASTTConj(a: VASTTypeExpr, b: VASTTypeExpr) extends VASTTypeExpr
case class VASTTDisj(a: VASTTypeExpr, b: VASTTypeExpr) extends VASTTypeExpr
case class VASTTSub(a: VASTTypeExpr, b: VASTTypeExpr) extends VASTTypeExpr

case class VASTTAppl(a: VASTTypeExpr, args: List[VASTTypeExpr]) extends VASTTypeExpr
case class VASTTAbs(param: List[VASTTName], body: VASTTypeExpr) extends VASTTypeExpr

case class VASTTBox(a: VASTTypeExpr) extends VASTTypeExpr
case class VASTTImpl(a: VASTTypeExpr, b: VASTTypeExpr) extends VASTTypeExpr

case class VASTTArrow(a: VASTTypeExpr, b: VASTTypeExpr) extends VASTTypeExpr

object VASTTTop extends VASTTypeExpr
object VASTTBot extends VASTTypeExpr
object VASTTUndef extends VASTTypeExpr

case class VASTParam(name: VName, tpe: VASTTypeExpr) extends VAST

trait VASTDef extends VAST
case class VASTDefRaw(
  name: VName,
  tpe: VASTTypeExpr
) extends VASTDef
case class VASTDefType(
  name: VName,
  tparams: List[VASTTName],
  whr: Option[VASTTypeExpr],
  body: VASTTypeExpr
  ) extends VASTDef
case class VASTDefClass(
  name: VName,
  tparams: List[VASTTName],
  whr: Option[VASTTypeExpr],
  body: VASTTypeExpr
  ) extends VASTDef
case class VASTDefDef(
  name: VName,
  tparams: List[VASTTName],
  params: List[VASTParam],
  ret: VASTTypeExpr,
  whr: Option[VASTTypeExpr],
  body : Option[VASTExpr]
  ) extends VASTDef
case class VASTDefVar(
  name: VName,
  tpe : VASTTypeExpr,
  body : Option[VASTExpr]
  ) extends VASTDef



