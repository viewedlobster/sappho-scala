// package shit
//
//trait Pass
//
//trait Parsed extends SimplyPass
//trait Resolved extends SimplyPass
//trait Typed extends SimplyPass
//trait TC extends SimplyPass
//
//type ResolvedName
//
//type NameEnv = Map[VName, ResolvedName]
//
//trait SimplyAST[P <: SimplyPass]
//
//trait SimplyDef[P] extends SimplyAST[P] {
//  var _nameEnv : Option[NameEnv]
//  var _rname : Option[ResolvedName]
//  var _tpe: Option[VType]
//}
//
//case class SimplyDefParam[P](name: VName) extends SimplyDef[P]
//case class VASTDefRaw[P](
//  name: VName,
//  tpe: VASTTypeExpr
//) extends VASTDef[P]
//case class VASTDefType[P](
//  name: VName,
//  tparams: List[VASTDefParam],
//  whr: Option[VASTTypeExpr],
//  body: VASTTypeExpr
//  ) extends VASTDef
//case class VASTDefClass(
//  name: VName,
//  tparams: List[VASTDefParam],
//  whr: Option[VASTTypeExpr],
//  body: VASTTypeExpr
//  ) extends VASTDef
//case class VASTDefDef(
//  name: VName,
//  tparams: List[VASTDefParam],
//  params: List[VASTFunParam],
//  ret: VASTTypeExpr,
//  whr: Option[VASTTypeExpr],
//  body : Option[VASTExpr]
//  ) extends VASTDef
//case class VASTDefVar(
//  name: VName,
//  tpe : VASTTypeExpr,
//  body : Option[VASTExpr]
//  ) extends VASTDef
//case class SimplyDefClass[P](
//  
//) extends SimplyDef[P]
//
//case class SimplyDefTypeName[P](
//
//) extends SimplyDef[P]