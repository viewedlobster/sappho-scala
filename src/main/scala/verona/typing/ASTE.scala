//package verona.typing.shit
//
//trait Pass {
//  type Next <: Pass
//  type Prev <: Pass
//}
//
//trait Parsed extends Pass {
//  type Next = Resolved
//  type Prev = Parsed
//}
//trait Resolved extends Pass {
//  type Next = Typed
//  type Prev = Parsed
//}
//trait Typed extends Pass {
//  type Next = Done
//  type Prev = Resolved
//}
//trait Done extends Pass {
//  type Next = Done
//  type Prev = Typed
//}
//
//
//trait ASTE[PassDecoration[X <: Pass], P <: Pass] {
//  type PrevDecoration[Y <: Pass] = Y match {
//    case Parsed => Any
//    case _ => PassDecoration[Y#Prev] & PrevDecoration[Y#Prev]
//  }
//  type CurrentDecoration = PrevDecoration[P] & PassDecoration[P]
//
//  val decoration : CurrentDecoration
//
//  def decorateNext(d : PassDecoration[P#Next]) : ASTE[PassDecoration, P#Next]
//}
//
//type ResolvedName
//
//type ClassDecoration[X <: Pass] = X match {
//  case Parsed => Any
//  case Resolved => { val rname : ResolvedName }
//  case Typed => { val tpe : VType }
//  case Done => Any
//}
//
//
//trait AST[P <: Pass] {
//  type Decoration
//
//  val decor : Decoration
//  var 
//}
//
//trait ASTDef[P <: Pass] extends AST[P] {
//
//}
//
//trait ASTExpr[P <: Pass] extends AST[P]
//
//class ASTClassDef[P <: Pass] extends ASTDef[P] {
//  type Decoration = P match {
//
//  }
//}
//
////trait Resolved[T] {
////  extension (t: T) def rname : ResolvedName
////}
////
////trait Typed[T] {
////  extension (t: T) def tpe : VType
////}



class A
class B
type XTest[X] = X match {
    case A => Int
    case B => Null
  }

class Test[X](v : XTest[X]) {
  var x : XTest[X] = v
}

