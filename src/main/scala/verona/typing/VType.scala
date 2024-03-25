package verona.typing

trait Showable[A]:
  extension (a : A) def show: String


/* Verona types */
trait VType

type VName = String
type FName = String

case class VTVar(name : VName) extends VType

case class VTName(name : VName) extends VType

case class VTConj(a : VType, b : VType) extends VType
case class VTDisj(a : VType, b : VType) extends VType

case class VTTrait(f : FName, t : VType) extends VType

case class VTAlias(n : VTName) extends VType
case class VTClass(n : VTName, args : List[VType]) extends VType

case class VTAbs(param : VTName, body : VType) extends VType
case class VTAppl(t : VType, arg : VType) extends VType

case class VTFunc(a : VType, b : VType) extends VType

case class VTImpl(a : VType, b : VType) extends VType
case class VTBox(t : VType) extends VType

case class VTBot() extends VType
case class VTTop() extends VType

case class VTUndef() extends VType

case class VTWith(name : VName, bound : VType, body : VType) extends VType