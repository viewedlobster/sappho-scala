package verona.typing

trait Showable[A]:
  extension (a : A) def show: String


/* Verona types */
trait VType

case class VTConj(a : VType, b : VType) extends VType
case class VTDisj(a : VType, b : VType) extends VType

case class VTTrait(f : VName, t : VType) extends VType

case class VTParam(name : VName) extends VType
case class VTClass(name: VName) extends VType
case class VTAlias(name: VName) extends VType

case class VTAbs(param : List[VTParam], body : VType) extends VType
case class VTAppl(t : VType, arg : List[VType]) extends VType

case class VTFunc(a : List[VType], b : VType) extends VType

case class VTImpl(a : VType, b : VType) extends VType
case class VTBox(t : VType) extends VType

case class VTBot() extends VType
case class VTTop() extends VType

case class VTUndef() extends VType

case class VTWith(name : VName, bound : VType, body : VType) extends VType