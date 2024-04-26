package verona.typing

//trait CompilerPass[S, T] {
//  def apply(in: S): Option[T]
//  def andThen[U](cp: CompilerPass[T, U]): CompilerPass[S, U] = {
//    def self = this
//    class Anon extends CompilerPass[S, U] {
//      def apply(in: S) : U = {
//        val ret = self.apply(in)
//        cp.apply(ret)
//      }
//    }
//    Anon()
//  }
//}

trait PassResult[T] {
  def isSucc : Boolean
  def andThen[U](f : T => PassResult[U]): PassResult[U]
}

class PassFail[T](val msg: String) extends PassResult[T] {
  def isSucc = false
  def andThen[U](f : T => PassResult[U]): PassResult[U] = PassFail[U](msg)
}

class PassSucc[T](val res : T) extends PassResult[T] {
  def isSucc = true
  def andThen[U](f : T => PassResult[U]): PassResult[U] = f(res)
}

//class FQN extends CompilerPass[VAST, VAST] {
//  def apply(in: VAST) : VAST = ???
//
//  def fqnResolve(in: VAST, varmap: Map[VName, VName]): PassResult[VAST]
//}