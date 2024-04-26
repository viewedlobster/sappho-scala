import verona.typing._
import scala.util.parsing.input.{StreamReader, Reader}

import java.io._

class RunConf(val inFile: String)

@main def run(inFile: String): Unit =
  // println(VTParser.vtparse("X & Y"))
  // println(VTParser.vtparse("X | Y"))
  // println(VTParser.vtparse("A & B & C | D"))
  // println(VTParser.vtparse("A | B | C & D"))
  // println(VTParser.vtparse("box(A & B)"))
  // println(VTParser.vtparse("{f : T}"))
  // println(VTParser.vtparse("A => B & C | D"))
  // println(VTParser.vtparse("A -> B"))
  // println(VTParser.vtparse("forall A . T"))
  // println(VTParser.vtparse("(forall A . A) [A]"))
  // println(VTParser.vtparse("forall A . A [A]"))
  // println(VTParser.vtparse("forall A. B => A -> A"))

  def in = StreamReader(new InputStreamReader(new FileInputStream(inFile)))
  VASTParser.vastParseFile("file:" ++ inFile, in) match {
    case Some(ast) => {
      println(ast)
      import verona.typing.given_ASTPass_VASTDef_NameEnv_VASTDef
      ast.passWith(Map())
    }
    case _ => println("stuff failed")
  }

  