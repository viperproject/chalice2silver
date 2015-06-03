/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.chalice2sil

import java.nio.file.Path
import viper.silver.verifier._
import viper.silver.frontend._
import viper.silver.ast.{SourcePosition, NoPosition, Position}
import translation._
import viper.chalice2sil.messages.ReportMessage

class Chalice2SILFrontEnd(var verf: Verifier  = null) extends DefaultPhases {
  type ChaliceProgram = List[chalice.TopLevelDecl]
  var file: Path = null
  var chaliceAST: List[chalice.TopLevelDecl] = null
  var silAST: viper.silver.ast.Program = null
  var failed: Seq[AbstractError] = Seq()
  var messages = Seq[ReportMessage]()
  var verifierResult: VerificationResult = null

  def TopAnnotationPosition = SourcePosition(file, 2, 1)
    // position 2, 1 helps with the error annotations.  the annotation appears in the first line

  override def init(verifier: Verifier) {
    verf = verifier
  }

  override def reset(files: Seq[Path]) {
    chaliceAST = null
    silAST = null
    file = files(0)
    failed = Seq()
  }

  protected def parseChaliceProgram(): Either[ChaliceProgram, AbstractError] = {
    val parser = new chalice.Parser

    parser.parseFile(file.toFile) match {
      case parser.Success(p, _) => Left(p.asInstanceOf[ChaliceProgram])
      case parser.Failure(msg, next) => {
        val position =  SourcePosition(file, next.pos.line, next.pos.column)
        Right(ParseError(msg.toString, position))
      }
      case _ => Right(ParseError("Chalice program contained syntax errors.", TopAnnotationPosition))
    }
  }

  override def parse() = {
    try {
     parseChaliceProgram() match {
        case Left(program) =>
          chaliceAST = program

        case Right(error) =>
          val failures = Seq(error)
          failed ++= failures
      }
    } catch {
      case e: Throwable =>
        val f = Seq(ParseError(e.toString, TopAnnotationPosition))
        failed ++= f
    }
  }

  override def typecheck() = {
    try {
      if(failed.isEmpty) {
        chalice.Resolver.Resolve(chaliceAST) match {
          case chalice.Resolver.Success() => Success
          case chalice.Resolver.Errors(ss) => {
            val (position, message) = ss.head
            val sourcePosition = if (position.line > 0) {
              SourcePosition(file, position.line, position.column)
            } else {
              // Exact position unknown, return top.
              TopAnnotationPosition
            }
            val f = Seq(TypecheckerError(message, sourcePosition))
            failed ++= f
          }
          case _ => {
            val f = Seq(TypecheckerError("Chalice program contained resolution errors.", TopAnnotationPosition))
            failed ++= f
          }
        }
      }
      else Success
    } catch {
        case e: Throwable =>
          val f = Seq(TypecheckerError(e.toString, TopAnnotationPosition))
          failed ++= f
    }
  }

  override def translate() = {
    try {
      if (failed.isEmpty) {
        val (s: viper.silver.ast.Program, messages) = new ProgramTranslator(file.toString).translate(chaliceAST)
        silAST = s
        this.messages = messages
        failed ++= messages.filter(_.fatal).map(_.translationError)
      }
    } catch {
        case e: Throwable =>
          val f = Seq(TranslationError(e.toString, TopAnnotationPosition))
          failed ++= f
    }
  }

  override def verify() = {
    try {
        if (failed.isEmpty && verf != null) {
          verifierResult = verf.verify(silAST)
          verifierResult match {
            case Failure(f) => failed ++= f
            case Success =>
          }
        }
    } catch {
        case e: Throwable =>
          val f = Seq(VerifierThrowsException(e.getStackTrace.mkString("\n"), TopAnnotationPosition))
          failed ++= f
    }
  }

  override def result = if (!failed.isEmpty) Failure(failed) else Success
}

case class TranslationError(message: String, position: Position) extends AbstractError {
  def fullId = "chalice2sil.error"
  def readableMessage = s"$message"
  def pos = if (position == null) SourcePosition(null, 2, 1) else position
}

case class VerifierThrowsException(message: String, pos: Position) extends AbstractError {
  def fullId = "verifier.exception"
  def readableMessage = s"$message"
}
