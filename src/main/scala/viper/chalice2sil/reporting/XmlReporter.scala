/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.chalice2sil

import java.io.File
import xml.{PrettyPrinter, Document, Node, NodeSeq}
import viper.silver.ast.{HasLineColumn, NoPosition, Position}
import viper.silver.verifier.{VerificationError, AbstractVerificationError, Success, VerificationResult, AbstractError, Failure}

/** The XML document generated by this reporter should be structurally close
  * to the XML document created by boogie -xml:report.xml input.bpl.
  *
  * Stefan Blom suggested this and it seems reasonable to follow his suggestion
  * because there might be several tools out there that can already parse
  * Boogie's XML output.
  */
class XmlReporter {
  var nodes = NodeSeq.Empty

  protected def splitPosition(pos: Position) = pos match {
    case NoPosition => ("0", "0")
    case rp: HasLineColumn => (rp.line, rp.column)
  }

  protected def formatErrorMessage(error: AbstractError) = error match {
    case ave: AbstractVerificationError => ave.readableMessage(false, false)
    case other => error.readableMessage
  }

  protected def formatError(sourceFile: String, error: AbstractError) = {
    val (line, column) = splitPosition(error.pos)

    <error
      id={error.fullId}
      message={formatErrorMessage(error)}
      file={sourceFile}
      line={line.toString}
      column={column.toString}
    > {error match {
      case ve: VerificationError =>
        val (line, column) = splitPosition(ve.reason.pos)

        <reason
          id={ve.reason.id}
          message={ve.reason.readableMessage}
          file={sourceFile}
          line={line.toString}
          column={column.toString}
        />
      case _ =>
        NodeSeq.Empty
    }}</error>
  }

  def reset() {
    nodes = NodeSeq.Empty
  }

  def addSection(sourceFile: String, result: VerificationResult) = {
    val fileNode =
      <file name={sourceFile}> { result match {
        case Success => NodeSeq.Empty
        case Failure(errors) => errors map (error => formatError(sourceFile, error))
      }} </file>

    nodes ++= fileNode
  }

  def writeToFile(file: File) {
    val pp = new PrettyPrinter(120, 2)
    val rootNode = <chalice2sil>{nodes}</chalice2sil>
    val stringRep = pp.format(rootNode)

    utils.io.writeToFile(stringRep, file)
  }
}
