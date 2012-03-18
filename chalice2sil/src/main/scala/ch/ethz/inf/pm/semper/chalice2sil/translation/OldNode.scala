package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.terms.OldTerm
import silAST.types.{booleanType, DataType}
import silAST.expressions.{PProgramVariableSubstitution, OldExpression}
import silAST.ASTNode

/**
  * @author Christian Klauser
  */
sealed abstract class OldNode {
  def dataType : DataType
  def astNode : ASTNode
}

final case class OldTermNode(oldTerm : OldTerm) extends OldNode {
  def dataType = oldTerm.dataType
  def astNode = oldTerm
}

final case class OldExpressionNode(oldExpression : OldExpression) extends OldNode {
  def dataType = booleanType
  def astNode = oldExpression
}
