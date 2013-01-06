package semper.chalice2sil.translation

import semper.sil.ast.expressions.terms.OldTerm
import semper.sil.ast.types.{booleanType, DataType}
import semper.sil.ast.expressions.OldExpression
import semper.sil.ast.ASTNode

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
