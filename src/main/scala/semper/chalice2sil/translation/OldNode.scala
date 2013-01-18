package semper.chalice2sil.translation

import semper.sil.ast.types.DataType
import semper.sil.ast.expressions.OldExpression
import semper.sil.ast.ASTNode

/**
 * @author Christian Klauser
 */
sealed abstract class OldNode {
  def dataType: DataType

  def astNode: ASTNode
}

final case class OldExpressionNode(oldExpression: OldExpression) extends OldNode {
  def dataType = oldExpression.dataType

  def astNode = oldExpression
}
