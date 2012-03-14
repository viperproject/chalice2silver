package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.terms.OldTerm
import silAST.expressions.OldExpression
import silAST.types.{booleanType, DataType}

/**
  * @author Christian Klauser
  */
sealed abstract class OldNode {
  def dataType : DataType
}

final case class OldTermNode(oldTerm : OldTerm) extends OldNode {
  def dataType = oldTerm.dataType
}

final case class OldExpressionNode(oldExpression : OldExpression) extends OldNode {
  def dataType = booleanType
}
