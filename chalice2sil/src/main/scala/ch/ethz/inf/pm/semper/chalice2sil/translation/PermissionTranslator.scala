package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil._

import silAST.expressions.terms.Term
import silAST.source.SourceLocation

/**
  * @author Christian Klauser
  */
trait PermissionTranslator extends TermTranslator {
  
  def translatePermission(permission : chalice.Permission) = permissionTranslation(permission)

  protected def matchingPermission(partialFunction : PartialFunction[chalice.Permission, Term]) = partialFunction

  protected def permissionTranslation = matchingPermission {
    case f@chalice.Full => currentExpressionFactory.makeFullPermission(f)
    case k@chalice.Epsilon => readFraction(k)
    case k@chalice.MethodEpsilon => readFraction(k)
    case permission =>
      report(messages.UnknownAstNode(permission))
      currentExpressionFactory.makeNoPermission(permission)
  }

  protected def readFraction(location : SourceLocation) : Term = {
    report(messages.NoContextForReadPermission(location))
    currentExpressionFactory.makeNoPermission(location)
  }
}
