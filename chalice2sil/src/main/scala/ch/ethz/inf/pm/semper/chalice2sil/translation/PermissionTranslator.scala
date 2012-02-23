package ch.ethz.inf.pm.semper.chalice2sil.translation

import ch.ethz.inf.pm.semper.chalice2sil._

import silAST.expressions.terms.Term
import silAST.source.SourceLocation

/**
  * @author Christian Klauser
  */
trait PermissionTranslator[T <: Term] extends TermTranslator[T] {
  
  def translatePermission(permission : chalice.Permission) = permissionTranslation(permission)

  protected def matchingPermission(partialFunction : PartialFunction[chalice.Permission, Term]) : PartialFunction[chalice.Permission, T] =
    partialFunction andThen {
      case e if ClassManifest.fromClass(e.getClass) <:< termClassManifest =>  e.asInstanceOf[T]
      case e => throw new InvalidNodeTypeError(e,termClassManifest)
    }

  protected def permissionTranslation = matchingPermission {
    case f@chalice.Full => currentExpressionFactory.makeFullPermission(f)
    case k@chalice.Epsilon if k.permissionType == chalice.PermissionType.Fraction => readFraction(k)
    case permission =>
      report(messages.UnknownAstNode(permission))
      currentExpressionFactory.makeNoPermission(permission)
  }

  protected def readFraction(location : SourceLocation) : Term = {
    report(messages.NoContextForReadPermission(location))
    currentExpressionFactory.makeNoPermission(location)
  }
}
