package semper.chalice2sil.translation

import scala.reflect.ClassTag
import semper.sil.ast.ASTNode

class InvalidNodeTypeError(node : ASTNode, classTag : ClassTag[_]) extends Error(
  "Chalice2SIL tried to supply a node of type %s (%s) where a node of type %s is expected.".format(node.getClass, node, classTag))
