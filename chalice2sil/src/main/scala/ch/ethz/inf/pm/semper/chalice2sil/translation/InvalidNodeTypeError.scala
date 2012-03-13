package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.ASTNode

class InvalidNodeTypeError(node : ASTNode, classManifest : ClassManifest[_]) extends Error(
  "Chalice2SIL tried to supply a node of type %s (%s) where a node of type %s is expected.".format(node.getClass, node, classManifest))