package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import ch.ethz.inf.pm.semper.chalice2sil
import chalice2sil._
import silAST.expressions.{OldExpression, QuantifierExpression, Expression}
import silAST.symbols.logical.quantification.LogicalVariable
import silAST.expressions.terms.{LogicalVariableTerm, Term}

/*
Copyright (c) 2012, Christian Klauser
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.*/

object ContractFormChecker extends ExpressionVisitor[Set[LogicalVariable],Set[Message]] {
  protected override def merge(left : Set[Message], right : Set[Message]) = left ++ right
  protected override def zero = Set()

  def apply(expr : Expression) : Set[Message] = visitExpression(expr,Set())
  def apply(term : Term) : Set[Message] = visitTerm(term,Set())

  override def visitExpression(expression : Expression, arg : Set[LogicalVariable]) : Set[Message] = expression match {
    case old@OldExpression(inner) => super.visitExpression(old,inner.freeVariables)
    case e => super.visitExpression(e, arg)
  }

  override def visitTerm(term : Term, arg : Set[LogicalVariable]) : Set[Message] = term match {
    case t@LogicalVariableTerm(v) if arg contains v =>
         super.visitTerm(t,arg) + messages.FreeVariableInOld(t)
    case t => super.visitTerm(t, arg)
  }
}
