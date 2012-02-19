package ch.ethz.inf.pm.semper.chalice2sil.translation

import operators.Lookup
import silAST.programs.ProgramFactory
import silAST.methods.MethodFactory
import silAST.domains.{Domain, DomainPredicate, DomainFunction}
import silAST.types.{DataType, referenceDomain, permissionDomain, integerDomain}

/**
 * Author: Christian Klauser
 * Date: 24.01.12
 */

trait ProgramEnvironment extends Environment {
  def programFactory : ProgramFactory
  def methodFactories : FactoryCache[chalice.Method,MethodFactory]
  def fields : FactoryCache[chalice.Field, silAST.programs.symbols.Field]
  def prelude : ChalicePrelude

  lazy val domains = List(integerDomain,permissionDomain,referenceDomain,prelude.Boolean.Domain)
  lazy val domainFunctionLookup = new operators.OperatorLookup[DomainFunction]() {
    def lookup(domains : TraversableOnce[Domain]) : (String, Seq[Option[DataType]]) => Lookup[DomainFunction] =
      lookup(domains.flatMap(d => d.functions)) _
  }
  lazy val domainPredicateLookup = new operators.OperatorLookup[DomainPredicate]() {
    def lookup(domains : TraversableOnce[Domain]) : (String, Seq[Option[DataType]]) => Lookup[DomainPredicate] =
      lookup(domains.flatMap(_.predicates)) _
  }
}