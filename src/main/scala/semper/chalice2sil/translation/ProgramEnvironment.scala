package semper.chalice2sil.translation

import operators.Lookup
import semper.sil.ast.programs.ProgramFactory
import semper.sil.ast.methods.MethodFactory
import semper.sil.ast.domains.{Domain, DomainPredicate, DomainFunction}
import semper.sil.ast.types.{DataType, referenceDomain, permissionDomain, integerDomain}
import semper.sil.ast.programs.symbols.{FunctionFactory, PredicateFactory}
import util.{DerivedFactoryCache, FactoryCache}

/**
 * Author: Christian Klauser
 * Date: 24.01.12
 */

trait ProgramEnvironment extends Environment {
  def programFactory : ProgramFactory
  def methods : FactoryCache[chalice.Method,MethodTranslator]
  def fields : DerivedFactoryCache[chalice.Field, String, FieldTranslator]
  def predicates : DerivedFactoryCache[chalice.Predicate, String, PredicateTranslator]
  def functions : DerivedFactoryCache[chalice.Function,  String,  FunctionTranslator]
  def monitorInvariants : DerivedFactoryCache[chalice.Class, String, PredicateTranslator]
  def prelude : ChalicePrelude

  lazy val domains = List(integerDomain,permissionDomain,referenceDomain,prelude.Boolean.domain)
  lazy val domainFunctionLookup = new operators.OperatorLookup[DomainFunction]() {
    def lookup(domains : TraversableOnce[Domain]) : (String, Seq[Option[DataType]]) => Lookup[DomainFunction] =
      lookup(domains.flatMap(d => d.functions)) _
  }
  lazy val domainPredicateLookup = new operators.OperatorLookup[DomainPredicate]() {
    def lookup(domains : TraversableOnce[Domain]) : (String, Seq[Option[DataType]]) => Lookup[DomainPredicate] =
      lookup(domains.flatMap(_.predicates)) _
  }
}