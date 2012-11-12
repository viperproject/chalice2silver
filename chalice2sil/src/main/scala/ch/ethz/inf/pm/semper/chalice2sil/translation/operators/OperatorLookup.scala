package ch.ethz.inf.pm.semper.chalice2sil.translation.operators

import collection._
import semper.sil.ast.types.{referenceEquality, DataType, DataTypeSequence}

class OperatorLookup[TOp <: {def signature : {def parameterTypes : DataTypeSequence}; def name : String}] {
  protected def operatorNameCandidates(opName : String) : List[String] = opName :: (opName match {
    case "=="
       | "<==>" => List("↔","<=>",referenceEquality.name)
    case "<" => List("LT")
    case "<=" | "≤" => List("LE")
    case ">" => List("GT")
    case ">=" | "≥" => List("GE")
    case _ => Nil
  })

  def assignableFrom(lhsType : DataType, rhsType : DataType) : Boolean = lhsType.isCompatible(rhsType)

  def lookup(candidates : TraversableOnce[TOp])(name : String, parameterConstraints : Seq[Option[DataType]]) : Lookup[TOp] = {
    val nameCandidates = operatorNameCandidates(name)
    val finalCandidatesView = candidates
      .filter(nameCandidates contains _.name)
      .map(c => {
      val params = c.signature.parameterTypes
      if(params.length != parameterConstraints.length) {
        None
      } else {
        val matched = params
          .zip(parameterConstraints)
          .map({
          case (actual, Some(constraint)) => assignableFrom(actual, constraint)
          case _ => true
        })
          .forall(identity)
        if(matched) Some(c) else None
      }
    })
      .filter(_.isDefined)
      .map(_.get)
      .toSet
    Lookup.fromList(finalCandidatesView.toList)
  }
}

