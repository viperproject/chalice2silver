package ch.ethz.inf.pm.semper.chalice2sil.translation.ssa

import ch.ethz.inf.pm.semper.chalice2sil._
import translation.ssa._
import collection._

/**
  * @author Christian Klauser
  */
class AssignmentInterpretation(block : ChaliceBlock) {
  private [this] val indices = mutable.Map(block.blockVariableInfoMap.keys.map(_ -> 0).toSeq: _*)
  
  def version(v : chalice.Variable) : Version = {
    val vi = block.blockVariableInfo(v)
    vi.versions(indices(v))
  }
  
  def registerAssignment(v : chalice.Variable) : Version = {
    val vi = block.blockVariableInfo(v)
    val nextIndex = indices(v)+1    
    require(vi.versions.isDefinedAt(nextIndex),"Too many assignments registered in block %s.".format(block))
    indices.update(v,nextIndex)
    version(v)
  }
  
  
}
