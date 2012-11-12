package ch.ethz.inf.pm.semper.chalice2sil

/**
  * @author Christian Klauser
  */
final case class ExpectedSiliconMessage(line : Int, code : Int) {
  override def toString = if(code == 0) "@Error *" else "@Error " + code
}
