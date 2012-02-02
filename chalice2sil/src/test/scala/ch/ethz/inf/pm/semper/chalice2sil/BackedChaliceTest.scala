package ch.ethz.inf.pm.semper.chalice2sil

import java.nio.file.Path
import org.junit.runners.model.{Statement, FrameworkMethod}
import org.junit.runner.Description

/**
 * Author: Christian Klauser
 */

class BackedChaliceTest(backingClass : Class[_], val sourceFile : Path, val backingMethod : Option[FrameworkMethod] = None) {
  lazy val description = backingMethod match {
    case Some(m) => Description.createTestDescription(backingClass, m.getName)
    case None    => Description.createTestDescription(backingClass, "$implicit$")
  }
}