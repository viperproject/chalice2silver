/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.chalice2sil

/**
Author: Yannis Kassios
*/

import java.nio.file.Path
import viper.silver.testing._
import viper.silver.verifier._
import viper.silicon.Silicon
import viper.silver.frontend._

class AllTests extends SilSuite {
  override lazy val testDirectories: Seq[String] = Seq(
    "basic",
    "oldC2SCases",
    "chaliceSuite/examples",
    "chaliceSuite/general-tests",
    "chaliceSuite/permission-model"/*,
    "chaliceSuite/predicates",
    "chaliceSuite/regressions",
    "chaliceSuite/substantial-examples",
    "quantificationOverPermissions"*/
    // these test cases are removed from the test suite until they are annotated (todo)
  )

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new Chalice2SILFrontEnd()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  override val verifiers = {
    val silicon = new Silicon()
    silicon.parseCommandLine(optionsFromScalaTestConfigMap())
    silicon.config.initialize {
      case _ =>
        /* Ignore command-line errors, --help, --version and other non-positive
         * results from Scallop.
         * After initialized has been set to true, Silicon itself will not call
         * config.initialize again.
         */
        silicon.config.initialized = true
    }
    Seq(silicon)
  }

  override val defaultTestPattern: String = ".*\\.chalice"

  private def optionsFromScalaTestConfigMap(): Seq[String] = {
    val prefix = "silicon:"

    prefixSpecificConfigMap.get(prefix) match {
      case None => Seq()
      case Some(optionMap) => optionMap.flatMap{
        case (k, v) => Seq(s"--$k", v.toString)
      }.toSeq
    }
  }
}
