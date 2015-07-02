/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.chalice2sil

import java.nio.file.Path
import viper.silver.testing._
import viper.silver.verifier._
import viper.silicon.Silicon
import viper.carbon.CarbonVerifier
import viper.silver.frontend._

class AllTests extends SilSuite {
  override lazy val testDirectories: Seq[String] = Seq(
    "basic",
    "oldC2SCases",
    "chaliceSuite/examples",
    "chaliceSuite/general-tests",
    "chaliceSuite/permission-model",
    "chaliceSuite/predicates",
    "chaliceSuite/regressions",
    "chaliceSuite/substantial-examples"
    /*, "quantificationOverPermissions"*/
    // quantified permissions are not included in the test cases, until the feature is stable in Silicon
  )

  override def projectInfo: ProjectInfo = super.projectInfo.update("Chalice2Silver")

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    require(files.length == 1, "tests should consist of exactly one file")

    val fe = new Chalice2SILFrontEnd()
    fe.init(verifier)
    fe.reset(files.head)
    fe
  }

  override val verifiers = createSiliconInstance() :: createCarbonInstance() :: Nil

  private def createSiliconInstance() = {
    val args = Silicon.optionsFromScalaTestConfigMap(prefixSpecificConfigMap.getOrElse("silicon", Map()))
    val silicon = Silicon.fromPartialCommandLineArguments(args)

    silicon
  }

  private def createCarbonInstance() = {
    val carbon = CarbonVerifier()
    carbon
  }

  override val defaultTestPattern: String = ".*\\.chalice"
}
