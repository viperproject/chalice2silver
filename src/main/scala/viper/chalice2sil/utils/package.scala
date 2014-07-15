/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.chalice2sil.utils

import java.io.{
  File => JFile,
  PrintWriter => JPrintWriter,
  BufferedWriter => JBufferedWriter,
  FileWriter => JFileWriter}

/* TODO: This is a copy of silicon/common/io. We should create viper-wide
 *       "common"-packages that bundle such generally useful functions.
 */

package object io {
  /**
   * Writes the `contents` to the given `file`. Existing content will be overwritten.
   *
   * @param contents The content to write.
   * @param file The file to which the content will be written.
   */
  def writeToFile(contents: String, file: JFile) {
    val sink = PrintWriter(file)

    sink.write(contents)
    sink.close()
  }

  /**
   * Creates a `java.io.PrintWriter` with `autoFlush` enabled that writes to the given `file`.
   * `File.mkdirs()` is called to ensure that the file path exists.
   *
   * @param file Is assumed to denote a file, not a directory.
   * @return The instantiated sink.
   */
  def PrintWriter(file: JFile): JPrintWriter = {
    val pf = file.getParentFile
    if (pf != null) pf.mkdirs()

    new JPrintWriter(new JBufferedWriter(new JFileWriter(file)), true)
  }
}
