package com.fivebytestudios.wildfreddy
package helpers

import scala.io.Source

trait ResourceHelpers {
  private val resourceBasePath = "2020"
  def resourcePath: String

  def getResourceLines(fileName: String): List[String] =
    Source.fromResource(s"$resourceBasePath/$resourcePath/$fileName")
      .getLines.toList
}
