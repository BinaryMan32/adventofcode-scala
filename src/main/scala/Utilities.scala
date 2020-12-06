package com.fivebytestudios.wildfreddy

object Utilities {
  def groupLines(input: List[String], isDelimiter: String => Boolean = _.isEmpty):
    List[List[String]] = {
    input.foldRight(List(List.empty[String])) { case (line, groups) =>
      if (isDelimiter(line))
        List.empty[String] :: groups
      else
        (line :: groups.head) :: groups.tail
    }
  }
}
