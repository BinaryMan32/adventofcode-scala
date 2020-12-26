package com.fivebytestudios.wildfreddy

object Day20 {
  private val debug = false
  case class TileEdge(edge: String) {
    def flipped: TileEdge = TileEdge(edge.reverse)
  }
  case class Tile(id: Int, data: List[String]) {
    def oriented: TileOrientation =
      TileOrientation(data, this)
  }
  case class TileOrientation(data: List[String], tile: Tile) {
    val edges: List[TileEdge] = List(
      TileEdge(data.head),
      TileEdge(data.map(_.last).mkString),
      TileEdge(data.last.reverse),
      TileEdge(data.map(_.head).mkString.reverse)
    )
    def allPossibleEdges: List[TileEdge] =
      edges ++ edges.map(_.flipped)
    def id: Int = tile.id
    def flipped: TileOrientation = copy(data = data.indices.map(i =>
      data.map(_(i)).mkString
    ).toList)
    def rotated: TileOrientation = copy(data = data.indices.map(i =>
      data.map(_(i)).mkString.reverse
    ).toList)
    def rotations: List[TileOrientation] = Iterator.iterate(this)(_.rotated).take(4).toList
    def orientations: List[TileOrientation] = rotations.flatMap(
      orientation => List(orientation, orientation.flipped)
    )
    def dataWithoutEdges: List[String] =
      data.tail.dropRight(1)
        .map(_.tail.dropRight(1))
  }
  case class EdgePattern(pattern: List[Option[TileEdge]]) {
    def matchingRotations(tile: Tile): List[TileOrientation] =
      tile.oriented.rotations.filter(matches)
    def matches(orientation: TileOrientation): Boolean =
      (orientation.edges zip pattern)
        .forall{case (e, p) => p.forall(_ == e)}
    def flipped: EdgePattern = copy(pattern = pattern.map(_.map(_.flipped)).reverse)
  }

  case class EdgeIndex(edges: Map[TileEdge, Set[Tile]]) {
    def isEmpty: Boolean = edges.isEmpty
    def matching(pattern: EdgePattern): List[TileOrientation] = {
      matchingSide(pattern) ++ matchingSide(pattern.flipped).map(_.flipped)
    }
    def matchingSide(pattern: EdgePattern): List[TileOrientation] = {
      val candidates = pattern.pattern
        .flatten
        .flatMap(edges.get)
        .reduceOption(_ intersect _)
        .map(_.toList)
        .getOrElse(List.empty)
      candidates.flatMap(pattern.matchingRotations)
    }
    def removedTile(tile: TileOrientation): EdgeIndex =
      EdgeIndex(
        edges
          .map{ case edge -> tiles => edge -> tiles.filterNot(_.id == tile.id) }
          .filter(_._2.nonEmpty)
      )
  }
  object EdgeIndex {
    def fromTiles(tiles: List[Tile]): EdgeIndex =
      EdgeIndex(
        tiles.flatMap(tile =>
          tile.oriented.allPossibleEdges.distinct.map(_ -> tile)
        ).groupMapReduce(_._1){case _ -> t => Set(t)}(_ ++ _)
      )
  }
  case class Move(tile: TileOrientation, destination: (Int, Int)) {
    def description: String =
      s"Placing tile ${tile.id} at ${destination}"
  }
  case class State(assembled: Map[(Int, Int), TileOrientation],
                   border: Map[(Int, Int), EdgePattern], unused: EdgeIndex,
                   level: Int) {
    import State._
    def moves: Iterable[Move] =
      for {
        (destination, pattern) <- border
        orientation <- unused.matching(pattern)
      } yield Move(tile = orientation, destination = destination)

    def apply(move: Move): State = {
      if (debug)
        println(" " * level + move.description)
      val newAssembled = assembled.updated(move.destination, move.tile)
      State(
        assembled = newAssembled,
        border = border - move.destination
          ++ neighborEdgePatterns(newAssembled, move.destination),
        unused = unused.removedTile(move.tile),
        level = level + 1
      )
    }

    def ranges: ((Int, Int), (Int, Int)) = {
      val rows = assembled.iterator.map{ case (r, _) -> _ => r }.distinct.toList
      val cols = assembled.iterator.map{ case (_, c) -> _ => c }.distinct.toList
      ((rows.min, rows.max), (cols.min, cols.max))
    }

    def corners: List[TileOrientation] = {
      val ((rowsMin, rowsMax), (colsMin, colsMax)) = ranges
      List(
        (rowsMin, colsMin),
        (rowsMin, colsMax),
        (rowsMax, colsMax),
        (rowsMax, colsMin)
      ).map(assembled)
    }

    def assembledTiles: List[List[TileOrientation]] = {
      val ((rowsMin, rowsMax), (colsMin, colsMax)) = ranges
      (rowsMin to rowsMax).map(r =>
        (colsMin to colsMax).map(c =>
          assembled((r, c))
        ).toList
      ).toList
    }

    def assembledData: List[String] =
      assembledTiles.flatMap(row =>
        row.map(_.dataWithoutEdges).transpose.map(_.reduce(_ ++ _))
      )
  }
  object State {
    def initial(tiles: List[Tile]): State = {
      val initialMove = Move(tiles.head.oriented, (0, 0))
      State(
        assembled = Map.empty,
        border = Map.empty,
        unused = EdgeIndex.fromTiles(tiles),
        level = 0
      )(initialMove)
    }

    def neighborEdgePatterns(assembled: Map[(Int, Int), TileOrientation],
                             position: (Int, Int)): Map[(Int, Int), EdgePattern] =
      neighborPositions(position)
        .filterNot(assembled.contains)
        .map{ pos =>
          pos -> edgePattern(assembled, pos)
        }.toMap

    def neighborPositions(position: (Int, Int)): List[(Int, Int)] =
      position match {
        case (r, c) => List((r-1, c), (r, c+1), (r+1, c), (r, c-1))
      }

    def edgePattern(assembled: Map[(Int, Int), TileOrientation],
                    position: (Int, Int)): EdgePattern =
      position match {
        case (r, c) => EdgePattern(List(
          assembled.get((r - 1, c)).map(_.edges(2)),
          assembled.get((r, c + 1)).map(_.edges(3)),
          assembled.get((r + 1, c)).map(_.edges(0)),
          assembled.get((r, c - 1)).map(_.edges(1))
        ).map(_.map(_.flipped)))
      }
  }

  def parseTile(input: List[String]) : Tile = {
    val tileRegex = """Tile (\d+):""".r
    val tileId = input.head match {
      case tileRegex(id) => id.toInt
    }
    Tile(
      id = tileId,
      data = input.tail
    )
  }

  def parseInput(input: List[String]): List[Tile] =
    Utilities.groupLines(input).map(parseTile)

  def search(state: State): Option[State] = {
    if (state.unused.isEmpty)
      Some(state)
    else {
      val moves = state.moves
      if (debug) {
        val borderDescriptions = state.border
          .map { case pos -> pat => s"  $pat at $pos" }.toList
        println(("* borders:" :: borderDescriptions)
          .map(msg => s"${" " * state.level}$msg").mkString("\n"))
        val moveDescriptions = moves
          .map(m => s"  ${m.tile.id} ${m.tile.edges} at ${m.destination}").toList
        println(("* possible moves:" :: moveDescriptions)
          .map(msg => s"${" " * state.level}$msg").mkString("\n"))
      }
      moves.iterator
        .map(move => search(state(move)))
        .collectFirst{case Some(state) => state}
    }
  }

  def assembleTiles(input: List[String]): State = {
    val tiles = Utilities.groupLines(input).map(parseTile)
    search(State.initial(tiles)).get
  }

  case class Pattern(required: Map[(Int, Int), Char], size: (Int, Int)) {
    def flipped: Pattern = copy(
      required = required.map{ case (r, c) -> v => (r, size._2 - 1 - c) -> v}
    )
    def rotated: Pattern = copy(
      required = required.map{ case (r, c) -> v => (c, size._1 - 1 - r) -> v},
      size = size.swap
    )
    def rotations: List[Pattern] = Iterator.iterate(this)(_.rotated).take(4).toList
    def orientations: List[Pattern] = rotations.flatMap(
      orientation => List(orientation, orientation.flipped)
    )
    def visualize: List[String] =
      (0 until size._1).map(r =>
        (0 until size._2).map(c =>
          required.getOrElse((r, c), ' ')
        ).mkString
      ).toList
    def findMatchingSpaces(data: List[String]): Set[(Int, Int)] = {
      val offsets = for {
        r <- 0 until (data.size - size._1)
        c <- 0 until (data.head.length - size._2)
      } yield (r, c)
      offsets.map{ case (rb, cb) =>
        val requiredOffsets = required.map{ case (r, c) -> v =>
          (rb + r, cb + c) -> v
        }
        if (requiredOffsets.forall{case (r, c) -> v => data(r)(c) == v})
          requiredOffsets.keySet
        else
          Set.empty[(Int, Int)]
      }.reduce(_ ++ _)
    }
    def findAllMatchingSpaces(data: List[String]): Set[(Int, Int)] = {
      orientations
        .map(_.findMatchingSpaces(data))
        .reduce(_ ++ _)
    }
    def labelMonsters(data: List[String], label: Char = 'O'): List[String] = {
      val matching = findAllMatchingSpaces(data)
      data.zipWithIndex.map{case (row, r) =>
        row.zipWithIndex.map{case (v, c) =>
          if (matching.contains((r, c)))
            label
          else
            v
        }.mkString
      }
    }
  }
  object Pattern {
    def fromStringList(input: List[String]): Pattern = {
      val required = for {
        (row, r) <- input.zipWithIndex
        (v, c) <- row.zipWithIndex if v != ' '
      } yield (r, c) -> v
      Pattern(required=required.toMap, size=(input.size, input.map(_.length).max))
    }
  }

  val seaMonsterPatternStringList: List[String] = List(
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  )
  val seaMonsterPattern: Pattern = Pattern.fromStringList(seaMonsterPatternStringList)

  def part1(input: List[String]): Long =
    assembleTiles(input)
      .corners.map(_.id.toLong).product

  def part2(input: List[String]): Long = {
    val assembled = assembleTiles(input).assembledData
    seaMonsterPattern
      .labelMonsters(assembled)
      .flatten
      .count(_ == '#')
  }
}
