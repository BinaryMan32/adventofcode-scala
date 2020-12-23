package com.fivebytestudios.wildfreddy

object Day20 {
  private val debug = false
  case class TileEdge(edge: String) {
    def flipped: TileEdge = TileEdge(edge.reverse)
  }
  case class Tile(id: Int, data: List[String]) {
    private def column(c: Int): String =
      data.map(_(c)).mkString
    def edges: List[TileEdge] = List(
      TileEdge(data.head),
      TileEdge(column(9)),
      TileEdge(data.last).flipped,
      TileEdge(column(0)).flipped
    )
    def allPossibleEdges: List[TileEdge] =
      edges ++ edges.map(_.flipped)
    def oriented: TileOrientation =
      TileOrientation(edges, this)
  }
  case class TileOrientation(edges: List[TileEdge], tile: Tile) {
    def id: Int = tile.id
    def flipped: TileOrientation = copy(edges = edges.map(_.flipped).reverse)
    def rotated: TileOrientation = copy(edges = edges.last :: edges.dropRight(1))
    def rotations: List[TileOrientation] = Iterator.iterate(this)(_.rotated).take(4).toList
    def orientations: List[TileOrientation] = rotations.flatMap(
      orientation => List(orientation, orientation.flipped)
    )
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
          tile.allPossibleEdges.distinct.map(_ -> tile)
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

    def corners: List[TileOrientation] = {
      val rows = assembled.iterator.map{ case (r, _) -> _ => r }.distinct.toList
      val cols = assembled.iterator.map{ case (_, c) -> _ => c }.distinct.toList
      List(
        (rows.min, cols.min),
        (rows.min, cols.max),
        (rows.max, cols.max),
        (rows.max, cols.min)
      ).map(assembled)
    }
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

  def part1(input: List[String]): Long = {
    val tiles = Utilities.groupLines(input).map(parseTile)
    search(State.initial(tiles))
      .map(_.corners.map(_.id.toLong).product)
      .get
  }

  def part2(input: List[String]): Long = {
    0
  }
}
