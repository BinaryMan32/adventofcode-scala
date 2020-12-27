package com.fivebytestudios.wildfreddy

import scala.annotation.tailrec

object Day24 {
  object TileNeighbor extends Enumeration {
    type TileNeighbor = Value
    val East, SouthEast, SouthWest, West, NorthWest, NorthEast = Value
    def parseLine(line: String): Seq[TileNeighbor] = {
      @tailrec
      def parseLineAux(parsed: List[TileNeighbor], line: String):
        Seq[TileNeighbor] = line.headOption match {
        case Some('n') => line(1) match {
          case 'e' => parseLineAux(NorthEast :: parsed, line.drop(2))
          case 'w' => parseLineAux(NorthWest :: parsed, line.drop(2))
        }
        case Some('e') => parseLineAux(East :: parsed, line.tail)
        case Some('s') => line(1) match {
          case 'e' => parseLineAux(SouthEast :: parsed, line.drop(2))
          case 'w' => parseLineAux(SouthWest :: parsed, line.drop(2))
        }
        case Some('w') => parseLineAux(West :: parsed, line.tail)
        case None => parsed
      }
      parseLineAux(List.empty, line).reverse
    }
  }
  import TileNeighbor._

  /**
   * Represents a tile addressing scheme with neighbor transitions.
   *
   * This models movement based on the idea that advancing diagonally should
   * move half as much in X as in Y. As long as all movement begins at ORIGIN,
   * coordinates are limited to the following:
   * 1. if y is even, x must be even
   * 2. if y is odd, x must be odd
   */
  case class TileAddress(x: Int, y: Int) {
    def move(neighbor: TileNeighbor): TileAddress = neighbor match {
      case NorthWest => TileAddress(x - 1, y + 1)
      case NorthEast => TileAddress(x + 1, y + 1)
      case West => TileAddress(x - 2, y)
      case East => TileAddress(x + 2, y)
      case SouthWest => TileAddress(x - 1, y - 1)
      case SouthEast => TileAddress(x + 1, y - 1)
    }
    def adjacent: Iterable[TileAddress] = TileNeighbor.values.toSeq.map(move)
    def atPathEnd(neighbors: Iterable[TileNeighbor]): TileAddress =
      neighbors.foldRight(this)((neighbor, address) => address.move(neighbor))
  }

  object TileAddress {
    private val ORIGIN = TileAddress(0, 0)
    def resolvePath(neighbors: Iterable[TileNeighbor]): TileAddress =
      ORIGIN.atPathEnd(neighbors)
  }

  case class Floor(blackTiles: Set[TileAddress] = Set.empty) {
    def flipTile(address: TileAddress): Floor =
      copy(blackTiles =
        if (blackTiles.contains(address))
          blackTiles - address
        else
          blackTiles + address
      )

    def numBlackTiles: Int = blackTiles.size

    def nextDay: Floor = {
      val numBlackAdjacent = blackTiles
        .toSeq
        .flatMap(_.adjacent)
        .groupMapReduce(identity)(_ => 1)(_ + _)
      val blackTilesNoAdjacent = blackTiles -- numBlackAdjacent.keySet
      val blackTilesMoreThan2Adj = blackTiles & numBlackAdjacent.filter(_._2 > 2).keySet
      val flipToWhite = blackTilesNoAdjacent ++ blackTilesMoreThan2Adj
      val whiteTilesWith2Adj = numBlackAdjacent.filter(_._2 == 2).keySet -- blackTiles
      val flipToBlack = whiteTilesWith2Adj
      copy(blackTiles = blackTiles -- flipToWhite ++ flipToBlack)
    }
  }

  object Floor {
    def fromInput(input: List[String]): Floor =
      input.map(TileNeighbor.parseLine)
        .foldRight(Floor())((path, floor) =>
          floor.flipTile(TileAddress.resolvePath(path))
        )
  }

  def part1(input: List[String]): Long = {
    Floor.fromInput(input).numBlackTiles
  }

  def part2(input: List[String]): Long = {
    Iterator.iterate(Floor.fromInput(input))(_.nextDay)
      .drop(100)
      .next
      .numBlackTiles
  }
}
