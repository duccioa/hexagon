#' @title Class object for hexagon center
#' @name HexCubeCenter
#' @description This is a class that contains the Cube coordinates of the center
#' of an Hexagon.
#' It inherits from `CubeCoords` but it is specific for `Hexagon`.
#' @usage HexCubeCenter$new(x)
#' @param x A numeric vector of length 3 or 2, whose sum is 0.
#' @references \link{https://www.redblobgames.com/grids/hexagons}
#' @export
HexCubeCenter = R6::R6Class(
  "HexCubeCenter",
  inherit=CubeCoords,
  public = list(
    # HexCubeCenter's Public methods ----
    #' @description Initialise a new instance.
    #' @return An object of class `HexCubeCenter`.
    initialize = function(x){
      super$initialize(x)
    }
  ),
  active = list(
    # HexCubeCenter's Active fields ----
  ),
  private = list(
    # HexCubeCenter's Private fields ----

    # HexCubeCenter's Private methods ----

  )
)

# HexCubeCenter's Methods ----

#' @title Cube direction vectors from an hexagon
#' @description  `hex_direction` and `hex_diagonal_direction` return a vector
#' towards one of the the six possible directions in the hexagon's neighbour.
#' They are ued in the neighbour methods `hex_neighbour` and
#' `hex_diagonal_neighbour`. Given two `HexCubeCenter` A and B, where A is a
#' generic center and B is the center build on the output of the direction
#' functions, the sum A + B gives the coordinates of neighbour hexagon in the
#' specified direction.
#' @param direction a number between 1 and 6.
#' @return A numeric vector of length 3.
#' @name direction
#' @rdname direction
#' @references \link{https://www.redblobgames.com/grids/hexagons}
#' @aliases hex_direction,hex_diagonal_direction
NULL

#' @describeIn direction Directions of the immediate neighbour.
hex_direction = function(direction){
  .HexDirection = list((c(1, 0, -1)),
                       (c(1, -1, 0)),
                       (c(0, -1, 1)),
                       (c(-1, 0, 1)),
                       (c(-1, 1, 0)),
                       (c(0, 1, -1)))
  HexCubeCenter$new(.HexDirection[[direction]])
}

#' @describeIn direction Directions of the diagonal neighbour.
hex_diagonal_direction = function(direction){
  .HexDiagonals = list((c(2, -1, -1)),
                       (c(1, -2, 1)),
                       (c(-1, -1, 2)),
                       (c(-2, 1, 1)),
                       (c(-1, 2, -1)),
                       (c(1, 1, -2)))
  HexCubeCenter$new(.HexDiagonals[[direction]])
}

#' @describeIn HexCubeCenter Find the neighbour center along a direction.
hex_neighbour = function(object, direction){
  UseMethod("hex_neighbour")
}

#' @title Hexagon neighbour
#' @description `hex_neighbour` method for class HexCubeCenter
#' @return An object of class `HexCubeCenter`.
hex_neighbour.HexCubeCenter = function(hex, direction){
  add(hex, hex_direction(direction))
}

#' @describeIn HexCubeCenter Find the neighbour center along a diagonal direction.
hex_diagonal_neighbour = function(object, direction){
  UseMethod("hex_diagonal_neighbour")
}

#' @title Hexagon diagonal neighbour
#' @description `hex_diagonal_neighbour` method for class HexCubeCenter
#' @return An object of class `HexCubeCenter`.
hex_diagonal_neighbour.HexCubeCenter = function(hex, direction){
  add(hex, hex_diagonal_direction(direction))
}


