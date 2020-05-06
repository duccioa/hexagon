# HexCenter = R6::R6Class(
#   "HexCenter",
#   inherit = Coords,
#   public = list(
#     # Public methods ----
#     initialize = function(x){
#       super$initialize(x)
#     }
#     # Public fields ----
#
#   ),
#   active = list(
#     # Active methods ----
#
#     # Active fields ----
#
#   ),
#   private = list(
#     # Private fields ----
#
#     # Private methods ----
#
#   )
# )

#' @title Class object for hexagon center
#' @name HexCubeCenter
#' @description This is a class that contains the Cube coordinates of the center of an Hexagon.
#' It inherits from `CubeCoords` but it is specific for `Hexagon`.
#' @export
HexCubeCenter = R6::R6Class(
  "HexCubeCenter",
  inherit=CubeCoords,
  public = list(
    # Public methods ----
    #' @description Initialise a new instance.
    #' @param x A numeric vector.
    #' @return An object of class `HexCubeCenter`.
    initialize = function(x){
      super$initialize(x)
    }
    # Public fields ----

  ),
  active = list(
    # Active fields ----

  ),
  private = list(
    # Private fields ----

    # Private methods ----

  )
)

# HexCubeCenter's Methods ----

hex_direction = function(direction){
  .HexDirection = list((c(1, 0, -1)),
                       (c(1, -1, 0)),
                       (c(0, -1, 1)),
                       (c(-1, 0, 1)),
                       (c(-1, 1, 0)),
                       (c(0, 1, -1)))
  HexCubeCenter$new(.HexDirection[[direction]])
}
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
hex_diagonal_neighbor = function(object, direction){
  UseMethod("hex_diagonal_neighbor")
}

#' @title Hexagon diagonal neighbour
#' @description `hex_diagonal_neighbour` method for class HexCubeCenter
#' @return An object of class `HexCubeCenter`.
hex_diagonal_neighbor.HexCubeCenter = function(hex, direction){
  add(hex, hex_diagonal_direction(direction))
}


