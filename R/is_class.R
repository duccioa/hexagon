#' Set of functions that checks whether an object inherits from a certain class.
#' @param x an R object.
#' @usage NULL
#' @return Logical
#' @rdname is_class
#' @name is_<class>
#' @title Does it inherits from a class?
NULL

#' @describeIn is_class TRUE if x inherits from class `HexCenter`.
#' @export
is_HexCenter = function(x){
  inherits(x, "HexCenter")
}
#' @describeIn is_class TRUE if x inherits from class `Coords`.
#' @export
is_Coords = function(x){
  inherits(x, "Coords")
}
#' @describeIn is_class TRUE if x inherits from class `CubeCoords`.
#' @export
is_CubeCoords = function(x){
  inherits(x, "CubeCoords")
}
#' @describeIn is_class TRUE if x inherits from class `Hexagon`.
#' @export
is_Hexagon = function(x){
  inherits(x, "Hexagon")
}
#' @describeIn is_class TRUE if x inherits from class `PointyHexagon`.
#' @export
is_PointyHexagon = function(x){
  inherits(x, "PointyHexagon")
}
#' @describeIn is_class TRUE if x inherits from class `FlatHexagon`.
#' @export
is_PointyHexagon = function(x){
  inherits(x, "FlatHexagon")
}