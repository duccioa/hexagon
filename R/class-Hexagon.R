# Hexagon class ----
#' @title Hexagon classes
#' @name Hexagon
#' @description Hexagons are the primitive unit to build hex grids. They can be pointy
#' or flat, depending on whether the top is a vertex or an edge.
#'
#' An hexagon is defined by its radius and its position within the grid. A geographical
#' hexagon is also defined by its geographical position.
#'
#' @section Geographical reference:
#'
#' In order to place the hexagon in place, a geographical origin must be
#' specified. The geographical origin is assumed to be in planar coordinates.
#'
#' @param center A vector of cube coordinates or an object that can be coerced to
#' a `HexCubeCenter`.
#' @param radius A number defining the radius of the circumscribed circle
#' of the hexagon, in the units of the planar_origin.
#' @param planar_origin A vector of geographical coordinates.
#' If `NULL`, it is set to 0,0.
#' @param pointy_top Logical.
#' @param in_place logical. If TRUE the instance is modifed by reference and
#' cloned otherwise.
#' @references \link{https://www.redblobgames.com/grids/hexagons}
Hexagon = R6::R6Class(
  "Hexagon",
  public = list(
    # Hexagon's public methods ----
    #' @description Initialise a new instance.
    #' @return An object of class `Hexagon`.
    initialize = function(center, radius=NULL, planar_origin=NULL,
                          pointy_top=TRUE){
      assert_that(is_logical(pointy_top),
                  msg="'pointy_top' must be logical")
      private$.pointy_top = pointy_top
      self$set_radius(radius, in_place=TRUE)
      self$set_center(center, in_place=TRUE)
      self$set_planar_origin(planar_origin, in_place=TRUE)
      invisible(self)
    }
    #' @description Deep clone function.
    ,deep_clone = function(){
      cc = self$clone()
      cc$.__enclos_env__$private$.cube_center = cc$center$clone()
      return(cc)
    }
    #' @description Compute vertices' geographical coordinates in
    #' planar coordinates.
    #' @return A numeric matrix 6 by 2.
    ,v_planar_coordinates = function(){
      center = self$get_planar_center()$value
      radius = self$radius
      angle_deg = private$.hex_angle_deg()
      angle_rad = pi / 180 * angle_deg
      return (matrix(c(center[[1]] + radius * cos(angle_rad),
                       center[[2]] + radius * sin(angle_rad)),
                     ncol=2))
    }
    #' @description Whether the hexagon is pointy or flat.
    #' @return Logical.
    ,is_pointy = function() {private$.pointy_top}
    #' @description Compute the geographical coordinates of the hex
    #' center in planar coordinate system.
    #' @return An object of class `Coords`.
    ,get_planar_center = function(){
      center = self$center
      cartesian_coords = Coords$
        new(private$.hex_to_cartesian(center$q, center$r))
      cartesian_coords
    }
    #' @description Set the center of the hexagon.
    #' @param x A set of cube coordinates or something that can be coerced to it.
    ,set_center = function(x, in_place=TRUE){
      if(in_place){
        private$.cube_center = HexCubeCenter$new(x)
        invisible(self)
      }
      else {
        cc = self$deep_clone()
        cc$set_center(x, in_place=TRUE)
        return(cc)
      }
    }
    #' @description Set the the planar origin.
    #' @param x Geographical coordinates.
    ,set_planar_origin = function(x, in_place=TRUE){
      if(rlang::is_null(x)) x = c(0, 0, 0)
      assert_that(is.numeric(x),
                  msg="'planar_origin' must be numeric")
      assert_that(length(x)==2,
                  msg="'planar_origin' must be numeric of length 2")
      if(in_place){
        private$.planar_origin = x
        invisible(self)
      }
      else {
        cc = self$deep_clone()
        cc$set_planar_origin(x, in_place=TRUE)
        return(cc)
      }
    }
    #' @description Set the radius of the hexagon. If `NULL` the radius is set to 1.
    ,set_radius = function(radius=NULL, in_place=TRUE){
      if(in_place){
        if(rlang::is_null(radius)) radius = 1
        assert_that(is.numeric(radius),
                    msg="'radius' must be numeric")
        assert_that(length(radius)==1,
                    msg="'radius' must be numeric of length 1")
        private$.radius = radius
        invisible(self)
      }
      else{
        cc = self$deep_clone()
        cc$set_radius(radius, in_place=TRUE)
        return(cc)
      }
    }
    #' @description Compute the geographical
    #' representation of the polygon in planar coordinates.
    #' @return An object of class `XY, POLYGON, sfg` (see package `sf`).
    ,to_polygon = function(){
      coords = self$v_planar_coordinates()
      coords = rbind(coords, coords[1,])
      hex = sf::st_polygon(list(coords))
      attr(hex, "pointy") = self$is_pointy()
      attr(hex, "radius") = self$radius
      return(hex)
    }
    #' @description Print method for class `Hexagon`.
    ,print = function(){
      l = list(
        sprintf("Object of class %s", class(self)[1])
        ,sprintf("radius: %s", self$radius)
        ,sprintf("Pointy top: %s", self$is_pointy())
        ,sprintf("Planar origin: [%s]", paste(self$planar_origin,
                                              collapse=","))
        ,sprintf("Center: \n[%s]", paste(self$center$value,
                                         collapse=","))
      )
      cat(do.call(paste, list(l, collapse="\n")))
      invisible(self)
    }
  ),
  active = list(
    # Hexagon's active fields ----
    #' @field center `CubeCoords` object.
    center = function(x){
      if(missing(x))
        private$.cube_center
      else
        stop("Read-only value, use set methods instead")
    }
    #' @field radius numeric. The cartesian distance between
    #' the center and any vertex of the hexagon.
    ,radius = function(x){
      if(missing(x))
        private$.radius
      else
        stop("Read-only value, use set methods instead")
    }
    #' @field planar_origin numeric of length 2. The planar coordinates of the origin.
    ,planar_origin = function(x){
      if(missing(x))
        private$.planar_origin
      else
        stop("Read-only value, use set methods instead")
    }
  ),
  private = list(
    # Hexagon's private fields ----
    .pointy_top = NULL
    ,.cube_center = NULL
    ,.radius = NULL
    ,.planar_origin = NULL
    # Hexagon's private methods ----
  )
)

# PointyHexagon class ----
#' @title Pointy Hexagon class
#' @description A class `Hexagon` with a pointy top.
#' @param center A vector of cube coordinates or an object that can be coerced to
#' a `HexCubeCenter`.
#' @param radius A number defining the radius of the circumscribed circle
#' of the hexagon, in the units of the planar_origin.
#' @param planar_origin A vector of geographical coordinates.
#' If `NULL`, it is set to 0,0.
#' @param in_place logical. If TRUE the instance is modifed by reference and
#' cloned otherwise.
#' @export
PointyHexagon = R6::R6Class(
  "PointyHexagon",
  inherit=Hexagon,
  public = list(
    # PointyHexagon's public methods ----
    #' @description Initialise a new instance.
    #' @return An object of class `PointyHexagon`.
    initialize = function(center, radius=NULL, planar_origin=NULL){
      super$initialize(center=center, radius=radius,
                       planar_origin=planar_origin, pointy_top=TRUE)
    }
    #' @description Shift the hex center of `i` places in the cube system,
    #' along the `q` axis.
    #' @param i the distance of the shift in number of cells.
    #' @return In place or a new `Hexagon`.
    ,shift_along_q = function(i=1, in_place=TRUE){
      cc = self$center$clone()
      cc$shift_along_q(i, in_place=TRUE)
      self$set_center(cc, in_place=in_place)
    }
    #' @description Shift the hex center of `i` places in the cube system,
    #' along the `r` axis.
    #' @param i the distance of the shift in number of cells.
    #' @return In place or a new `Hexagon`.
    ,shift_along_r = function(i, in_place=TRUE){
      cc = self$center$clone()
      cc$shift_along_r(i=1, in_place=TRUE)
      self$set_center(cc, in_place=in_place)
    }
    #' @description Shift the hex center of `i` places in the cube system,
    #' along the `s` axis.
    #' @param i the distance of the shift in number of cells.
    #' @return In place or a new `Hexagon`.
    ,shift_along_s = function(i=1, in_place=TRUE){
      cc = self$center$clone()
      cc$shift_along_s(i, in_place=TRUE)
      self$set_center(cc, in_place=in_place)
    }
    #' @description Shift along a direction.
    #' @param i the distance of the shift in number of cells.
    #' @param direction a number from 1 to 6, corresponding to a direction. 1
    #' in a pointy hexagon is to the right and the other are in sequence
    #' anti-clockwise.
    #' @return In place or a new `Hexagon`.
    ,shift_along_direction = function(i=1, direction, in_place=TRUE){
      if(missing(direction))
        stop("[shift_along_direction]: 'direction' missing with no default")
      cc = self$center$clone()
      cc_shifted = hex_neighbour(cc, direction) * i
      self$set_center(cc_shifted, in_place=in_place)
    }
    #' @description Shift along a diagonal direction.
    #' @param i the distance of the shift in number of cells.
    #' @param direction a number from 1 to 6, corresponding to a direction.
    #' @return In place or a new `Hexagon`.
    ,shift_along_diagonal_direction = function(i=1, direction,
                                               in_place=TRUE){
      if(missing(direction))
        stop("[shift_along_diagonal_direction]: 'direction' missing with no default")
      cc = self$center$clone()
      cc_shifted = hex_diagonal_neighbour(cc, direction) * i
      self$set_center(cc_shifted, in_place=in_place)
    }
  ),
  active = list(
    # PointyHexagon's active fields ----
    #' @field width the distance between two edges of the hexagon in
    #' the units of `radius`.
    width = function(x){
      if(missing(x)){
        sqrt(3) * self$radius
      }
      else
        stop("Read-only field, use methods instead")
    }
    #' @field height the distance between two vertices of the hexagon in
    #' the units of `radius`.
    ,height = function(x){
      if(missing(x)){
        2 * self$radius
      }
      else
        stop("Read-only field, use methods instead")
    }
  ),
  private = list(
    # PointyHexagon's private fields ----

    # PointyHexagon's private methods ----
    .hex_angle_deg = function(){
      i = 1:6
      angle_deg = 60 * i - 30
      angle_deg
    }
    ,.hex_to_cartesian = function(q, r){
      x = self$radius * (sqrt(3) * q  +  sqrt(3)/2 * r) + self$planar_origin[[1]]
      y = self$radius * (3./2 * r) + self$planar_origin[[2]]
      return(Coords$new(c(x, y)))
    }
  )
)


