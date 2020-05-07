# Hexagon class ----
#' @title Hexagon classes
#' @name Hexagon
#' @description Hexagons are the primitive unit to build hex grids. They can be pointy
#' or flat, depending on whether the top is a vertex or an edge.
#'
#' An hexagon is defined by its size and its position within the grid. A geographical
#' hexagon is also defined by its geographical position.
#'
#' Bear in mind the difference between the `center`,
#'  which is the center of the hexagon in the cube coordinate system, and the
#'  `planar_origin`, which is the origin of the cube coordinate system translated
#'  to the `crs_input` system.
#'
#' @section Geographical reference:
#'
#' In order to place the hexagon in place, a geographical origin and a size must be
#' specified. The geographical origin is assumed to be in planar coordinates,
#' the output can be any valid EPSG code.
#'
#' @param center A vector of cube coordinates or an object that can be coerced to
#' a `HexCubeCenter`.
#' @param size A number defining the radius of the circumscribed circle
#' of the hexagon, in the units of the planar_origin.
#' @param planar_origin A vector of geographical coordinates.
#' If `NULL`, it is set to 0,0.
#' @param pointy_top Logical.
#' @param crs_input A number. It determines the geographical reference
#' of the input.
#' @param crs_output A number. It determines the geographical reference
#' of the output.
#' @param in_place logical. If TRUE the instance is modifed by reference and
#' cloned otherwise.
#' @references \link{https://www.redblobgames.com/grids/hexagons}
Hexagon = R6::R6Class(
  "Hexagon",
  public = list(
    # Hexagon's public methods ----
    #' @description Initialise a new instance.
    #' @return An object of class `Hexagon`.
    initialize = function(center, size=NULL, planar_origin=NULL,
                          crs_input=NULL, crs_output=crs_input, pointy_top=TRUE){
      assert_that(is_logical(pointy_top),
                  msg="'pointy_top' must be logical")
      private$.pointy_top = pointy_top
      self$set_size(size, in_place=TRUE)
      self$set_center(center, in_place=TRUE)
      self$set_crs_input(crs_input)
      self$set_crs_output(crs_output)
      self$set_planar_origin(planar_origin, crs_input, in_place=TRUE)
      invisible(self)
    }
    #' @description Deep clone function.
    ,deep_clone = function(){
      cc = self$clone()
      cc$.__enclos_env__$private$.cube_center = cc$center$clone()
      return(cc)
    }
    #' @description Compute vertices' geographical coordinates in the
    #' planar `crs_input` system.
    #' @return A numeric matrix 6 by 2.
    ,v_planar_coordinates = function(){
      center = self$get_planar_center()$value
      size = self$size
      angle_deg = private$.hex_angle_deg()
      angle_rad = pi / 180 * angle_deg
      return (matrix(c(center[[1]] + size * cos(angle_rad),
                       center[[2]] + size * sin(angle_rad)),
                     ncol=2))
    }
    #' @description Whether the hexagon is pointy or flat.
    #' @return Logical.
    ,is_pointy = function() {private$.pointy_top}
    #' @description Compute the geographical coordinates of the hex
    #' center in the planar `crs_input` coordinate system.
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
    ,set_planar_origin = function(x, crs_input, in_place=TRUE){
      if(rlang::is_null(x)) x = c(0, 0, 0)
      if(missing(crs_input)) self$crs_input
      assert_that(is.numeric(x),
                  msg="'planar_origin' must be numeric")
      assert_that(length(x)==2,
                  msg="'planar_origin' must be numeric of length 2")
      if(in_place){
        private$.planar_origin = x
        self$set_crs_input(crs_input, in_place=TRUE)
        invisible(self)
      }
      else {
        cc = self$deep_clone()
        cc$set_planar_origin(x, in_place=TRUE)
        cc$set_crs_input(crs_input, in_place=TRUE)
        return(cc)
      }
    }
    #' @description Set the size of the hexagon. If `NULL` the size is set to 1.
    ,set_size = function(size=NULL, in_place=TRUE){
      if(in_place){
        if(rlang::is_null(size)) size = 1
        assert_that(is.numeric(size),
                    msg="'size' must be numeric")
        assert_that(length(size)==1,
                    msg="'size' must be numeric of length 1")
        private$.size = size
        invisible(self)
      }
      else{
        cc = self$deep_clone()
        cc$set_size(size, in_place=TRUE)
        return(cc)
      }
    }
    #' @description Set the coordinate reference system of the planar origin.
    #' Does not perform any transformation.
    ,set_crs_input = function(crs_input, in_place=TRUE){
      if(in_place){
        if(rlang::is_null(crs_input)) private$.crs_input = crs_input
        else{
          assert_that(is.numeric(crs_input),
                      msg="'crs_input' must be numeric")
          private$.crs_input = crs_input
        }
        invisible(self)
      }
      else{
        cc = self$deep_clone()
        cc$set_crs_input(crs_input, in_place=TRUE)
        return(cc)
      }

    }
    #' @description Set the coordinate reference system of the output. It is used to
    #' transform the the `crs_input` into any valid ESPG system via `sf::st_transform`.
    ,set_crs_output = function(crs_output, in_place=TRUE){
      if(in_place){
        if(rlang::is_null(crs_output)) crs_output = private$.crs_input
        assert_that(is.numeric(crs_output),
                    msg="'crs_input' must be numeric")
        private$.crs_output = crs_output
        invisible(self)
      }
      else{
        cc = self$deep_clone()
        cc$set_crs_output(crs_output, in_place=TRUE)
        return(cc)
      }
    }
    #' @description Compute the geographical
    #' representation of the polygon in `crs_output` reference system.
    #' @return An object of class `sfg` (see package `sf`).
    ,to_polygon = function(){
      coords = self$v_planar_coordinates()
      coords = rbind(coords, coords[1,])
      hex = sf::st_polygon(list(coords))
      attr(hex, "pointy") = self$is_pointy()
      attr(hex, "size") = self$size
      return(hex)
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
    #' @field size numeric. The cartesian distance between
    #' the center and any vertex of the hexagon.
    ,size = function(x){
      if(missing(x))
        private$.size
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
    #' @field crs_input CRS code of the planar origin.
    ,crs_input = function(x){
      if(missing(x))
        private$.crs_input
      else
        stop("Read-only value, use set methods instead")
    }
    #' @field crs_output CRS code of the polygon output.
    ,crs_output = function(x){
      if(missing(x))
        private$.crs_output
      else
        stop("Read-only value, use set methods instead")
    }
  ),
  private = list(
    # Hexagon's private fields ----
    .pointy_top = NULL
    ,.cube_center = NULL
    ,.size = NULL
    ,.planar_origin = NULL
    ,.crs_input = NULL
    ,.crs_output = NULL
    # Hexagon's private methods ----
  )
)

# PointyHexagon class ----
#' @title Pointy Hexagon class
#' @description A class `Hexagon` with a pointy top.
#' @param center A vector of cube coordinates or an object that can be coerced to
#' a `HexCubeCenter`.
#' @param size A number defining the radius of the circumscribed circle
#' of the hexagon, in the units of the planar_origin.
#' @param planar_origin A vector of geographical coordinates.
#' If `NULL`, it is set to 0,0.
#' @param crs_input A number. It determines the geographical reference
#' of the input.
#' @param crs_output A number. It determines the geographical reference
#' of the output.
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
    initialize = function(center, size=NULL, planar_origin=NULL,
                          crs_input=3857, crs_output=crs_input){
      super$initialize(center=center, size=size, planar_origin=planar_origin,
                       crs_input=crs_input, crs_output=crs_output, pointy_top=TRUE)
    }
    #' @description Shift the hex center of `i` places in the cube system,
    #' along the `q` axis.
    #' @param i the distance of the shift in number of cells.
    #' @return In place or a new `Hexagon`.
    ,shift_on_q = function(i, in_place=TRUE){
      cc = self$center$clone()
      cc$shift_on_q(i, in_place=TRUE)
      self$set_center(cc, in_place=in_place)
    }
    #' @description Shift the hex center of `i` places in the cube system,
    #' along the `r` axis.
    #' @param i the distance of the shift in number of cells.
    #' @return In place or a new `Hexagon`.
    ,shift_on_r = function(i, in_place=TRUE){
      cc = self$center$clone()
      cc$shift_on_r(i, in_place=TRUE)
      self$set_center(cc, in_place=in_place)
    }
    #' @description Shift the hex center of `i` places in the cube system,
    #' along the `s` axis.
    #' @param i the distance of the shift in number of cells.
    #' @return In place or a new `Hexagon`.
    ,shift_on_s = function(i, in_place=TRUE){
      cc = self$center$clone()
      cc$shift_on_s(i, in_place=TRUE)
      self$set_center(cc, in_place=in_place)
    }
    #' @description Shift along a direction.
    #' @param i the distance of the shift in number of cells.
    #' @param direction a number from 1 to 6, corresponding to a direction. 1
    #' in a pointy hexagon is to the right and the other are in sequence
    #' anti-clockwise.
    #' @return In place or a new `Hexagon`.
    ,shift_along_direction = function(i, direction, in_place=TRUE){
      cc = self$center$clone()
      cc_shifted = hex_neighbour(cc, direction) * i
      self$set_center(cc_shifted, in_place=in_place)
    }
    #' @description Shift along a diagonal direction.
    #' @param i the distance of the shift in number of cells.
    #' @param direction a number from 1 to 6, corresponding to a direction.
    #' @return In place or a new `Hexagon`.
    ,shift_along_diagonal_direction = function(i, direction,
                                               in_place=TRUE){
      cc = self$center$clone()
      cc_shifted = hex_diagonal_neighbour(cc, direction) * i
      self$set_center(cc_shifted, in_place=in_place)
    }
  ),
  active = list(
    # PointyHexagon's active fields ----
    #' @field width the distance between two edges of the hexagon in
    #' the units of `size`.
    width = function(x){
      if(missing(x)){
        sqrt(3) * self$size
      }
      else
        stop("Read-only field, use methods instead")
    }
    #' @field height the distance between two vertices of the hexagon in
    #' the units of `size`.
    ,height = function(x){
      if(missing(x)){
        2 * self$size
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
      x = self$size * (sqrt(3) * q  +  sqrt(3)/2 * r) + self$planar_origin[[1]]
      y = self$size * (3./2 * r) + self$planar_origin[[2]]
      return(Coords$new(c(x, y)))
    }
  )
)


