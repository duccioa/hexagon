# CubeCoords class ----
#' @title Cube coordinates class
#' @description A class `Coords`of cube coordinates. Cube coordinates are specific
#' for hexagon and are based on three dimensions with the constrain `q + r + s = 0`.
#' The implementation and the notation of this package follow the exaplanation given
#' in the blog [redblobgames](https://www.redblobgames.com/grids/hexagons).
#' To be noted that in the theory page, the notation is `x, y, z` and in the
#' implementation is `q, r, s` with x -> q, z -> r and y -> s. This package follows
#' the latter notation.
#' @param x Numeric coordinates of length 3.
#' @param in_place logical. If TRUE the instance is modifed by reference and
#' cloned otherwise.
#' @references \link{https://www.redblobgames.com/grids/hexagons}
#' @export
CubeCoords = R6::R6Class(
  "CubeCoords",
  inherit=Coords,
  public = list(
    # Public methods ----
    #' @description Initialise a new instance.
    #' @return An object of class `CubeCoords`.
    initialize = function(x){
      x = private$._parse_input(x)
      super$initialize(x)
      invisible(self)
    }
    # Public fields ----
    #' @description Set the coordinates value.
    #' @return Invisible self.
    ,set = function(x, in_place=TRUE){
      if(in_place){
        private$.coords = private$._parse_input(x)
        invisible(self)
      }
      else{
        cc = self$clone()
        cc$set(x)
        return(cc)
      }
    }
    #' @description Set the 'q' coordinate value.
    #' @param q Numeric coordinate of length 1.
    #' @return Invisible self.
    ,set_q = function(q, in_place=TRUE){
      assert_that(is.numeric(q),
                  msg="'q' must be numeric")
      assert_that(length(q)==1,
                  msg="'q' must be numeric of length 1")
      coords = self$value
      coords[1] = q
      coords[3] = sum(-coords[-3])
      self$set(coords, in_place=in_place)
    }
    #' @description Set the 'r' coordinate value.
    #' @param r Numeric coordinate of length 1.
    #' @return Invisible self.
    ,set_r = function(r, in_place=TRUE){
      assert_that(is.numeric(r),
                  msg="'r' must be numeric")
      assert_that(length(r)==1,
                  msg="'r' must be numeric of length 1")
      coords = self$value
      coords[2] = r
      coords[3] = sum(-coords[-3])
      self$set(coords, in_place=in_place)
    }
    #' @description Get coodinate values.
    #' @param y The index of the dimension to be retrieved or
    #' the name as a string 'q', 'r' or 's'.
    #' @return A numeric value. As many as `NA_real_` as there are indexes
    #' out of boundaries.
    ,get = function(y){
      if(missing(y)) out = private$.coords
      else {if(is.character(y)){
        i = switchv(y,
                    'q'=1,
                    'r'=2,
                    's'=3)
        if(is_list(i)) do.call(c, i)
        self$get(i)
      }
        else if(is.numeric(y)){
          i = y
        }
        else stop("'y' must be numeric or character")
        out = super$get(i)
      }
      return(out)
    }
    #' @description Shift `i` places along the `q` axis.
    #' @param i An integer, indicating how many places to shift. The sign determines
    #' the direction along the axis.
    ,shift_along_q = function(i=1, in_place=TRUE){
      self$set_q(self$q + i, in_place=in_place)
    }
    #' @description Shift `i` places along the `r` axis.
    #' @param i An integer, indicating how many places to shift. The sign determines
    #' the direction along the axis.
    ,shift_along_r = function(i=1, in_place=TRUE){
      self$set_r(self$r + i, in_place=in_place)
    }
    #' @description Shift `i` places along the `s` axis.
    #' @param i An integer, indicating how many places to shift. The sign determines
    #' the direction along the axis.
    ,shift_along_s = function(i=1, in_place=TRUE){
      new_q = self$q + i
      new_r = self$r + i
      self$set(c(new_q, new_r), in_place=in_place)
    }
  ),
  active = list(
    # Active fields ----
    #' @field q numeric, the 'q' dimension.
    q = function(x){
      if(missing(x))
        self$value[1]
      else
        stop("Read-only value, use set methods instead")
    }
    #' @field r numeric, the 'r' dimension.
    ,r = function(x){
      if(missing(x))
        self$value[2]
      else
        stop("Read-only value, use set methods instead")
    }
    #' @field s numeric, the 's' dimension.
    ,s = function(x){
      if(missing(x))
        self$value[3]
      else
        stop("Read-only value, use set methods instead")
    }
  ),
  private = list(
    # Private fields ----

    # Private methods ----
    ._parse_input = function(x){
      if(missing(x)) x = numeric()
      else if(is_Coords(x)) x = x$value
      assert_that(is.numeric(x),
                  msg="'x' must be numeric")
      assert_that(length(x) %in% 2:3,
                  msg="'x' must be length 2 or 3")
      if(length(x)==2) x = c(x, -sum(x))
      assert_that(sum(x) == 0,
                  msg="sum(x) must be 0")
      x
    }
  )
)

# Methods ----
#' @describeIn CubeCoords Rotate left method for a cube center.
rotate_left = function(object) {
  UseMethod("rotate_left")
}
#' @describeIn CubeCoords Rotate right method for a cube center.
rotate_right = function(object) {
  UseMethod("rotate_right")
}

rotate_left.CubeCoords = function(a){
  CubeCoords$new(c(-a$s, -a$q, -a$r))
}

rotate_right.CubeCoords = function(a){
  CubeCoords$new(c(-a$r, -a$s, -a$q))
}