#' @name Coords
#' @title Coordinates class
#' @description A class to manage the basic functions of an n dimension coordinate system.
#' @param in_place logical. If TRUE the instance is modifed by reference and
#' cloned otherwise.
#' @references \link{https://www.redblobgames.com/grids/hexagons}
#' @export
Coords = R6::R6Class(
  "Coords",
  lock_objects = TRUE,
  lock_class = TRUE,
  public = list(
    # Public methods ----
    #' @description Initialise a new instance.
    #' @param x A numeric vector.
    #' @return An object of class `Coords`.
    initialize = function(x){
      x = private$._parse_input(x)
      self$set(x)
      invisible(self)
    }
    # Public fields ----
    #' @description Set the numeric value of the coordinates.
    #' @param x A numeric vector.
    ,set = function(x, in_place=TRUE){
      assert_that(is.numeric(x),
                  msg = "'x' must be numeric")
      if(in_place){
        private$.coords = x
        invisible(self)
      }
      else{
        cc = self$clone()
        cc$set(x, in_place=TRUE)
        return(cc)
      }
    }
    #' @description Get the numeric value of the coordinates.
    #' @param i integer, the index(es) to be retrieved. If missing, the full set of
    #' coordinates is returned.
    #' @return A numeric vector.
    ,get = function(i){
      if(missing(i))
        private$.coords
      else private$.coords[i]
    }
    #' @description Get the number of dimensions.
    #' @return A number.
    ,dim = function(){
      length(self$value)
    }
    #' @description `print` method for class `Coords`.
    ,print = function(){
      cat(sprintf("Class %s object\n", class(self)[1]))
      print(self$value)
      invisible(self)
    }
  ),
  active = list(
    # Active fields ----
    #' @field value Numeric, the value of the coordinates.
    value = function(x){
      if(missing(x))
        self$get()
      else self$set(x, in_place=TRUE)
    }
  ),
  private = list(
    # Private fields ----
    .coords = NULL
    # Private methods ----
    ,._parse_input = function(x){
      if(missing(x)) x = numeric()
      if(is_Coords(x)) x = x$value
      return(x)
    }
  )
)


# Methods ----

#' @describeIn Coords Subset by index as in any numeric vector x[i].
`[.Coords` = function(x, i) {
  x$value[[i]]
}

#' @describeIn Coords Number of dimension of the coordinate system.
dim.Coords = function(x) x$dim()


#' @describeIn Coords Sum `Coords` objects as in `A + B`.
`+.Coords` = function(a, b, na.rm=FALSE){
  assert_that(dim(a) == dim(a),
              msg="'a' and 'b' must have the same number of dimensions")
  assert_that(identical(class(a), class(b)),
              msg="'a' and 'b' must share the same class")
  x = a$value + b$value
  initiate_instance(x, what=class(a)[1])
}

#' @describeIn Coords Add two `Coords` objects with `add(A, B)`.
add.Coords = `+.Coords`

#' @describeIn Coords Subtract `Coords` objects as in `A - B`.
`-.Coords` = function(a, b, na.rm=FALSE){
  assert_that(a$dim() == b$dim(),
              msg="'a' and 'b' must have the same number of dimensions")
  assert_that(identical(class(a), class(b)),
              msg="'a' and 'b' must share the same class")
  x = a$value - b$value
  initiate_instance(x, what=class(a)[1])
}

#' @describeIn Coords Subtract two `Coords` objects with `subtract(A, B)`.
subtract.Coords = `-.Coords`


#' @describeIn Coords Scale a `Coords` object with `A * k`.
`*.Coords` = function(x, k){
  assert_that(is.numeric(k),
              msg="'k' must be numeric")
  v = x$value * k
  initiate_instance(v, what=class(x)[1])
}

#' @describeIn Coords Scale a `Coords` object with `scale(A, k)`.
scale.Coords = `*.Coords`




