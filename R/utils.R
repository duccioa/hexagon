#' @title A vectorised switch function
#' @name switchv
#' @description switchv evaluates a vector of EXPR and accordingly chooses one of the
#' further arguments (in ...).
#' @inheritParams base::switch
#' @return A vector of elements of `...`.
switchv = function(EXPR, ...) {
  EXPR = as.list(EXPR)
  sapply(EXPR, function(z) switch(z, ...))
  }


initiate_instance = function(..., what){
  Gen = get(what)
  assert_that(inherits(Gen, 'R6ClassGenerator'),
              msg="'what' must be a string with the name of an R6ClassGenerator")
  Gen$new(...)
}