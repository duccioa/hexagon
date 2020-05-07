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

#' @title Initiate instance
#' @description Programatically initiate an R6 class.
#' @param ... Named arguments of the `$new` method of the instance to be
#' instantiated.
#' @param what character, the name of the class to be instantiated.
#' @return An object of class as specified in `what`.
initiate_instance = function(..., what){
  Gen = get(what)
  assert_that(inherits(Gen, 'R6ClassGenerator'),
              msg="'what' must be a string with the name of an R6ClassGenerator")
  Gen$new(...)
}

#' @title Random names for your hexagons
#' @description Generate random couples of adjectives and nouns. It can be used
#' to generate names for your hexagon in a grid.
#' @param n How many names to generate
#' @export
generate_names = function(n){
  assert_that(is.numeric(n),
              msg="'n' must be an integer")
  n = round(n)
  rnd_names = sample(random_names, n, replace=n>length(random_names))
  rnd_adj = sample(random_adjectives, n, replace=n>length(random_adjectives))
  paste(rnd_adj, rnd_names, sep="_")
}