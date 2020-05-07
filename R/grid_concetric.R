#' Coordinates of a concentric hex grid
#'
#' Compute the coordinates of a concentric hex grid of `n` rings.
#'
#' @param n the number of rings around an origin hexagon. It determines the
#' extent of the grid.
#'
#' @return A list of cube coordinates.
.coords_grid_concetric = function(n){
  results = list()
  q_range = -n:n
  i = 1
  for(q in q_range){
    s_range = max(-n, -q-n):min(n, -q+n)
    tmp = list()
    j = 1
    for(s in s_range){
      r = -q-s
      tmp[[j]] =c(q, r, s)
      j = j + 1
    }
    results = c(results, tmp)
    i = i + 1
  }
  results
}

#' Cube centers of a concentric hex grid
#'
#' Instantiate the `HexCubeCenter` of a concentric hex grid of `n` rings.
#'
#' @param coords_list A list of vectors, each containing cube coordinates.
#'
#' @return A list of objects of class `HexCubeCenter`.
.hex_cube_center_v = function(coords_list){
  lapply(coords_list, HexCubeCenter$new)
}

#' @title Concentric Hex Grid
#' @description Create a concentric hex grid in the geographical space. Concetric
#' grids are built in concentric rings around an hexagon origin. The overall
#' shape resembles an hexagon and the size is roughly `4 * n * hexagon_radius`
#' @param hexagon_radius the radius of the hexagon's circumscribed circle.
#' @param n the number of rings around the origin hexagon.
#' @param identifier_name character, the name of the identifier column
#' of the output.
#' @param planar_origin A vector of geographical coordinates.
#' If `NULL`, it is set to 0,0.
#' @param pointy_top Logical.
#' @param crs_input A number. It determines the geographical reference
#' of the input.
#' @param crs_output A number. It determines the geographical reference
#' of the output.
#' @return An `sfg` object.
#' @references \link{https://www.redblobgames.com/grids/hexagons}
#' @export
hex_grid_concentric = function(hexagon_radius=100, n=5, planar_origin=NULL,
                              crs_input=3857, crs_output=4326, pointy_top=TRUE){
  cc = HexCubeCenter$new(c(0,0))
  coords_list = .coords_grid_concetric(n)
  center_list = .hex_cube_center_v(coords_list)
  center_list = c(cc, center_list)
  pol_list = lapply(center_list,
                    function(z) PointyHexagon$new(z, hexagon_radius, planar_origin)$to_polygon()
  )

}

