# Hexagon class ----

Hexagon = R6::R6Class(
  "Hexagon",
  public = list(
    # Hexagon's public methods ----
    initialize = function(center, size, planar_origin=c(0,0), pointy_top=TRUE){
      assert_that(is_logical(pointy_top),
                  msg="'pointy_top' must be logical")
      private$.pointy_top = pointy_top
      assert_that(is.numeric(planar_origin),
                  msg="'planar_origin' must be numeric")
      assert_that(length(planar_origin)==2,
                  msg="'planar_origin' must be numeric of length 2")
      private$.planar_origin = planar_origin
      self$set_size(size)
      self$set_center(center)
      invisible(self)
    }
    ,is_pointy = function() private$.pointy_top
    ,set_center = function(x){
      private$.cube_center = HexCubeCenter$new(x)
      invisible(self)
    }
    ,get_center = function(){
      # return cube coords
      private$.cube_center
      # a geographical version of the hexagon should have methods for get_center_planar
      # and get_center_WGS
    }
    ,set_size = function(size){
      assert_that(is.numeric(size),
                  msg="'size' must be numeric")
      assert_that(length(size)==1,
                  msg="'size' must be numeric of length 1")
      private$.size = size
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
  ),
  private = list(
    # Hexagon's private fields ----
    .pointy_top = NULL
    ,.cube_center = NULL
    ,.size = NULL
    ,.planar_origin = NULL
    # Hexagon's private methods ----
  )
)

# PointyHexagon class ----

PointyHexagon = R6::R6Class(
  "PointyHexagon",
  inherit=Hexagon,
  public = list(
    # PointyHexagon's public methods ----
    initialize = function(center, size, planar_origin=c(0,0)){
      super$initialize(center, size, planar_origin, pointy_top=TRUE)
    }
    ,deep_clone = function(){
      cc = self$clone()
      cc$.__enclos_env__$private$.cube_center = cc$center$clone()
      return(cc)
    }
    ,v_coordinates = function(){
      center = self$get_planar_center()$value
      size = self$size
      angle_deg = private$.hex_angle_deg()
      angle_rad = pi / 180 * angle_deg
      return (matrix(c(center[[1]] + size * cos(angle_rad),
                       center[[2]] + size * sin(angle_rad)),
                     ncol=2))
    }
    ,get_planar_center = function(){
      center = self$get_center()
      cartesian_coords = Coords$
        new(c(private$.r_to_x(center$r),
              private$.q_to_y(center$q)))
      cartesian_coords
    }
    ,shift_horizontal = function(i, in_place=TRUE){
      if(in_place){
        self$center$set_r(self$center$r + i)
        invisible(self)
      }
      else {
        h = self$deep_clone()
        h$center$set_r(h$center$r + i)
        return(h)
      }
    }
    ,shift_left = function(i, in_place=TRUE){
      self$shift_horizontal(-i, in_place=in_place)
    }
    ,shift_right = function(i, in_place=TRUE){
      self$shift_horizontal(i, in_place=in_place)
    }
    ,shift_up = function(i){

    }
    ,shift_down = function(i){

    }
  ),
  active = list(
    # PointyHexagon's active fields ----
    width = function(x){
      if(missing(x)){
        sqrt(3) * self$size
      }
      else
        stop("Read-only field, use methods instead")
    }
    ,height = function(x){
      if(missing(x)){
        2 * self$size
      }
      else
        stop("Read-only field, use methods instead")
    }
    ,polygon = function(x){
      if(missing(x)){
        private$.hex_polygon()
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
    ,.q_to_y = function(q){
      angle_deg = 30
      angle_rad = pi / 180 * 30
      q * cos(angle_rad) * self$height + self$planar_origin[[2]]
    }
    ,.r_to_x = function(r){
      r * self$width + self$planar_origin[[1]]
    }
    ,.hex_to_cartesian = function(q, r){
      x = self$size * (sqrt(3) * q  +  sqrt(3)/2 * r)
      y = self$size * (3./2 * r) +
      return(Coords$new(c(x, y)))
    }
    ,.hex_polygon = function(){
      coords = self$v_coordinates()
      coords = rbind(coords, coords[1,])
      hex = sf::st_polygon(list(coords))
      attr(hex, "pointy") = TRUE
      attr(hex, "size") = self$size
      return(hex)
    }
  )
)


