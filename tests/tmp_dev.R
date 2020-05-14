# From https://www.redblobgames.com/grids/hexagons/
library(sf)
library(rlang)
library(data.table)
dt = data.table(id=1, geometry="POLYGON((0 0, 0 1, 1 1, 0 0))")
pol = sf::st_as_sf(dt, wkt='geometry')
# hex_pointy_v = function(center, size, polygon=FALSE){
#   if(is_true(polygon)) i = 1:7
#   else i = 1:6
#   angle_deg = 60 * i - 30
#   angle_rad = pi / 180 * angle_deg
#   return (matrix(c(center[[1]] + size * cos(angle_rad),
#                    center[[2]] + size * sin(angle_rad)),
#                  ncol=2)
#   )
# }
# hex_flat_v = function(center, size, polygon=FALSE){
#   i = 1:6
#   angle_deg = 60 * i
#   angle_rad = pi / 180 * angle_deg
#   return (matrix(c(center[[1]] + size * cos(angle_rad),
#                    center[[2]] + size * sin(angle_rad)),
#                  ncol=2)
#   )
# }
hex_angle_deg_flat = function(){
  i = 1:6
  angle_deg = 60 * i
  angle_deg
}
hex_angle_deg_pointy = function(){
  i = 1:6
  angle_deg = 60 * i - 30
  angle_deg
}
hex_coordinates = function(center, size, angle_deg){
  angle_rad = pi / 180 * angle_deg
  return (matrix(c(center[[1]] + size * cos(angle_rad),
                   center[[2]] + size * sin(angle_rad)),
                 ncol=2))
}
hex_coords_flat = function(center=c(0,0), size=1){
  angle_deg = hex_angle_deg_flat()
  hex_coordinates(center, size, angle_deg)
}
hex_coords_pointy = function(center=c(0,0), size=1){
  angle_deg = hex_angle_deg_pointy()
  hex_coordinates(center, size, angle_deg)
}

hex_polygon = function(center=c(0,0), size=1, pointy=TRUE){
  if(is_true(pointy)) hex_coords = hex_coords_pointy
  else hex_coords = hex_coords_flat
  coords = hex_coords(center, size)
  coords = rbind(coords, coords[1,])
  hex = sf::st_polygon(list(coords))
  attr(hex, "pointy") = pointy
  attr(hex, "size") = size
  return(hex)
}

hex_polygon_v = function(centers, size=1, pointy=TRUE){
  l = apply(centers, 1, function(z)
    hex_polygon(z, size=size, pointy=pointy)
  )
  sf::st_sfc(l)
}

h = hex_polygon_v(centers, size=0.5)
plot(h)
h = hex_polygon_v(centers, size=0.5, pointy=FALSE)
plot(h)
# centers must be a matrix of equally spaced points
hex_tiles = function(size=1, origin=c(0,0), n=10, pointy=TRUE, n_x=NULL, n_y=NULL){
  square = FALSE
  if(any(c(is_null(n_x), is_null(n_y)))){
      n_x = n_y = n
      square = TRUE
    }
  if(is_true(pointy)){
    w = sqrt(3) * size
    h = 2 * size
    h_spacing = w
    v_spacing = h * 3/4
    
    fixed_spacing = v_spacing
    shifting_spacing = h_spacing
    fixed_n = ifelse(is_true(square), round(n_y*1.2), n_y)
    shifting_n = n_x
    
    fixed_coord = origin[[2]] # centers_y1
    fixed_coord = c(fixed_coord, cumsum(rep(fixed_spacing, fixed_n-1)))
    shifting_coord1_0 = origin[[1]] # centers_x1
    shifting_coord1 = c(shifting_coord1_0, cumsum(rep(shifting_spacing, shifting_n-1)))
    shifting_coord2 = c(shifting_coord1 - shifting_spacing/2, 
                        max(shifting_coord1) + shifting_spacing/2) # centers_x2
    
    centers = matrix(numeric(), ncol=2)
    for(i in seq_along(fixed_coord)){
      if(floor(i/2)==i/2) 
        m = matrix(c(shifting_coord2,
                     rep(fixed_coord[[i]], length(shifting_coord2))),
                   ncol=2
        )
      else 
        m = matrix(c(shifting_coord1,
                     rep(fixed_coord[[i]], length(shifting_coord1))),
                   ncol=2
        )
      centers = rbind(centers, m)
    }
  }
  else {
    w = 2 * size
    h = sqrt(3) * size
    h_spacing = w * 3/4
    v_spacing = h
    
    fixed_spacing = h_spacing
    shifting_spacing = v_spacing
    fixed_n = ifelse(is_true(square), round(n_x*1.2), n_x)
    shifting_n = n_y
    
    fixed_coord = origin[[1]] # centers_y1
    fixed_coord = c(fixed_coord, cumsum(rep(fixed_spacing, fixed_n-1)))
    shifting_coord1_0 = origin[[2]] # centers_x1
    shifting_coord1 = c(shifting_coord1_0, cumsum(rep(shifting_spacing, shifting_n-1)))
    shifting_coord2 = c(shifting_coord1 - shifting_spacing/2, 
                        max(shifting_coord1) + shifting_spacing/2) # centers_x2
    
    centers = matrix(numeric(), ncol=2)
    for(i in seq_along(fixed_coord)){
      if(floor(i/2)==i/2) 
        m = matrix(c(
          rep(fixed_coord[[i]], length(shifting_coord2)),
          shifting_coord2
        ),
        ncol=2
        )
      else 
        m = matrix(c(
          rep(fixed_coord[[i]], length(shifting_coord1)),
          shifting_coord1
        ),
        ncol=2
        )
      centers = rbind(centers, m)
    }
  }
  
  
  
  h = hex_polygon_v(centers, size=size, pointy=pointy)
  return(h)
}
h = hex_tiles(n=6)
plot(h)
h = hex_tiles(n=6, pointy=F)
plot(h)
h = hex_tiles(size=2, 
              origin=c(0,0), 
              n_x=24, 
              n_y=5, 
              pointy=FALSE)
plot(h)
