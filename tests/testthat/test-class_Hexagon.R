load_all("./")


# PointyHexagon$debug('initialize')
ph = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
ph$center
cc = ph$get_planar_center()
m = ph$v_planar_coordinates()
plot(m[,1], m[, 2])
pol = ph$to_polygon()
plot(pol)

ph1 = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
ph2 = PointyHexagon$new(c(0,1), size=200, planar_origin=c(260464,6246874))
pol1 = ph1$to_polygon()
pol2 = ph2$to_polygon()
sfc_pol = sf::st_sfc(pol1, pol2)
plot(sfc_pol)

load_all()
ph1 = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
ph2 = ph1$shift_on_q(1, in_place=FALSE)
ph3 = ph1$shift_on_r(1, in_place=FALSE)
ph4 = ph1$shift_on_s(1, in_place=FALSE)
pol1 = ph1$to_polygon()
pol2 = ph2$to_polygon()
pol3 = ph3$to_polygon()
pol4 = ph4$to_polygon()
sfc_pol = sf::st_sfc(pol1, pol2, pol3, pol4)
plot(sfc_pol)
s = sf::st_as_sf(data.table(id=seq_along(sfc_pol), geometry=sfc_pol))
s = sf::st_set_crs(s, 3857)
mapview::mapview(s)

load_all()
ph1 = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
l = list()
for(i in 1:6)
  l[[i]] = ph1$shift_along_direction(1, i, in_place=FALSE)$to_polygon()
sfc_pol = sf::st_sfc(l)
plot(sfc_pol)
s = sf::st_as_sf(data.table(id=seq_along(sfc_pol), geometry=sfc_pol))
s = sf::st_set_crs(s, 3857)
mapview::mapview(s)

ph1 = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
l = list()
for(i in 1:6)
  l[[i]] = ph1$shift_along_direction(2, i, in_place=FALSE)$to_polygon()
sfc_pol = sf::st_sfc(l)
plot(sfc_pol)
s = sf::st_as_sf(data.table(id=seq_along(sfc_pol), geometry=sfc_pol))
s = sf::st_set_crs(s, 3857)
mapview::mapview(s)

ph1 = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
l = list()
for(i in 1:6)
  l[[i]] = ph1$shift_along_diagonal_direction(1, i, in_place=FALSE)$to_polygon()
sfc_pol = sf::st_sfc(l)
plot(sfc_pol)
s = sf::st_as_sf(data.table(id=seq_along(sfc_pol), geometry=sfc_pol))
s = sf::st_set_crs(s, 3857)
mapview::mapview(s)

load_all()
ph1 = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
directions = 1:6
steps = 1:5

l = list(ph1$to_polygon())
for(j in steps){
  l_tmp = list()
  for(i in 1:6)
    l_tmp[[i]] = ph1$shift_along_direction(j, i, in_place=FALSE)$to_polygon()
  l = c(l, l_tmp)
}

for(j in steps){
  l_tmp = list()
  for(i in 1:6)
    l_tmp[[i]] = ph1$shift_along_diagonal_direction(j, i, in_place=FALSE)$to_polygon()
  l = c(l, l_tmp)
}

sfc_pol = sf::st_sfc(c(l))
plot(sfc_pol)
s = sf::st_as_sf(data.table(id=seq_along(sfc_pol), geometry=sfc_pol))
s = sf::st_set_crs(s, 3857)
mapview::mapview(s)

