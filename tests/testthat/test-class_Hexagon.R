load_all("./")

ph = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
ph$center
cc = ph$get_planar_center()
m = ph$v_coordinates()
plot(m[,1], m[, 2])
pol = ph$polygon
plot(pol)

ph1 = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
ph2 = PointyHexagon$new(c(0,1), size=200, planar_origin=c(260464,6246874))
pol1 = ph1$polygon
pol2 = ph2$polygon
sfc_pol = sf::st_sfc(pol1, pol2)
plot(sfc_pol)

load_all()
ph1 = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
ph2 = ph1$shift_horizontal(1, in_place=FALSE)
ph3 = ph1$shift_horizontal(2, in_place=FALSE)
pol1 = ph1$polygon
pol2 = ph2$polygon
pol3 = ph3$polygon
sfc_pol = sf::st_sfc(pol1, pol2, pol3)
plot(sfc_pol)

ph1 = PointyHexagon$new(c(0,0), size=200, planar_origin=c(260464,6246874))
ph2 = PointyHexagon$new(c(0,-1), size=200, planar_origin=c(260464,6246874))
pol1 = ph1$polygon
pol2 = ph2$polygon
sfc_pol = sf::st_sfc(pol1, pol2)
plot(sfc_pol)
