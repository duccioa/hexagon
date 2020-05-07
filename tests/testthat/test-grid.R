load_all()
gg = hex_grid_concetric(hexagon_size=500, rings=5, planar_origin=c(260464,6246874))
s = sf::st_as_sf(data.table(id = seq_along(gg), geometry = gg), crs=3857)
mapview::mapview(s)
