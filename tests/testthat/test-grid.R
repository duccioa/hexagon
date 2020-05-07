load_all()
gg = hex_ring_grid(hexagon_radius=500, n=5, planar_origin=c(260464,6246874))
s = sf::st_as_sf(data.table(id = seq_along(gg), geometry = gg))
mapview::mapview(s)
