load_all("./")
# HexCubeCenter$debug("initialize")
h1 = HexCubeCenter$new(c(0,0,0))
h2 = HexCubeCenter$new(c(3,5))
h1 + h2
h3 = hex_direction(1)

hex_neighbour(h1, 1)
hex_diagonal_neighbor(h1, 1)
