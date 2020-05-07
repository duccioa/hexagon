# Hexagon
An `R` package to create hexagon grids on geographic coordinates.
It is based on the notation and implementation of [redblobgames](s://www.redblobgames.com/grids/hexagons) blocg.

The main function is the creation of geographical hex grids.

```{r}
library(hexagon)
g = hex_ring_grid(hexagon_radius=400, n=4,planar_origin=c(261354,6249876),
                  crs_input=3857, crs_output=4326, pointy_top=TRUE)
nm = generate_names(length(g))
sg = sf::st_sf(names=nm, geometry=g, crs = 4326)
mapview(sg, color="red", alhpa=0.1,
        alpha.regions=0.1, col.regions="yellow", 
        legend=FALSE)
```