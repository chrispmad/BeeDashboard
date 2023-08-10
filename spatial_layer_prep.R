library(sf)
library(bcmaps)
library(rmapshaper)
library(bcdata)

# 1. natural resource layers

nr_districts = bcmaps::nr_districts()
nr_districts_l = rmapshaper::ms_simplify(nr_districts)
write_sf(nr_districts_l, 'www/nr_districts_simplified.gpkg')

nr_regions = bcmaps::nr_regions()
nr_regions_l = rmapshaper::ms_simplify(bcmaps::nr_regions())
write_sf(nr_regions_l, 'www/nr_regions_simplified.gpkg')

# Add in queries for species data that is relevant to bees...
# Maybe check in with Jenny first to see which species these could
# be.
bcdc_query_geodata()