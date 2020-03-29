setwd("~/unlikelyvolcano.github.io")


# Counties ----------------------------------------------------------------

map <- readOGR("data/tl_2019_us_county", stringsAsFactors = F)
map_data <- map@data
rownames(map_data) <- sapply(map@polygons, function(x) x@ID)
map_simple <- gSimplify(map, tol = 0.01, topologyPreserve = TRUE)
map_new <- SpatialPolygonsDataFrame(map_simple, map_data)

saveRDS(map_new, "data/us_counties.RDS")

rm(map, map_data, map_simple, map_new)


# States ------------------------------------------------------------------

map <- readOGR("data/tl_2019_us_state", stringsAsFactors = F)
map_data <- map@data
rownames(map_data) <- sapply(map@polygons, function(x) x@ID)
map_simple <- gSimplify(map, tol = 0.0001, topologyPreserve = TRUE)
map_new <- SpatialPolygonsDataFrame(map_simple, map_data)

saveRDS(map_new, "data/us_states.RDS")

rm(map, map_data, map_simple, map_new)
