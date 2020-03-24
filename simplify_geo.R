setwd("~/unlikelyvolcano.github.io")

map <- readOGR("tl_2019_us_county", stringsAsFactors = F)
map_data <- map@data
rownames(map_data) <- sapply(map@polygons, function(x) x@ID)
map_simple <- gSimplify(map, tol = 0.01, topologyPreserve = TRUE)
map_new <- SpatialPolygonsDataFrame(map_simple, map_data)

saveRDS(map_new, "smallercounties.RDS")

rm(map, map_data, map_simple, map_new)
