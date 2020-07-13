library(sf); library(dplyr)


# Read and manipulate flowline data ---
flowline <- st_read('data/raw/marshyhope_flowline.gpkg')


##  Merge different river sections into one.
flowline <- st_combine(flowline)
flowline <- st_line_merge(flowline)



# Read in and manipulate receiver locations ----
dets <- data.table::fread('data/derived/sturgeon_detections.gz')

recs <- dets %>%
  distinct(station, lat, long) %>%
  arrange(-lat) %>%
  st_as_sf(coords = c('long', 'lat'),
           crs = 4326)


# Reproject spatial objects ----
flowline <- st_transform(flowline, 32618)
recs <- st_transform(recs, 32618)



# Break the line that makes up the flowline into points 1 m apart from each other ----
flowline <- flowline %>%
  st_segmentize(1) %>%
  st_cast('MULTIPOINT')



# Find points on the line that are closest to receiver locations ----
pts <- st_nearest_points(flowline, recs)



# Cast flowline from MULTIPOINT into simplified LINESTRING ---
flowline <- st_cast(flowline, 'LINESTRING')



# # Split linestring into sections by the nearest points ----
# flowline_split <- lwgeom::st_split(flowline, pts) %>%
#   st_collection_extract('LINESTRING')
#
#
#
# # Visualize ----
# parts_all <- st_as_sf(
#   data.frame(
#     id = 1:length(flowline_split),
#     geometry = flowline_split
#   )
# )
#
# library(ggplot2)
# ggplot() +
#   geom_sf(aes(color = as.factor(id)), parts_all, size =5) +
#   geom_sf(data = recs) +
#   theme_bw()



# Make distance matrix ----
k <- recs
st_geometry(k) <- pts
m <- matrix(nrow = nrow(k), ncol = nrow(k))

key <- t(combn(seq(1, nrow(k), 1), 2))

for(i in 1:nrow(key)){
  l <- lwgeom::st_split(flowline, k[c(key[i, 1], key[i, 2]),])
  j <- st_collection_extract(l, 'LINESTRING')
  m[key[i, 1], key[i, 2]] <- st_length(j)[2]
}

diag(m) <- 0



n <- rbind(recs$station, m)
n <- cbind(c('from', recs$station), n)
n <- as.data.frame(n)
names(n) <- n[1,]
n <- n[-1,]

n <- tidyr::pivot_longer(n, -from, names_to = 'to', values_to = 'distance_m')
n$distance_m <- round(as.numeric(n$distance_m))
