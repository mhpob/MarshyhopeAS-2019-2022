library(sf); library(data.table)


# Read and manipulate flowline data ---
flowline <- st_read('data/raw/marshyhope_flowline.gpkg')


##  Merge different river sections into one.
flowline <- st_combine(flowline)
flowline <- st_line_merge(flowline)



# Read in and manipulate receiver locations ----
dets <- fread('data/derived/sturgeon_detections.gz')
# dets <- dets[date.local >= '2017-01-01'& date.local <= '2017-12-31']
recs <- unique(dets, by = c('long', 'lat'))
recs <- st_as_sf(recs, coords = c('long', 'lat'), crs = st_crs(flowline))


# Reproject spatial objects ----
flowline <- st_transform(flowline, 32618)
recs <- st_transform(recs, 32618)



# Break the line that makes up the flowline into points 1 m apart from each other ----
flowline <- st_segmentize(flowline, 1)
flowline <- st_cast(flowline, 'MULTIPOINT')



# Find points on the line that are closest to receiver locations ----
pts <- st_nearest_points(flowline, recs)



# Cast flowline from MULTIPOINT into simplified LINESTRING ---
flowline <- st_cast(flowline, 'LINESTRING')
flowline <- st_simplify(flowline)



# Split linestring into sections by the nearest points ----
flowline_split <- st_collection_extract(lwgeom::st_split(flowline, pts), 'LINESTRING')



# Visualize ----
parts_all <- st_as_sf(
  data.frame(
    id = 1:length(flowline_split),
    geometry = flowline_split
  )
)

library(ggplot2)
ggplot() +geom_sf(aes(color = as.factor(id)), parts_all, size =5) + geom_sf(data = recs) +
  theme_bw()

