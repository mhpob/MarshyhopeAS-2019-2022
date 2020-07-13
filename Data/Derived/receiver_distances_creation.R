library(tidyr); library(dplyr); library(sf)


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


# Make distance matrix ----
st_geometry(recs) <- pts
dist_mat <- matrix(nrow = nrow(recs), ncol = nrow(recs))

key <- t(combn(seq(1, nrow(recs), 1), 2))


##  Brute force ditance calculation
for(i in 1:nrow(key)){
  # Split flowline into thirds according to each combination of receivers
  flowline_split <- lwgeom::st_split(flowline, recs[c(key[i, 1], key[i, 2]),])
  flowline_split <- st_collection_extract(flowline_split, 'LINESTRING')

  # Find the length of the middle third
  dist_mat[key[i, 1], key[i, 2]] <- st_length(flowline_split)[2]
}

diag(dist_mat) <- 0



# Convert distance matrix to data frame ----
dist_mat <- rbind(recs$station, dist_mat)
dist_mat <- cbind(c('from', recs$station), dist_mat)


dist_df <- as.data.frame(dist_mat)
names(dist_df) <- dist_df[1,]
dist_df <- dist_df[-1,]


dist_df <- tidyr::pivot_longer(dist_df, -from,
                               names_to = 'to', values_to = 'distance_m') %>%
  mutate(distance_m = as.numeric(distance_m),
         distance_m = round(distance_m)) %>%
  filter(!is.na(distance_m))
