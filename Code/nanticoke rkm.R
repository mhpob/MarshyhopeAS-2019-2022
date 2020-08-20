library(sf)

# Read and manipulate flowline data ---
flowline <- st_read('data/raw/nanticoke_flowline.gpkg')


##  Merge different river sections into one.
flowline <- st_combine(flowline)
flowline <- st_line_merge(flowline)


# Reproject spatial object to something measured in meters----
flowline <- st_transform(flowline, 32618)



# Break the line that makes up the flowline into points 1 km apart from each other ----
rkms <- flowline %>%
  st_line_sample(n = as.numeric(st_length(flowline)/1000)) %>%
  st_cast('POINT') %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as.data.frame

write.csv(rkms, 'data/raw/nanticoke_rkms.csv')
