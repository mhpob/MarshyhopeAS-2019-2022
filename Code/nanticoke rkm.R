library(dplyr); library(sf)

# Read and manipulate flowline data ---
flowline <- st_read('data/raw/nanticoke_flowline.gpkg')


##  Merge different within-river secctions into one.
flowline <- flowline %>%

  # Combine different sections into one object
  group_by(gnis_name) %>%
  summarize(geom = st_combine(geom), .groups = 'keep') %>%

  # Merge within-object lines together
  summarize(geom = st_line_merge(geom)) %>%
  st_transform(32618)



# Calculate RKM of Creek mouths ----
## Find where the creeks meet the mainstem
mouths <- flowline %>%
  st_intersection() %>%
  filter(n.overlaps == 2)

## Find RKM of Creek mouths
nan_split <- flowline %>%
  filter(gnis_name == 'Nanticoke River') %>%

  # Split Nanticoke by mouth locations
  lwgeom::st_split(mouths) %>%

  # Pull out the split sections
  st_collection_extract('LINESTRING') %>%

  # Find their lengths in km
  st_length() %>%
  units::set_units(km)

## Return RKMs
cumsum(nan_split)

# Units: [km]
# [1] 45.55766 58.47172 68.09327 80.51762
#     Marshy,  Broad,   Deep



# Break the line that makes up the flowline into points 1 km apart from each other ----
rkms <- flowline %>%
  st_line_sample(density = 1/1000) %>%
  st_zm() %>%
  st_as_sf() %>%
  mutate(body = flowline$gnis_name) %>%
  st_cast('POINT', ids = body) %>%
  rbind(lwgeom::st_endpoint(flowline) %>%
          st_as_sf() %>%
          mutate(body = flowline$gnis_name)) %>%
  st_transform(4326) %>%
  group_by(body) %>%
  mutate(rkm = (length(body)-1):0) %>%
  ungroup() %>%
  mutate(lat = st_coordinates(.)[,2],
         long = st_coordinates(.)[,1]) %>%
  as.data.frame() %>%
  mutate(x = NULL) %>%
  arrange(body, rkm)


write.csv(rkms, 'data/raw/nanticoke_system_rkms.csv', row.names = F)
