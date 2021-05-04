

positions <- data.table::fread(file.path('p:/obrien/biotelemetry/marshyhope/vps',
                                         'VPS-NanticokeRiver-Brookview-01-Results-20210106',
                                         'positions',
                                         'all-calc-positions.csv'),
                               col.names = tolower)

positions

unique(positions$transmitter)


library(sf)
positions <- st_as_sf(positions,
                      coords = c('lon', 'lat'),
                      crs = 4326) %>%
  st_transform(4269)


library(dplyr)
fish <- positions %>%
  filter(grepl('^\\d', transmitter))


library(ggplot2)

ggplot(data = fish) +
  geom_sf(aes(color = hpe)) +
  scale_color_continuous(low = 'blue', high = 'red')





crop_box <- positions %>%
  st_bbox() %>%
  st_as_sfc()
mh_shape <- st_read('data/raw/NHD_H_0208_HU4_GDB.gdb',
                    layer = 'nhdarea',
                    wkt_filter = st_as_text(crop_box))

mh_shape <- mh_shape %>%
  st_crop(crop_box)

ggplot() +
  geom_sf(data = mh_shape) +
  geom_sf(data = fish) +
  facet_wrap(~cut(hpe, quantile(hpe, probs = c(0, .25 ,.5, .75, .9, 1))))



ggplot() +
  geom_sf(data = mh_shape) +
  geom_sf(data = fish, aes(color = transmitter)) +
  facet_wrap(~cut(hpe, quantile(hpe, probs = c(0, .25 ,.5, .75, .9, 1))))


fish_subs <- fish %>%
  filter(hpe < 40.3) %>%
  mutate(wk = lubridate::week(datetime))

ggplot() +
  geom_sf(data = mh_shape) +
  geom_sf(data = fish_subs) +
  facet_wrap(~ wk)


ggplot() +
  geom_sf(data = mh_shape) +
  geom_sf(data = fish_subs, aes(color = transmitter)) +
  facet_wrap(~ wk)


# Habitat polygons (big file!):
# https://www.habitat.noaa.gov/chesapeakebay/gis/Seafloor_Mapping_Geodatabases/nanticoke_and_tributaries/

habitat <- st_read('data/raw/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                   layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016',
                   wkt_filter = st_as_text(crop_box %>% st_transform(26918))) %>%
  st_transform(st_crs(mh_shape)) %>%
  st_make_valid() %>%
  st_crop(crop_box)


ggplot() +
  geom_sf(data = mh_shape) +
  geom_sf(data = habitat, aes(fill = Group_)) +
  geom_sf(data = fish_subs, alpha = 0.1)
