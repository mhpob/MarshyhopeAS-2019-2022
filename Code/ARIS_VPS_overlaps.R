library(data.table); library(lubridate); library(sf)


derived_tracks <- lapply(list.files('data/derived/aris mobile gps/',
                                    full.names = T),
                         st_read,
                         options = c("X_POSSIBLE_NAMES=lon",
                                     "Y_POSSIBLE_NAMES=lat"),
                         crs = 4326)

derived_tracks <- rbindlist(derived_tracks)
derived_tracks[, geometry :=
                 st_transform(geometry,
                              '+proj=aeqd +lat_0=38.574224 +lon_0=-75.789577 +x_0=750 +y_0=750')]
derived_tracks[, datetime := mdy_hms(paste(date, time), tz = 'America/New_York')]
derived_tracks[, datetime := with_tz(datetime, 'UTC')]



positions <- fread('data/raw/2020_vps_positions.csv',
                   col.names = tolower)

fish <- positions[grepl('^\\d', transmitter)]
# fish_hpe <- fish[hpe <= quantile(fish$hpe, 0.9, na.rm = T)]
fish <- st_as_sf(fish,
                     coords = c('x', 'y'),
                     crs = '+proj=aeqd +lat_0=38.574224 +lon_0=-75.789577 +x_0=750 +y_0=750')

setDT(fish)

track_sub <- derived_tracks[fish, on = 'datetime', nomatch = 0]
track_sub[, distance := as.numeric(
  st_distance(geometry, i.geometry, by_element = T)
  )]

k <- track_sub[distance <= 100]

lengths <- fread('data/raw/sturgeon capture data.csv',
                 col.names = tolower)
setnames(lengths, 'transmitternumber', 'detectedid')

k <- lengths[k, on = 'detectedid']

k[, c(6:9, 3:4, 26)]











k_aris <- k$buff
k_aris <- st_as_sf(k_aris) %>% st_transform(4326)
k_fish <- k$i.geometry
k_fish <- st_as_sf(k_fish) %>% st_transform(4326)

library(mapview)
mapview(list(k_aris, k_fish))
