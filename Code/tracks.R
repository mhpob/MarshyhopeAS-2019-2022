library(ggplot2); library(sf); library(data.table)

innovasea_proj <- '+proj=aeqd +lat_0=38.542096 +lon_0=-75.761724 +x_0=1800 +y_0=1800'

# Basemap
nan <- st_read('data/raw/geo/NHD_H_0208_HU4_GDB.gdb',
               query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan <- st_read('data/raw/geo/NHD_H_0208_HU4_GDB.gdb',
               layer = 'nhdarea',
               wkt_filter = nan$wkt)

nan <- st_transform(nan, innovasea_proj)


seg <- st_crop(nan,
               xmin = 1300, ymin = 1000,
               xmax = 2350, ymax = 2450)

# Position data
vps <- data.table::fread('data/raw/embargo/2021 vps/results/animal/all.csv',
                         col.names = tolower)

vps <- st_as_sf(vps,
                coords = c('x', 'y', 'z'),
                crs = innovasea_proj,
                remove = F)

setDT(vps)

# Remove high-error positions
vps <- vps[hpe < quantile(hpe, 0.9),]

# Remove float-through transmitter (29927) and sentinel transmitter (65011)
vps <- vps[!id %in% c(65011, 29927)]


# ARIS location
aris_loc <- st_sfc(st_point(c(-75.76290, 38.54498)), crs = 4326)
aris_loc <- st_transform(aris_loc, '+proj=aeqd +lat_0=38.542096 +lon_0=-75.761724 +x_0=1800 +y_0=1800')

# Distance between positions and ARIS
vps[, dist := as.numeric(st_distance(geometry, aris_loc))]

# Identify tracks
setorder(vps, id, time)
vps[, interval := as.numeric(difftime(time, shift(time), units = 'mins')), by = 'id']
vps[is.na(interval), interval := 0]

vps[, track_id := as.numeric(interval > 60), by = 'id']
vps[, track_id := cumsum(track_id), by = 'id']





# Plot tracks
ggplot() +
  geom_sf(data = seg) +
  geom_path(data = vps, aes(x = x, y = y, color = time,
                            group = interaction(id, track_id))) +
  geom_sf(data = aris_loc) +
  geom_sf(data = st_buffer(aris_loc, 100), fill = NA) +
  scale_color_viridis_c(trans = 'time', option = 'cividis') +
  labs(x = NULL, y = NULL, color = NULL) +
  facet_wrap(~ id, nrow = 3) +
  theme_minimal() +
  theme(axis.text = element_blank())


# Plot zoomed in
ggplot() +
  geom_sf(data = seg) +
  geom_sf(data = vps, aes(geometry = geometry, color = time)) +
  geom_path(data = vps, aes(x = x, y = y, color = time,
                            group = interaction(id, track_id))) +
  geom_sf(data = aris_loc) +
  geom_sf(data = st_buffer(aris_loc, 100), fill = NA) +
  coord_sf(xlim = c(1500, 1800), ylim = c(2000, 2300)) +
  scale_color_viridis_c(trans = 'time', option = 'cividis') +
  labs(x = NULL, y = NULL, color = NULL) +
  facet_wrap(~ id, nrow = 3) +
  theme_minimal() +
  theme(axis.text = element_blank())


# Plot each track individually
ggplot() +
  geom_sf(data = seg) +
  # arrows apparently don't play well with colored segments at the moment, so
  # need to split
  geom_path(data = vps, aes(x = x, y = y,
                            group = interaction(track_id, id)),
            arrow = arrow(angle = 15)) +
  geom_path(data = vps, aes(x = x, y = y, color = time,
                            group = interaction(track_id, id))) +
  geom_sf(data = aris_loc) +
  geom_sf(data = st_buffer(aris_loc, 100), fill = NA) +
  scale_color_viridis_c(trans = 'time', option = 'cividis') +
  labs(x = NULL, y = NULL, color = NULL) +
  facet_wrap(~ interaction(track_id, id), nrow = 5) +
  theme_minimal() +
  theme(axis.text = element_blank())
