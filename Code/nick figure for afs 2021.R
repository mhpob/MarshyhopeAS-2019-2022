library(sf); library(ggplot2)
library(patchwork); library(ragg)

nan <- st_read('data/raw/NHD_H_0208_HU4_GDB.gdb',
               query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan <- st_read('data/raw/NHD_H_0208_HU4_GDB.gdb',
               layer = 'nhdarea',
               wkt_filter = nan$wkt)

rkm_lines <- st_read('data/derived/rkm_lines.gpkg')


city_labels <- data.frame(
  long = c(-75.615, -75.77, -75.57),
  lat = c(38.64, 38.692, 38.645),
  labs = c('Seaford, DE', 'Federalsburg, MD', 'Deep Creek')
)


marshy <-
  ggplot() +
  geom_sf(data = nan) +
  geom_sf(data = rkm_lines, color = 'blue', lwd = 2) +
  coord_sf(label_axes = '--EN',
           xlim = c(-75.845, -75.73), ylim = c(38.51, 38.709), expand = F) +
  annotate('point', y = 38.545, x = -75.762, shape = 'circle', size = 5, fill = 'black') +
  annotate('text', x = -75.805, y = 38.692, label = 'Federalsburg, MD', size = 12/.pt) +
  scale_x_continuous(breaks = seq(-75.84, -75.74, by = 0.04)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(plot.margin = margin(0, 3, 0, 0),
        axis.text.y = element_text(angle = 45, size = 10),
        axis.text.x = element_text(size = 10))


crop_box <- st_bbox(c(ymin = 36.76498, xmin = -77.08675,
                      ymax = 39.72379, xmax = -74.84402),
                    crs = st_crs(4326)) %>%
  # turn into a simple features collection
  st_as_sfc()


# Import map, selecting only features that touch the crop box.
#   Do this by turning the box into well-known text

inset_map <- st_read('c:/users/darpa2/analysis/nansturg-analysis/manuscript/data/spatial/natural earth/ne_10m_coastline.shp',
                     # turn box into well-known text
                     wkt_filter = st_as_text(crop_box))

# plot(inset_map$geometry)


# Use the coastline (a linestring) to cut up the bbox polygon
inset_map <- crop_box %>%
  lwgeom::st_split(inset_map)%>%
  # Separate into individual features
  st_collection_extract() %>%
  # just so happens that features 6 (upper Potomac) and 2 (everything else) are
  #   redundant to what we need
  .[-c(2, 6)]

river_labels1 <- data.frame(
  long = c(-76.832102, -76.902022),
  lat = c(36.99268, 37.612137),
  labs = c('James', 'York')
)
river_labels2 <- data.frame(
  long = -75.65,
  lat = 38.46,
  labs = 'Nanticoke'
)

inset <-
  ggplot() +
  geom_sf(data = inset_map, fill = 'gray') +
  geom_text(data = river_labels1, aes(x = long, y = lat, label = labs), angle = -45,
            size = 12 / .pt) +
  geom_text(data = river_labels2, aes(x = long, y = lat, label = labs), angle = 45,
            size = 12 / .pt) +
  annotate('rect', xmin = -75.845, xmax = -75.73, ymin = 38.51, ymax = 38.709,
           fill = NA, color = 'black') +
  coord_sf(label_axes = '-NE-', expand = F) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 3),
        axis.text.y = element_text(angle = -45, vjust = 0, size = 10),
        axis.text.x = element_text(size = 10))



agg_png('figures/nick_poster.png',
        width = 8, height = 6.83, units = 'in', res = 144)

marshy + inset

dev.off()
