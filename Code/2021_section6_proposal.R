library(dplyr); library(sf)

# Create box to crop using well-known text
#   Decided not to do it this way, but keeping it here in case I want to in the future

# crop_wkt <- st_bbox(c(ymin = 38.223568, xmin = -75.969909,
#                       ymax = 38.730472, xmax = -75.552283),
#                     crs = st_crs(4269)) %>%
#   st_as_sfc() %>%
#   st_as_text()


nan <- st_read('data/raw/NHD_H_0208_HU4_GDB.gdb',
               query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan <- st_read('data/raw/NHD_H_0208_HU4_GDB.gdb',
               layer = 'nhdarea',
               wkt_filter = nan$wkt)


dnrec <- fread('data/raw/dnrec_2021receivers.csv')


nan_recs <- data.table::fread('p:/obrien/biotelemetry/detections/dnr/nanticoke/vue_export_2020_nanticoke.csv')
marsh_recs <- data.table::fread('p:/obrien/biotelemetry/detections/dnr/marshyhope/vue_export_2020_marshyhope.csv')

nan_recs <- unique(nan_recs, by = 'Station Name')
marsh_recs <- unique(marsh_recs, by = 'Station Name')

marsh21 <- fread('data/raw/mdnr_2021receivers.csv')


locs <- rbind(nan_recs[, c('Station Name', 'Latitude', 'Longitude')],
              marsh_recs[, c('Station Name', 'Latitude', 'Longitude')],
              dnrec,
              marsh21,
              use.names = F)

#issue with current location of "ROFO Corner"
locs <- locs[`Station Name` != 'ROFO Corner']


rkm_lines <- st_read('data/derived/rkm_lines.gpkg') %>%
  arrange(body, rkm)


# Locations of labels
river_labels <- data.frame(
  long = c(-75.825,
           #-75.77, -75.7,
           -75.8, -75.67,
           -75.625, -75.58),
  lat = c(38.255,
          # 38.63, 38.61,
          38.532, 38.532,
          38.557, 38.633),
  labs = c('Lower Nanticoke', 'Marshyhope Creek', 'Upper Nanticoke',
           'Broad Creek', 'Deep Creek')
)
city_labels <- data.frame(
  long = c(-75.615, -75.77),
  lat = c(38.64, 38.692),
  labs = c('Seaford, DE', 'Federalsburg, MD')
)


# Plotting
library(ggplot2); library(ggrepel); library(patchwork); library(ragg)

lower <-
  ggplot() +
  geom_sf(data = nan) +
  geom_sf(data = rkm_lines, color = 'blue', lwd = 1) +
  geom_point(data = locs, aes(x = Longitude, y = Latitude),
             size = 3) +
  scale_color_viridis_d(option = 'turbo') +
  geom_label_repel(data = filter(rkm_lines, grepl('Nan', body), rkm <= 45),
                   aes(label = rkm, geometry = geom),
                   stat = 'sf_coordinates',
                   nudge_x = -75.74 - st_coordinates(
                     st_point_on_surface(
                       filter(rkm_lines, grepl('Nan', body), rkm <= 45)))[,1],
                   label.size = 0, label.padding = unit(0.1, 'line'),
                   segment.color = 'blue')+
  coord_sf(xlim = c(-75.96, -75.729), ylim = c(38.2467, 38.55), expand = F) +
  geom_text(data = river_labels[river_labels$labs == 'Lower Nanticoke',],
            aes(x = long, y = lat, label = labs)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = 'none')


upper <-
  ggplot() +
  geom_sf(data = nan) +
  geom_sf(data = rkm_lines, color = 'blue', lwd = 1) +
  geom_point(data = locs, aes(x = Longitude, y = Latitude),
             size = 3) +
  scale_color_viridis_d(option = 'turbo') +
  geom_label_repel(data = filter(rkm_lines, !(grepl('Nan|Deep', body) & rkm <= 45)),
                   aes(label = rkm, geometry = geom),
                   stat = 'sf_coordinates',
                   nudge_x = c(c(0.02, 0, 0),
                               -75.85 - st_coordinates(
                                 st_point_on_surface(
                                   filter(rkm_lines, grepl('Mar', body))
                                 ))[, 1],
                               -75.545 - st_coordinates(
                                 st_point_on_surface(
                                   filter(rkm_lines, grepl('Nan', body), rkm > 45)
                                 ))[, 1]),
                   nudge_y = c(rep(0.017, 3),
                               rep(0, 6),
                               rep(0, 6)),
                   label.size = 0, label.padding = unit(0.1, 'line'),
                   segment.color = 'blue') +
  coord_sf(label_axes = '-NE-',
           xlim = c(-75.845, -75.54), ylim = c(38.52, 38.709), expand = F) +
  geom_text(data = river_labels, aes(x = long, y = lat, label = labs),
            check_overlap = T) +
  geom_label_repel(data = city_labels, aes(x = long, y = lat, label = labs),
                   nudge_x = c(-0.04, 0.05), nudge_y = c(0.01, -0.01),
                   label.size = 0, label.padding = unit(0.1, 'line')) +
  labs(x = NULL, y = NULL, color = 'Year') +
  theme_bw() +
  theme(legend.position = c(0.7, 0.96),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(color = guide_legend(direction = 'horizontal'))





# Make a polygon using a bounding box
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


# Create data frame to hold labels
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


inset <- ggplot() +
  geom_sf(data = inset_map, fill = 'gray') +
  geom_text(data = river_labels1, aes(x = long, y = lat, label = labs), angle = -45,
            size = 8 / .pt) +
  geom_text(data = river_labels2, aes(x = long, y = lat, label = labs), angle = 45,
            size = 6 / .pt) +
  annotate('rect', xmin = -75.96, xmax = -75.54, ymin = 38.2467, ymax = 38.7,
           fill = NA, color = 'black', linetype = 'dotted') +
  coord_sf(expand = F) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))
# panel.border = element_rect(color = 'black', fill = NA))


library(cowplot)
lower_inset <- lower + draw_plot(inset, -76, 38.399, 0.2, 0.149)



agg_tiff('figures/proposal_map.tif', res = 600,
         width = 7.5, height = 3.65, scaling = .75, units = 'in', compression = 'lzw')

lower_inset + upper & theme(plot.margin = margin(0, 0, 0, 0))


dev.off()

