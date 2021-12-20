library(data.table); library(sf)
nan <- st_read('data/raw/geo/NHD_H_0208_HU4_GDB.gdb',
               query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan <- st_read('data/raw/geo/NHD_H_0208_HU4_GDB.gdb',
               layer = 'nhdarea',
               wkt_filter = nan$wkt)

# dnr <- lapply(c('p:/obrien/biotelemetry/detections/dnr/nanticoke',
#                  'p:/obrien/biotelemetry/detections/dnr/marshyhope'),
#                list.files, pattern = '20(19|20).*.csv', full.names = T)
#
# dnr <- do.call(c, dnr)
#
# dnr <- lapply(dnr, fread, fill = T)
#
# dnr <- rbindlist(dnr[sapply(dnr, nrow) != 0])
#
#
# dnrec <- fread('data/raw/de detections_2019-20.csv')
#
# dets <- rbind(dnr, dnrec)
# dets <- unique(dets, by = c('Date and Time (UTC)', 'Station Name'))

# fwrite(dets, 'data/derived/all_detections_to_2020.csv')
dets <- fread('data/derived/all_detections_to_2020.csv',
              col.names = function(.) tolower(gsub('[) (]', '', .)))
tag_data <- fread('data/raw/embargo/sturgeon capture data.csv', col.names = tolower)

dets <- tag_data[dets, on = 'transmitternumber == transmitter', nomatch = 0]


# dets <- dets[transmitter %in%
#                # MDNR transmitters
#                paste0('A69-9001-',
#                       c(# MDNR transmitters
#                         seq(21063, 21072, 1),
#                         seq(23900, 23904, 1),
#                         seq(26350, 26354, 1),
#                         seq(27543, 27547, 1),
#                         seq(18009, 18010, 1),
#                         seq(18977, 18983, 1),
#                         # DNREC transmitters
#                         10157,
#                         seq(21060, 21062, 1),
#                         seq(6274, 6276, 1)))]

dets[, ':='(day = yday(dateandtimeutc),
            year = year(dateandtimeutc))]
dets <- dets[year >= 2019]

agg_dets <- dets[, .(N = uniqueN(transmitternumber)),
                 by = c('day', 'year', 'stationname', 'latitude', 'longitude')]
agg_dets[, date := as.Date(paste(year, '01-01', sep = '-')) + day + 1]

agg_dets <- st_as_sf(agg_dets,
                     coords = c('longitude', 'latitude'),
                     crs = 4326)

rkm_lines <- st_read('data/derived/rkm_lines.gpkg')
setorder(rkm_lines, body, rkm)


# Locations of labels
river_labels <- data.frame(
  long = c(-75.825,
           -75.89,
           -75.625),
  lat = c(38.3,
          38.58,
          38.554),
  labs = c('Nanticoke River', 'Marshyhope Creek', 'Broad Creek')
)
city_labels <- data.frame(
  long = c(-75.6, -75.79, -75.57),
  lat = c(38.67, 38.692, 38.645),
  labs = c('Seaford, DE', 'Federalsburg, MD', 'Deep Creek')
)
city_locs <- data.frame(
  long = c(-75.617542, ),
  lat = c(38.640819, )
)

fburg <- st_read('Data/Raw/geo/Maryland_Property_Data_-_Tax_Map_Grids',
                 query = "SELECT * FROM \"Maryland_Property_Data_-_Tax_Map_Grids\" WHERE JURSCODE = 'CARO'") %>%
  st_transform(4326) %>%
  st_union()
sford <- st_read('Data/Raw/geo/bnd_towns')



inset_map <- st_read('p:/obrien/midatlantic/matl_states_land.shp')

crop_box <- st_bbox(c(ymin = 36.76498, xmin = -77.08675,
                      ymax = 39.72379, xmax = -74.84402),
                    crs = st_crs(4326)) %>%
  # turn into a simple features collection
  st_as_sfc() %>%
  st_transform(4269)


inset_map <- inset_map %>%
  st_crop(crop_box)


river_labels1 <- data.frame(
  long = c(-76.8, -76.829, -76.83, -76.79),
  lat = c(36.99, 37.42, 37.75, 38.03),
  labs = c('James', 'York', 'Rapp.', 'Potomac')
)
#   river_labels2 <- data.frame(
#   long = -75.65,
#   lat = 38.46,
#   labs = 'Nanticoke'
# )


library(ggplot2); library(ggrepel); library(ragg)
library(gganimate)

inset <- ggplotGrob(
  ggplot() +
  geom_sf(data = inset_map, fill = 'gray') +
  annotate('rect', xmin = -75.96, xmax = -75.54, ymin = 38.247, ymax = 38.709,
           fill = NA, color = 'white') +
  geom_text(data = river_labels1, aes(x = long, y = lat, label = labs), angle = -45,
            size = 10 / .pt, color = 'gray20') +
  # geom_text(data = river_labels2, aes(x = long, y = lat, label = labs), angle = 45,
  #           size = 12 / .pt, color = 'antiquewhite') +
  coord_sf(label_axes = '----', expand = F) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = 'white'))
)





job::job({
  anim_save(
    'figures/no_git/Nanticoke_2020_inset.mp4',

    animate(
      ggplot() +
        geom_sf(data = fburg, fill = 'yellow') +
        geom_sf(data = sford, fill = 'yellow') +
        geom_sf(data = nan) +
        geom_sf(data = rkm_lines, color = 'blue', lwd = 1) +
        geom_sf(data = agg_dets[agg_dets$year == 2020,],
                aes(size = N, group = stationname))  +
        coord_sf(xlim = c(-75.96, -75.54), ylim = c(38.247, 38.709), expand = F) +
        geom_text(data = river_labels, aes(x = long, y = lat, label = labs),
                  check_overlap = T) +
        geom_label_repel(data = city_labels, aes(x = long, y = lat, label = labs),
                         # nudge_x = c(-0.04, 0.07, 0),
                         # nudge_y = c(0.01, -0.01, -0.035),
                         nudge_x = c(-0.04, -0.08, 0),
                         nudge_y = c(0.04, 0, -0.035),
                         label.size = 0, label.padding = unit(0.1, 'line')) +
        labs(x = NULL, y = NULL, size = 'Number of\ntagged sturgeon') +
        theme_bw() +
        theme(legend.position = c(0.15, 0.47),
              legend.margin = margin(0, 0, 0, 0),
              legend.background = element_blank(),
              axis.text.y = element_text(angle = 45),
              plot.margin = margin(1, 1, 1, 1)) +
        scale_size_continuous(breaks = c(1, 2, 5, 7)) +
        annotation_custom(inset, -75.71, -75.54, 38.26, 38.47) +


        transition_time(date, range = as.Date(c('2020-08-01', '2020-10-31'))) +
        labs(title = '{frame_time}'),
      nframes = 91,
      fps = 3,
      renderer = ffmpeg_renderer(),
      device = 'ragg_png',
      res = 300,
      width = 2492,
      height = 3300,
      scaling = 1.7

    )
  )
})

# Gif for GitHub README
job::job({
  anim_save(
    'figures/Nanticoke_2020_inset.gif',

    animate(
      ggplot() +
        geom_sf(data = fburg, fill = 'yellow') +
        geom_sf(data = sford, fill = 'yellow') +
        geom_sf(data = nan) +
        geom_sf(data = rkm_lines, color = 'blue', lwd = 1) +
        geom_sf(data = agg_dets[agg_dets$year == 2020,],
                aes(size = N, group = stationname))  +
        coord_sf(xlim = c(-75.96, -75.54), ylim = c(38.247, 38.709), expand = F) +
        geom_text(data = river_labels, aes(x = long, y = lat, label = labs),
                  check_overlap = T) +
        geom_label_repel(data = city_labels, aes(x = long, y = lat, label = labs),
                         # nudge_x = c(-0.04, 0.07, 0),
                         # nudge_y = c(0.01, -0.01, -0.035),
                         nudge_x = c(-0.04, -0.08, 0),
                         nudge_y = c(0.04, 0, -0.035),
                         label.size = 0, label.padding = unit(0.1, 'line')) +
        labs(x = NULL, y = NULL, size = 'Number of\ntagged sturgeon') +
        theme_bw() +
        theme(legend.position = c(0.15, 0.47),
              legend.margin = margin(0, 0, 0, 0),
              legend.background = element_blank(),
              axis.text.y = element_text(angle = 45),
              plot.margin = margin(1, 1, 1, 1)) +
        scale_size_continuous(breaks = c(1, 2, 5, 7)) +
        annotation_custom(inset, -75.71, -75.54, 38.26, 38.47) +


        transition_time(date, range = as.Date(c('2020-08-01', '2020-10-31'))) +
        labs(title = '{frame_time}'),
      nframes = 91,
      fps = 3,
      renderer = gifski_renderer(),
      device = 'ragg_png',
      res = 300,
      width = 624,
      height = 826,
      scaling = 0.47

    )
  )
})
