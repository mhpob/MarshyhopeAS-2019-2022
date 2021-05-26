library(ggplot2); library(patchwork); library(dplyr); library(sf)

positions <- data.table::fread(file.path('p:/obrien/biotelemetry/marshyhope/vps',
                                         'VPS-NanticokeRiver-Brookview-01-Results-20210106',
                                         'positions',
                                         'all-calc-positions.csv'),

                               # this just applies base::tolower to the column names
                               col.names = tolower)

## Total positions
nrow(positions)

## Fish positions
fish <- positions %>%
  filter(grepl('^\\d', transmitter))
nrow(fish)

fish_time <- fish %>%
  mutate(date_fl = lubridate::floor_date(datetime, '6 hours'),
         type = 'Sturgeon',
         type2 = 'all') %>%
  group_by(date_fl, type, type2) %>%
  summarise(n = n())

fish_time_quality <- fish %>%
  filter(hpe <= quantile(fish$hpe, 0.9, na.rm = T)) %>%
  mutate(date_fl = lubridate::floor_date(datetime, '6 hours'),
         type = 'Sturgeon',
         type2 = 'quality') %>%
  group_by(date_fl, type, type2) %>%
  summarise(n = n())


## Station positions
station <- positions %>%
       filter(!grepl('^\\d', transmitter))
nrow(station)

station_time <- station %>%
  mutate(date_fl = lubridate::floor_date(datetime, '6 hours'),
         type = 'Station',
         type2 = 'station') %>%
  group_by(date_fl, type, type2) %>%
  summarise(n = n())

ggplot(data = rbind(fish_time, fish_time_quality, station_time)) +
  geom_line(aes(x = date_fl, y = n, lty = type2),
            show.legend = F) +
  scale_x_datetime(date_breaks = 'week', date_labels = '%b %d') +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid')) +
  labs(x = NULL, y = 'Positions per 6 hrs') +
  facet_wrap(~type, ncol = 1, strip.position = 'right') +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))


## Remove top 10% of error
fish <- fish %>%
  filter(hpe <= quantile(fish$hpe, 0.9, na.rm = T))

nrow(fish)


## Convert to spatial
fish <- fish %>%
  st_as_sf(coords = c('lon', 'lat'),
           crs = 4326)

station <- station %>%
  filter(transmitter != 'Float through',
         hpe <= quantile(hpe, 0.95, na.rm = T)) %>%
  group_by(transmitter) %>%
  summarize(lat = median(lat),
            lon = median(lon)) %>%
  st_as_sf(coords = c('lon', 'lat'),
           crs = 4326)


## Transform to same CRS as HUD
fish <- fish %>%
  st_transform(4269)

station <- station %>%
  st_transform(4269)


## Import map
crop_box <- fish %>%
  # Find the bounding box
  st_bbox() %>%
  # Increase the bounds a little
  + c(-0.0005, -0.0005, 0.0005, 0.0005) %>%
  # Convert that bounding box to "simple features collection"
  st_as_sfc()

mh_shape <- st_read('data/raw/NHD_H_0208_HU4_GDB.gdb',
                    layer = 'nhdarea',

                    # st_as_text converts the cropping box to "well-known text", which is
                    #   a specific way to format spatial data. wkt_filter imports
                    #   anything inside or touching this box.
                    wkt_filter = st_as_text(crop_box))

# st_crop cuts off anything that was touching, but is outside of, the box.
mh_shape <- mh_shape %>%
  st_crop(crop_box)


## Import habitat polygons
habitat <- st_read('data/raw/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                   layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016',
                   wkt_filter = st_as_text(crop_box %>% st_transform(26918))) %>%
  st_transform(st_crs(mh_shape)) %>%
  st_make_valid() %>%
  st_crop(crop_box)



## Make map by week
fish <- fish %>%
  mutate(wk = lubridate::week(datetime))

library(ragg)

agg_png('vps.png', scaling = 1.75, width = 1270, height = 776)

ggplot() +
  geom_sf(data = mh_shape, fill = NA) +
  geom_sf(data = habitat, aes(fill = Group_)) +
  geom_sf(data = fish, alpha = 0.2, color = 'lightgray') +
  geom_sf(data = station, color = 'yellow', shape = 17) +
  labs(fill = 'Bottom Type') +
  scale_fill_viridis_d(option = 'H') +
  scale_x_continuous(breaks = seq(-75.796, -75.784, 0.004)) +
  facet_wrap(~ wk) +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12))

dev.off()













## Positions per m^2 figure
area_agg <- habitat %>%
  group_by(Group_) %>%
  summarize(area = st_area(
    st_union(
      Shape
    )
  )) %>%
  st_drop_geometry()

pip_scaled <- fish %>%
  st_intersection(habitat)%>%
  group_by(Group_, transmitter) %>%
  summarize(n = n()) %>%
  left_join(area_agg, by = 'Group_') %>%
  mutate(area = as.numeric(area),
         transmitter = as.factor(transmitter))





library(lme4)
mod <- glmer(n ~ Group_ + (1|transmitter) + offset(log(area)),
             family = 'poisson',
             data = pip_scaled)

library(emmeans)

# Provides pairwise comparisons
#   and the estimated marginal means for each group,
#   on the response scale
#   per 100 m^2
estim <- emmeans(mod, pairwise ~ Group_, type = 'response', offset = log(100),
                 adjust = 'bonferroni')


coefs <- coef(mod)$transmitter %>%
  data.frame(check.names = F) %>%
  mutate(transmitter = row.names(.)) %>%
  mutate_at(vars(matches('^G')),
            ~ . + `(Intercept)`) %>%
  tidyr::pivot_longer(matches('^[(G]'),
                      names_to = 'Group_') %>%
  mutate(Group_ = ifelse(Group_ == '(Intercept)', 'Gravel_Mixes', Group_),
         Group_ = gsub('Group_', '', Group_),
         value = exp(value) * 100)


all <- ggplot(data = cbind(data.frame(estim$emmeans),
                    lab = c('B', 'D', 'B',
                            'A', 'C', 'E'))) +
  geom_pointrange(aes(x = Group_, y = rate, ymin = asymp.LCL, ymax = asymp.UCL,
                 color = Group_), shape = 17, size = 1,
                 show.legend = F) +
  geom_text(aes(x = Group_, y = asymp.UCL, label = lab), nudge_x = 0.1) +
  geom_point(data = coefs,
             aes(x = Group_,
                 y = value,
                 color = Group_),
             alpha = 0.4,
             show.legend = F) +
  scale_color_viridis_d(option = 'H') +
  labs(x = 'Bottom type', y = 'Positions per 100 m^2') +
  coord_cartesian(x = c(0.5, 6.5), y = c(0, 2), expand = F) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

per_fish <- ggplot(data = pip_scaled) +
  geom_col(aes(x = Group_, y = n / area * 100, fill = Group_),
           show.legend = F) +
  facet_wrap(~ transmitter) +
  labs(x = 'Bottom type', y = 'Positions per m^2') +
  scale_fill_viridis_d(option = 'H') +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid.minor = element_blank())



# all <- ggplot(data = pip_scaled) +
#   geom_col(aes(x = Group_, y = n / area, fill = Group_),
#            show.legend = F) +
#   geom_text(data = summarize(group_by(pip_scaled, Group_, lab), y = sum(n/area)),
#             aes(x = Group_, y = y, label = lab), vjust = -1) +
#   labs(x = 'Bottom type', y = 'Positions per m^2') +
#   scale_fill_viridis_d(option = 'H') +
#   theme_minimal() +
#   theme(axis.text = element_text(size = 12),
#         axis.title = element_text(size = 12))
#
# per_fish <- ggplot(data = pip_scaled) +
#   geom_col(aes(x = Group_, y = n / as.numeric(area), fill = Group_),
#            show.legend = F) +
#   facet_wrap(~ transmitter) +
#   labs(x = 'Bottom type', y = 'Positions per m^2') +
#   scale_fill_viridis_d(option = 'H') +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 12),
#         axis.text.x = element_blank(),
#         axis.title = element_blank())


library(patchwork)

agg_png('habitat_positions.png', scaling = 1.3, width = 1200, height = 625)

all + per_fish + plot_annotation(tag_levels = 'A')

dev.off()



