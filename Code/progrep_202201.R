library(data.table); library(suncalc); library(ggplot2)
## lubridate will be used, but called directly via ::

# Find hours where the camera failed
## F-ARIS deployment period, by hour
aris <- data.table(
  start = seq.POSIXt(
    as.POSIXct('2021-08-19 14:00', tz = 'America/New_York'),
    as.POSIXct('2021-10-18 11:00', tz = 'America/New_York'),
    by = 'hour'
  )
)

## Make a dummy "end" column
aris[, end := start]


## Periods where the camera failed
camera_failure <- data.table(
  start = as.POSIXct(c('2021-08-22 12:00', '2021-08-26 10:00', '2021-09-07 18:00',
                       '2021-10-07 04:00', '2021-10-11 11:00'), tz = 'America/New_York'),
  end = as.POSIXct(c('2021-08-23 16:00', '2021-09-07 14:00', '2021-09-16 13:00',
                     '2021-10-07 12:00', '2021-10-14 12:00'), tz = 'America/New_York')
)

## Find which hours of deployment had camera failure
setkey(camera_failure, start, end)
aris <- foverlaps(aris, camera_failure)

## Camera was on if the hour was not during a period of camera failure (i.e., ==NA)
aris[, camera_status := fifelse(is.na(start), 'Camera on', 'Camera off')]

## Remove unneeded columns and rename column that lists the hours
aris[, ':='(start = NULL, end = NULL, i.start = NULL, i.end = NULL,
           'hrs' = i.end)]





# Sturgeon targets from F-ARIS as of 2022-01-10
sturg_targs <- as.POSIXct(c('2021-08-19 16:00', '2021-08-21 05:00', '2021-08-21 20:00',
                            '2021-08-24 13:00', '2021-08-24 15:00', '2021-08-24 20:00',
                            '2021-08-25 07:00'), tz = 'America/New_York')

## Create column IDing hours with sturgeon detections
aris[, sturg_targs := fifelse(hrs %in% sturg_targs, T, F)]


# Sunrise/set times
sun <- getSunlightTimes(date = unique(as.Date(aris$hrs)), lat = 38.56, lon = -75.76,
                        tz = 'America/New_York', keep = c('sunrise', 'sunset'))

## Find which hours are day or night
setDT(sun)
setkey(sun, sunrise, sunset)

## Create dummy end time column
aris[, hrs2 := hrs]

## Overlap join
aris <- foverlaps(aris, sun, by.x = c('hrs', 'hrs2'))
aris[, sun := fifelse(is.na(date), 'Night', 'Day')]


# Tide times
## Note that everything in this section is assuming that an hour is not split between
##    tides. Fine for a coarse overview, but should be refined if used for a more-
##    directed analysis.
tide <- fread('data/raw/sharptown_tide_predicted.txt', fill = T)

## Combine day and time columns
tide[, datetime := as.POSIXct(paste(Date, Time), tz = 'America/New_York')]

## Pivot DT to create start/stop times of ebb tide
tide <- dcast(tide, datetime ~ `High/Low`, value.var = 'datetime')

## Fill in missing high tide times so that they are paired with a low tide time,
##    remove datetime column (not needed)
tide[, ':='(H = nafill(H, 'locf'),
            datetime = NULL)]

## Remove rows where low tide time is NA
tide <- tide[!is.na(L)]

## Overlap join ebb tide periods
setkey(tide, H, L)
aris <- foverlaps(aris, tide, by.x = c('hrs', 'hrs2'))

## If there wasn't a match, it was a flood tide.
aris[, tide := fifelse(is.na(H), 'Flood', 'Ebb')]

## Remove unneeded columns
aris <- aris[, -c('date', 'lat', 'lon', 'hrs2')]


# Import detections of tagged sturgeon from station 3e
rec_3e <- fread('p:/obrien/biotelemetry/marshyhope/vps 2021/files/VR2W_127331_20211028_1.csv',
                select = c('Date and Time (UTC)', 'Transmitter'),
                col.names = function(.) tolower(gsub('and|\\(UTC)| ', '', .)),
                fill = T)

## Select sturgeon transmitters, Just so happen to be -9001- transmitters
rec_3e <- rec_3e[grepl('9001', transmitter)]

## Convert time to EDT
rec_3e[, hr := lubridate::with_tz(hr, 'America/New_York')]
## Round down to hour
rec_3e[, hr := lubridate::floor_date(datetime, 'hour')]

## Label hours that detected tagged sturgeon
aris[, sturg_vps := fifelse(hrs %in% unique(rec_3e$hr), T, F)]



# Visualize distribution of camera footage/detections/tides/daylight
## Melt DT to use geom_raster
aris_long <- melt(aris, id.vars = c('hrs', 'sturg_vps', 'sturg_targs'),
                  measure.vars = c('camera_status', 'sun', 'tide'))
aris_long[, value := factor(value, ordered = T,
                            levels = c('Ebb', 'Flood',
                                       'Day', 'Night',
                                       'Camera on', 'Camera off'))]

## Create figure
ggplot() +
  geom_raster(data = aris_long, aes(x = hrs, y = variable, fill = value)) +
  geom_rug(data = aris_long[sturg_vps == T], aes(x = hrs)) +
  geom_segment(data = aris_long[sturg_targs == T],
               aes(x = hrs, xend = hrs, y = 0.48, yend = 3.52),
               size = 1, color = 'yellow') +
  annotate('segment', x = as.POSIXct('2021-08-25 14:00'), xend = as.POSIXct('2021-08-25 14:00'),
           y = 0.48, yend = 3.52, color = 'black', linetype = 'dashed',
           size = 1) +
  scale_fill_manual(values = c('red1', 'rosybrown', 'lightblue', 'darkblue', 'darkgreen', 'white')) +

  labs(fill = '') +
  scale_x_datetime(date_breaks = 'week', date_labels = '%d-%b',
                   limits = as.POSIXct(c('2021-08-19', '2021-10-19')),
                   expand = c(0, 0)) +
  theme_void() +
  theme(axis.text.x = element_text(size = 15),
        legend.text = element_text(size = 12))


# Descriptive stats
## Number of hours that have been analyzed as of 2022-01-10
hrs_analyzed <- length(seq.POSIXt(
  as.POSIXct('2021-08-19 14:00'),
  as.POSIXct('2021-08-25 14:00'),
  by = 'hour'
))

## % hours lost to camera failure
nrow(aris[camera_status == 'Camera off']) / nrow(aris)

## % hours analyzed as of 2022-01-10
hrs_analyzed / nrow(aris[camera_status == 'Camera on'])

## Cross-tabulation
### Sun
addmargins(xtabs(~ sun + camera_status, data = aris))
summary(xtabs(~sun + camera_status, data = aris))

### Tide
addmargins(xtabs(~ tide + camera_status, data = aris))
summary(xtabs(~ tide + camera_status, data = aris))

### Sun x Tide
addmargins(xtabs(~sun + tide + camera_status, data = aris))
summary(xtabs(~sun + tide + camera_status, data = aris))
