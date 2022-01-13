library(data.table)


vps <- list.files('p:/obrien/biotelemetry/marshyhope/vps 2021/files',
                  pattern = '1028.*.csv', full.names = T)

vps <- lapply(vps, fread,
              select = c('Date and Time (UTC)', 'Receiver', 'Transmitter'),
              col.names = function(.) tolower(gsub('and|UTC|[) ()]', '', .)), fill = T)

vps <- rbindlist(vps)


gps_key <- readxl::read_excel(file.path('p:/obrien/biotelemetry/marshyhope',
                                        'vps 2021/vps-marshyhopecreek-henson.xlsx'),
                              sheet = 4, skip = 2, range = 'A3:F37')
deploy_key <- readxl::read_excel(file.path('p:/obrien/biotelemetry/marshyhope',
                                           'vps 2021/vps-marshyhopecreek-henson.xlsx'),
                                 sheet = 3, skip = 2, range = 'A3:H70')
setDT(gps_key)
setnames(gps_key, function(.) tolower(gsub(' ', '', .)))
gps_key[, ':='(waypoint = NULL,
               gtz = NULL)]


setDT(deploy_key)
setnames(deploy_key, function(.) tolower(gsub(' ', '', .)))
deploy_key[, ':='(latitude = NULL,
                  longitude = NULL,
                  stz = NULL)]


## fill NA station names. data.table::nafill currently doesn't work for factors, so fake it
for(i in 1:nrow(gps_key)){
  gps_key[i, 'stationname'] <- ifelse(is.na(gps_key[i, stationname]),
                                       gps_key[i - 1, stationname],
                                       gps_key[i, stationname])
}
for(i in 1:nrow(deploy_key)){
  deploy_key[i, 'name'] <- ifelse(is.na(deploy_key[i, name]),
                                          deploy_key[i - 1, name],
                                          deploy_key[i, name])
}


key <- deploy_key[gps_key, on = c('starttime == gpstime', 'name == stationname')]
key[, c('starttime', 'endtime') := lapply(.SD,
                                          as.POSIXct, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'),
    .SDcols = c('starttime', 'endtime')]
setkey(key, device, starttime, endtime)



vps[, dummy_time := datetime]

# Join receiver station name and location
vps_range <- foverlaps(vps, key, by.x = c('receiver', 'datetime', 'dummy_time'), nomatch = 0)
vps_range <- vps_range[, -c('devicedepth', 'starttime', 'endtime')]

# Join transmitter station name and location
vps_range <- foverlaps(vps_range, key,
                       by.x = c('transmitter', 'datetime', 'dummy_time'),
                       nomatch = 0)
vps_range <- vps_range[, -c('devicedepth', 'starttime', 'endtime', 'dummy_time')]

setnames(vps_range,
         c('name', 'i.name', 'latitude', 'longitude', 'i.latitude', 'i.longitude'),
         c('station_from', 'station_to', 'lat_from', 'lon_from', 'lat_to', 'lon_to'))


vps_range[, day := lubridate::floor_date(datetime, 'day')]

# Calculate number of detections heard from each transmitter x receiver pair ("successes")
successes <- vps_range[, {
  d <- data.table(xtabs(~ station_to + station_from))
  .SD[d, on = c('station_to', 'station_from')]
  },
                       by = c('day')]


# Calculate total number transmissions for each receiver (times a receiver heard its own transmitter)
# Usually, I'd use the diagonal of the xtabs matrix, but it seems that the stations
#   with a sync tag attached had fewer self-detections than other stations detected them.
#   so, I'm going to use the maximum value of the column.
trials <- vps_range[, {
  d <- apply(xtabs(~ station_to + station_from), 2, max)
  data.table(trials = d,
             station_from = names(d))
},
by = c('day')]


model_data <- successes[trials, on = c('day', 'station_from')]
model_data[, day_f := factor(day)]
model_data[, ':='(station_from = as.factor(station_from),
                  station_to = as.factor(station_to))]

fwrite(model_data, 'model output/model_data.csv')


#########################
library(data.table)
model_data <- fread('model output/model_data.csv')

library(sf)
nan <- st_read('data/raw/geo/NHD_H_0208_HU4_GDB.gdb',
               query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan <- st_read('data/raw/geo/NHD_H_0208_HU4_GDB.gdb',
               layer = 'nhdarea',
               wkt_filter = nan$wkt)


seg <- st_crop(st_transform(nan, 4326),
               xmin = -75.7662, ymin = 38.536,
               xmax = -75.755, ymax = 38.548)

plot(st_geometry(seg))







library(mgcv)

# job::job({
#   m2 <- bam(cbind(N, trials) ~ te(lat_from, lon_from, by = station_to, bs = 'cr'),
#             data = model_data,
#             family = binomial(),
#             discrete = T)
# }, import = c(model_data), packages = 'mgcv')
m2 <- readRDS('model output/from_by_stationto.rds')

# job::job({
#   m_GI <- bam(cbind(N, trials) ~
#                 te(lat_from, lon_from, bs = 'cr', m = 2) +
#                 te(lat_from, lon_from, by = station_to, bs = 'cr', m = 1),
#               data = model_data,
#               family = binomial(),
#               discrete = T)
# }, import = c(model_data), packages = 'mgcv')

# saveRDS(m_GI, 'model output/GI_from_by_stationto.RDS')
m_GI <- readRDS('model output/GI_from_by_stationto.RDS')



# job::job({
#   m_bystto_rDate <- bam(cbind(N, trials) ~
#                           station_to +
#                           te(lat_from, lon_from, by = station_to, bs = 'cr') +
#                           s(day_f, bs = 're'),
#                         data = model_data,
#                         family = binomial(),
#                         discrete = T)
# }, import = c(model_data), packages = 'mgcv')
#
# saveRDS(m_bystto_rDate, 'model output/by_stationto_rDate.RDS')

m_bystto_rDate <- readRDS('model output/by_stationto_rDate.RDS')


k <- st_make_grid(seg, n = c(100, 100))
k <- k[st_covered_by(k, seg, sparse = F)]
kk <- st_coordinates(st_centroid(k))

pred <- data.table(kk)
setnames(pred, c('lon_from', 'lat_from'))



dumb_fun <- function(station){
  pred[,':='(station_to = station,
             day_f = '2021-08-20')]

  pred[, preds := predict(m_bystto_rDate, newdata = pred, type = 'response',
                          exclude = 's(day_f)',
                          newdata.guaranteed = TRUE)]

  pred[, geom := st_geometry(k)]

  ggplot() +
    geom_sf(data = pred, aes(geometry = geom, fill = preds), color = NA) +
    geom_point(data = unique(model_data, by = c('lat_from', 'lon_from')),
               aes(x = lon_from, y = lat_from,
                   shape = station_from == station,
                   size = station_from == station,
                   color = station_from == station),
               show.legend = F) +
    scale_fill_viridis_c()
}

dumb_fun('3e')




md <- st_as_sf(model_data[!is.na(lon_from)], coords = c('lon_from', 'lat_from'), crs = 4326)
md <- st_transform(md, 32618)
setDT(md)
md[, ':='(east_from = st_coordinates(geometry)[,1],
          north_from = st_coordinates(geometry)[,2],
          station_to = as.factor(station_to),
          day_f = as.factor(day_f))]

seg_proj <- st_transform(seg, 32618)

### Try a soap film smoother
bound <- st_buffer(seg_proj, 20)
# bound <- st_coordinates(bound)[,1:2]
# bound <- list(list(east_from = bound[,1], north_from = bound[,2]))

## grid for interp
grd <- st_make_grid(bound, n = c(20, 20), what = 'centers')
grd2 <- grd[st_within(grd, bound, sparse = F)]

bound <-  st_buffer(seg_proj, 50)


plot(grd)
plot(st_geometry(bound), add = T)
plot(grd2, add = T, col = 'red')
plot(unique(md, by = 'north_from')[, geometry], add = T, col = 'blue')

bound <- st_coordinates(bound)[,1:2]
bound <- list(list(east_from = bound[,1], north_from = bound[,2]))

grd2 <- data.frame(st_coordinates(grd2))
setnames(grd2, c('east_from', 'north_from'))
# grd2 <- grd2[-c(55),]


job::job({
  hmmm <- bam(cbind(N, trials) ~
                station_to +
                s(east_from, north_from, by = station_to,
                  bs = 'so', xt = list(bnd = bound), k = 60) +
                s(day_f, bs = 're'),
              data = md,
              family = binomial(), knots = grd2)
}, import = c(md, bound, grd2), packages = 'mgcv')

# troubleshooting....
  # "Error in smooth.construct.so.smooth.spec(object, dk$data, dk$knots) :
  # data outside soap boundary": possibly an issue with discrete = T



k <- st_make_grid(seg_proj, n = c(100, 100))
k <- k[st_covered_by(k, seg_proj, sparse = F)]
kk <- st_coordinates(st_centroid(k))

pred <- data.table(kk)
setnames(pred, c('east_from', 'north_from'))


  pred[,':='(station_to = '3e',
             day_f = '2021-08-20')]

  pred[, preds := predict(hmmm, newdata = pred, type = 'response',
                          newdata.guaranteed = TRUE)]

  pred[, geom := st_geometry(k)]

  ggplot() +
    geom_sf(data = pred, aes(geometry = geom, fill = preds), color = NA) +
    geom_point(data = unique(md, by = c('east_from', 'north_from')),
               aes(x = east_from, y = north_from,
                   shape = station_from == '3e',
                   size = station_from == '3e',
                   color = station_from == '3e'),
               show.legend = F) +
    scale_fill_viridis_c()


