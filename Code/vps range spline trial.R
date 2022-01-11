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
