library(parallel); library(data.table)

# Import
## Make cluster for parallel computing
cl <- parallel::makeCluster(
  parallel::detectCores()
)

clusterEvalQ(cl, library(data.table))


## Pull in detections
detections <- list.files(file.path('p:/obrien/biotelemetry/detections/dnr',
                                   c('marshyhope', 'nanticoke')),
                         pattern = '^VR.*.csv', full.names = T)
detections <- parLapply(
  cl, detections,
  fread, fill = T,
  select = c('Date and Time (UTC)', 'Receiver', 'Transmitter',
             'Station Name', 'Latitude', 'Longitude'),
  col.names = function(.) tolower(gsub('[) (]', '', .))
)

## Close cluster
parallel::stopCluster(cl)


## Bind list
detections <- detections[sapply(detections, nrow) > 0]
detections <- rbindlist(detections, fill = T)

# Select sturgeon tagged as of 2021-10
detections <- detections[transmitter %in%
  # MDNR transmitters
  paste0('A69-9001-',
         c(# MDNR transmitters
           seq(21063, 21072, 1),
           seq(23900, 23904, 1),
           seq(26350, 26354, 1),
           seq(27543, 27547, 1),
           seq(18009, 18010, 1),
           seq(18977, 18985, 1),
           # DNREC transmitters
           10157,
           seq(21060, 21062, 1)))]

detections[, date.local := dateandtimeutc]
setattr(detections$date.local, 'tzone', 'America/New_York')
setnames(detections,
         c('dateandtimeutc', 'stationname', 'latitude', 'longitude'),
         c('date.utc', 'station', 'lat', 'long'))
setorder(detections, date.utc)

# Arrange columns
detections <- detections[, .(date.utc, date.local, transmitter,
                             station, receiver, lat, long)]



# Export
fwrite(detections, file = 'data/derived/sturgeon_detections.gz',
       dateTimeAs = 'write.csv')
