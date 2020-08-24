library(TelemetryR); library(data.table)

# Import
## Make cluster for parallel computing
cl <- parallel::makeCluster(
  parallel::detectCores()
)

## Pull in detections
detections <- lapply(file.path('p:/obrien/biotelemetry/detections/dnr',
                               c('marshyhope', 'nanticoke')),
                     vemsort, clust = cl)
detections <- rbindlist(detections)


## Close cluster
parallel::stopCluster(cl)



# Select sturgeon tagged as of 2020-09
detections <- detections[transmitter %in%
  # MDNR transmitters
  paste0('A69-9001-',
         c(# MDNR transmitters
           seq(21063, 21072, 1),
           seq(23900, 23904, 1),
           seq(26350, 26354, 1),
           seq(27543, 27547, 1),
           seq(18009, 18010, 1),
           seq(18977, 18979, 1),
           # DNREC transmitters
           10157,
           seq(21060, 21062, 1)))]



# Drop unused columns
detections <- detections[, .(date.utc, date.local, transmitter,
                             station, lat, long)]



# Export
fwrite(detections, file = 'data/derived/sturgeon_detections.gz', dateTimeAs = 'write.csv')
