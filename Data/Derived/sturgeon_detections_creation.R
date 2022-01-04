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




# General manipulation
## Add local date
detections[, date.local := dateandtimeutc]
setattr(detections$date.local, 'tzone', 'America/New_York')


## Rename columns and reorder
setnames(detections,
         c('dateandtimeutc', 'stationname', 'latitude', 'longitude'),
         c('date.utc', 'station', 'lat', 'long'))
setorder(detections, date.utc)


## Repair station names
##  Doing this in steps for the sake of (a little bit of?) clarity

### Create a lookup table (key)
key <- data.table(station = unique(detections$station))

### Remove leading numbers and any apostrophe
###   Regex plain English: "starts with a 1-2 digit number which may or may not
###     be followed by 'point number', which is then followed by a space
###     OR any apostrophe"
key[, repair := gsub("^\\d{1,2}(\\.\\d)? |'", '', station)]

### Remove "Marshyhope" and "Nanticoke" monikers
###   Regex plain English: "Marshyhope or Nanticoke, followed by any number of
###     spaces or slashes, possibly followed by a number then a space
key[, repair := gsub("(Marshyhope|Nanticoke)[ /]*(\\d )?", '', repair)]

### Remove the word "road" and abbreviations
key[, repair := gsub(' (Road|Rd.)', '', repair)]

### Fix the regex casualties
key[, repair := fifelse(grepl('[Cc]onfluence', repair), 'Marshyhope Confluence', repair)]
key[, repair := fifelse(grepl('mouth', repair), 'Nanticoke Mouth', repair)]

### Final individual changes
key[, repair := gsub('warf', 'Wharf', repair)]
key[, repair := fifelse(grepl('VFW', repair), 'VFW Boat Ramp', repair)]
key[, repair := fifelse(repair == 'Wright', 'Wrights Pier', repair)]

### Make everything have consistent casing
key[, repair := tools::toTitleCase(repair)]

### Join the lookup table
detections <- detections[key, on = 'station']

### Complete renaming and drop extra column
detections[, ':='(station = repair, repair = NULL)]



# Arrange columns
detections <- detections[, .(date.utc, date.local, transmitter,
                             station, receiver, lat, long)]


# Export
fwrite(detections, file = 'data/derived/sturgeon_detections.gz',
       dateTimeAs = 'write.csv')
