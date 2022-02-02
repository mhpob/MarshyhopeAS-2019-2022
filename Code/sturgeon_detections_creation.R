# Last updated 2022-01-18
library(parallel); library(data.table)

# Import
## Make cluster for parallel computing
cl <- parallel::makeCluster(
  parallel::detectCores()
)

clusterEvalQ(cl, library(data.table))


## Pull in detections
detections <- c(
  list.files(file.path('p:/obrien/biotelemetry/detections/dnr',
                       c('marshyhope', 'nanticoke')),
             pattern = '^VR.*_201.*.csv', full.names = T),
  list.files(file.path('p:/obrien/biotelemetry/detections/dnr',
                       c('marshyhope', 'nanticoke')),
             pattern = 'Export_2020.*.csv', full.names = T),
  list.files(file.path('p:/obrien/biotelemetry/detections/dnr',
                       c('marshyhope', 'nanticoke')),
             pattern = 'All 2021.csv', full.names = T)
)
detections <- parLapply(
  cl, detections,
  fread, fill = T,
  select = c('Date and Time (UTC)', 'Receiver', 'Transmitter',
             'Station Name', 'Latitude', 'Longitude'),
  col.names = function(.) tolower(gsub('and|UTC|[) (]', '', .))
)

## Close cluster
parallel::stopCluster(cl)


## Bind list
detections <- detections[sapply(detections, nrow) > 0]
detections <- rbindlist(detections, fill = T)

# Select sturgeon tagged as of 2021-10
deployed_tags <- fread('data/raw/embargo/sturgeon capture data.csv')
detections <- detections[transmitter %in% deployed_tags$TransmitterNumber]




# General manipulation
## Add local date
detections[, date.local := datetime]
setattr(detections$date.local, 'tzone', 'America/New_York')


## Rename columns and reorder
setnames(detections,
         c('datetime', 'stationname', 'latitude', 'longitude'),
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
