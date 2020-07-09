## Attempting to re-create analyses found in Bruneel et al. 2020
## https://doi.org/10.1016/j.ecoinf.2020.101133
##  Code is embargoed until 2020-07-17

library(data.table)


# Import and munge data ----
dets <- fread('data/derived/sturgeon_detections.gz')

## Will have to run this by year, as there are a different number of receivers
dets <- dets[, year := year(date.local)]


## TRIAL RUN: SELECT ONLY 2017 ##
dets <- dets[year == 2017]
##



# Identify when fish move to a different station ----
##  Order each transmitter from earliest to latest detection
setorder(dets, transmitter, date.local)

##  Make a dummy column of the stations shifted up one ("shift()")
##  "by = transmitter" makes sure each transmitter "shifts" separately
##  Will need to add year to "by = " when running multiple years
dets <- dets[, next_station := shift(station, fill = '-999'), by = transmitter]

##  Select detections where the next station is not the same as the current station
dets <- dets[station != next_station]

##  Drop "next_station" column
dets <- dets[, next_station := NULL]



# Find distances between sites ----
