## Attempting to re-create analyses found in Bruneel et al. 2020
## https://doi.org/10.1016/j.ecoinf.2020.101133
##  Code is embargoed until 2020-07-17

library(data.table)


# Import and munge data ----
dets <- fread('data/derived/sturgeon_detections.gz')

## Will have to run this by year, as there are a different number of receivers
dets <- dets[, year := year(date.local)]


## TRIAL RUN: SELECT ONLY 2017 and transmitter #23901
dets <- dets[year == 2017 & grepl('23901', transmitter)]
##



# Identify when fish move to a different station ----
##  Order each transmitter from earliest to latest detection
setorder(dets, transmitter, date.utc)

##  Make a dummy column of the stations shifted up one ("shift()")
##  "by = transmitter" makes sure each transmitter "shifts" separately
##  Will need to add year to "by = " when running multiple years
dets <- dets[, c('next_station', 'next_time') := shift(.SD,
                                                       type = 'lead', fill = '-999'),
             .SDcols= c('station', 'date.utc'), by = 'transmitter']

#$ Drop last row
dets <- dets[-nrow(dets)]

dets[, grp := rleid(station, next_station)]
dets[, dt := next_time - date.utc]

d2 <- dets[, .(dt_1 = sum(dt), n = .N),
           by = c('grp', 'station', 'next_station', 'transmitter')]

# If the fish is resident at at site (n >1), there's higher uncertainty.
d2[, ':='(lag = shift(dt_1, type = 'lag'),
          lead = shift(dt_1, type = 'lead'))]
d2[, dt_2 := fifelse(n > 1, sum(dt_1, lag, lead, na.rm = T), dt_1), by = grp]
d2[, uncertainty := dt_2 - dt_1]


# Restart here



# Find distances between sites ----
dist_df <- fread('data/derived/receiver_distances.csv')

# For now, these are only marshyhope stations. need to find nanticoke distances later
test <- dist_df[dets, on = c(from = 'station', to = 'next_station'), nomatch = 0]
