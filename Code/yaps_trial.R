library(data.table); library(sf); library(yaps)

vue <- data.table::fread('data/raw/vueexport_2020_timecorrected.csv',
                         fill=TRUE, tz = '')

prepDetections_gsub <- function(raw_dat, type){
  detections <- data.table::data.table()
  if (type == "vemco_vue"){
    detections[, ts:=as.POSIXct(raw_dat$'Date and Time (UTC)', tz="UTC")]
    detections[, tag:=as.numeric(gsub('.*-', '', raw_dat$Transmitter))]
    detections[, epo:=as.numeric(ts)]
    detections[, frac:= as.numeric(gsub('.*\\.', '', raw_dat$"Date and Time (UTC)"))/1000]
    detections[, serial:=as.numeric(gsub('.*-', '', raw_dat$Receiver))]
  }
  detections[]
  return(detections)
}



detections <- prepDetections_gsub(vue, 'vemco_vue')
detections[is.na(frac), frac := 0]

# Remove detections after VR2ARs started to be pulled
detections <- detections[ts <= '2020-11-03 16:00:00']


library(readxl)
excel_sheets('data/raw/vps-nanticokeriver-brookview-01.xls')

hydro_loc <- read_excel('data/raw/vps-nanticokeriver-brookview-01.xls',
                        'GPS Measurements',
                        skip = 1)

# Remove Brookview stations
hydro_loc <- hydro_loc[-c(1:4),]
hydro_loc <- st_as_sf(hydro_loc, coords = c('Longitude', 'Latitude'), crs = 4326,
                      remove = F) %>%
  st_transform('+proj=aeqd +lat_0=38.574224 +lon_0=-75.789577 +x_0=750 +y_0=750 +ellps=WGS84')

setDT(hydro_loc)

hydro_ser <- read_excel('data/raw/vps-nanticokeriver-brookview-01.xls',
                        'Stations',
                        skip = 1)
setDT(hydro_ser)
# hydro_ser[2:3, `End Time` := hydro_ser$`End Time`[4:5]]
# hydro_ser <- hydro_ser[-c(1, 4:5),]
hydro_ser <- hydro_ser[-c(1:7),]
hydro_ser[, Name := Name[nafill(replace(.I, is.na(Name), NA), "locf")]]
hydro_ser[, Latitude := Latitude[nafill(replace(.I, is.na(Latitude), NA), "locf")]]
hydro_ser[, Longitude := Longitude[nafill(replace(.I, is.na(Longitude), NA), "locf")]]
hydro_ser[, serial := as.numeric(gsub('.*-', '', Device))]

detections[, ts_stop := ts]
key <- copy(hydro_ser)[grepl('^V', Device)]
setkey(key, serial, `Start Time`, `End Time`)

detections <- foverlaps(detections, key,
                by.x = c('serial', 'ts', 'ts_stop'),
                by.y = c('serial', 'Start Time', 'End Time'),
                nomatch = 0)



hydro_ser <- hydro_ser %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>%
  st_transform('+proj=aeqd +lat_0=38.574224 +lon_0=-75.789577 +x_0=750 +y_0=750 +ellps=WGS84') %>%
  data.table()
hydro_ser[, ':='(X = st_coordinates(geometry)[, 1],
                 Y = st_coordinates(geometry)[, 2],
                 Z = `Device Depth`)]

hydros <- data.table(
  hydro_ser[grepl('^VR2', Device), .(serial = serial,
                                     x = st_coordinates(geometry)[, 1],
                                     y = st_coordinates(geometry)[, 2],
                                     z = `Device Depth`,
                                     idx = .I)],
  hydro_ser[grepl('^A', Device), .(sync_tag = as.numeric(gsub('.*-', '', Device)))]
)

ex <- list(hydros = copy(hydros),
           detections = copy(detections))


wtemp <- fread('data/raw/marshyhopevps_event_export.csv',
               col.names = function(.) tolower(gsub('[) (]', '', .)))
wtemp <- wtemp[description == 'Temperature'][, data := as.numeric(data)]
wtemp <- wtemp[, .(bt = mean(data)),
               by = 'dateandtimeutc']
wtemp[, ':='(ts = dateandtimeutc,
             ss = tempToSs(temp = bt, sal = 0, depth = 5))]


k <- getInpSync(sync_dat = ex,
                max_epo_diff = 250,
                min_hydros = 2,
                time_keeper_idx = 2,
                fixed_hydros_idx = 1:10,
                n_offset_day = 2,
                n_ss_day = 2,
                keep_rate = 100,
                excl_self_detect = F,
                ss_data_what = 'data',
                ss_data = wtemp
                )

getSyncCoverage(k, plot=TRUE)


sync_model <- getSyncModel(k, silent=F, max_iter=500, tmb_smartsearch = T)
