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
# detections <- detections[ts <= as.POSIXct('2020-11-03 00:00:00', tz = 'UTC')]
# detections <- det[ts %between% c(as.POSIXct('2020-09-01 00:00:00', tz = 'UTC'),
                                 # as.POSIXct('2020-10-01 00:00:00', tz = 'UTC'))]

library(readxl)
excel_sheets('data/raw/vps-nanticokeriver-brookview-01.xls')

hydro_loc <- read_excel('data/raw/vps-nanticokeriver-brookview-01.xls',
                        'GPS Measurements',
                        skip = 1)

# Remove Brookview stations
hydro_loc <- hydro_loc[-c(1,3),]

hydro_loc <- st_as_sf(hydro_loc, coords = c('Longitude', 'Latitude'), crs = 4326,
                      remove = F) %>%
  st_transform('+proj=aeqd +lat_0=38.574224 +lon_0=-75.789577 +ellps=WGS84')

setDT(hydro_loc)

hydro_ser <- read_excel('data/raw/vps-nanticokeriver-brookview-01.xls',
                        'Stations',
                        skip = 1)
setDT(hydro_ser)
hydro_ser[2:3, `End Time` := hydro_ser$`End Time`[4:5]]
hydro_ser <- hydro_ser[-c(1, 4:5),]
# hydro_ser <- hydro_ser[-c(1:7),]
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
  st_transform('+proj=aeqd +lat_0=38.574224 +lon_0=-75.789577 +ellps=WGS84') %>%
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



wtemp <- fread('data/raw/marshyhopevps_event_export.csv',
               col.names = function(.) tolower(gsub('[) (]', '', .)))
wtemp <- wtemp[description == 'Temperature'][, data := as.numeric(data)]
wtemp <- wtemp[, .(bt = mean(data)),
               by = 'dateandtimeutc']
wtemp[, ':='(ts = dateandtimeutc,
             ss = tempToSs(temp = bt, sal = 0, depth = 5))]

ex <- list(hydros = copy(hydros)[, .(serial, x, y, z, sync_tag, idx)],
           detections = copy(detections)[
             tag %in% hydros$sync_tag &
               ts <= as.POSIXct('2020-10-12 00:00:00', tz = 'UTC'),
             .(ts, tag, epo, frac, serial)],
           ss = copy(wtemp)[ts <= as.POSIXct('2020-10-12 00:00:00', tz = 'UTC'),
                            .(ts, ss)])


k <- getInpSync(sync_dat = ex,
                max_epo_diff = 230,
                min_hydros = 5,
                time_keeper_idx = 4, #546211 had the lowest drift (~1s)
                fixed_hydros_idx = 3:12,
                n_offset_day = 2,
                n_ss_day = 2,
                keep_rate = 165,
                excl_self_detect = T,
                ss_data_what = 'data',
                ss_data = ex$ss
                )

getSyncCoverage(k, plot=TRUE)

sync_model <- getSyncModel(k, silent=F, tmb_smartsearch = T, max_iter = 500)

plotSyncModelHydros(sync_model)

plotSyncModelResids(sync_model, by = "overall")
plotSyncModelResids(sync_model, by = "quantiles")
plotSyncModelResids(sync_model, by = "sync_tag")
plotSyncModelResids(sync_model, by = "hydro")
plotSyncModelResids(sync_model, by = "temporal_hydro")
plotSyncModelResids(sync_model, by = "temporal_sync_tag")

j <- fineTuneSyncModel(sync_model, eps_threshold=1E4, silent=TRUE)
j <- fineTuneSyncModel(j, eps_threshold=1E3, silent=TRUE)

plotSyncModelResids(j, by = "overall")
plotSyncModelResids(j, by = "quantiles")
plotSyncModelResids(j, by = "sync_tag")
plotSyncModelResids(j, by = "hydro")
plotSyncModelResids(j, by = "temporal_hydro")
plotSyncModelResids(j, by = "temporal_sync_tag")
plotSyncModelHydros(j)
plotSyncModelCheck(j, by = "hydro")
plotSyncModelCheck(j, by = "sync_tag")
plotSyncModelCheck(j, by = "sync_bin_sync")
plotSyncModelCheck(j, by = "sync_bin_hydro")


jj <- fineTuneSyncModel(j, eps_threshold=1E2, silent=TRUE)
plotSyncModelResids(jj, by = "overall")
plotSyncModelResids(jj, by = "quantiles")
plotSyncModelResids(jj, by = "sync_tag")
plotSyncModelResids(jj, by = "hydro")
plotSyncModelResids(jj, by = "temporal_hydro")
plotSyncModelResids(jj, by = "temporal_sync_tag")
plotSyncModelHydros(jj)
plotSyncModelCheck(jj, by = "hydro")
plotSyncModelCheck(jj, by = "sync_tag")
plotSyncModelCheck(jj, by = "sync_bin_sync")
plotSyncModelCheck(jj, by = "sync_bin_hydro")


k3 <- getInpSync(sync_dat = ex,
                max_epo_diff = 250,
                min_hydros = 3,
                time_keeper_idx = 2, #546211 had the lowest drift (~1s)
                fixed_hydros_idx = 1:10,
                n_offset_day = 2,
                n_ss_day = 2,
                keep_rate = 85,
                excl_self_detect = F,
                ss_data_what = 'data',
                ss_data = ex$ss
)

getSyncCoverage(k2, plot=TRUE)
sync_model <- getSyncModel(k2, silent=F, max_iter = 500, tmb_smartsearch = T)


plotSyncModelResids(sync_model, by = "overall")
plotSyncModelResids(sync_model, by = "quantiles")
plotSyncModelResids(sync_model, by = "sync_tag")
plotSyncModelResids(sync_model, by = "hydro")
plotSyncModelResids(sync_model, by = "temporal_hydro")
plotSyncModelResids(sync_model, by = "temporal_sync_tag")
plotSyncModelHydros(sync_model)
plotSyncModelCheck(sync_model, by = "hydro")
plotSyncModelCheck(sync_model, by = "sync_tag")
plotSyncModelCheck(sync_model, by = "sync_bin_sync")
plotSyncModelCheck(sync_model, by = "sync_bin_hydro")

j <- fineTuneSyncModel(sync_model, eps_threshold=1E4, silent=F)
