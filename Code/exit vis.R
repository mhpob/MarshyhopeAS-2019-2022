library(data.table)

det1 <- fread('data/derived/sturgeon_detections.gz')

det2 <- fread('p:/obrien/biotelemetry/detections/dnr/vue_export_2020.csv',
              col.names = function(.) tolower(gsub('[) ()]', '', .)),
              fill = T)

rec_rkm <- fread('data/derived/mddnr_receiver_rkm.csv')

lengths <- fread('data/raw/sturgeon capture data.csv')


det2 <- det2[transmitter %in% lengths$TransmitterNumber |
               transmitter == 'A69-9001-21071']

setnames(det2, c('dateandtimeutc', 'stationname', 'latitude', 'longitude'),
         c('date.utc', 'station', 'lat', 'long'))

det2[, date.local := lubridate::with_tz(date.utc, 'America/New_York')]
det2 <- det2[, c("date.utc", "date.local", "transmitter", "station", "lat", "long" )]


dets <- rbind(det1, det2)


dets <- rec_rkm[dets, on = 'station']
dets[, doy := as.Date('2020-01-01') + (yday(date.local) - 1)]
dets[, year := year(date.local)]

d <- dets[body == 'Marshyhope Creek']
d <- d[, .(rkm = median(rkm_body_mouth)), by = c('doy', 'year', 'transmitter')]

library(ggplot2)
ggplot() +
  geom_rect(data = d[, .(mx = max(doy)), by = c('year', 'transmitter')][
    , .(mn = min(mx), mx = max(mx)), by = 'year'],
    aes(ymin = -Inf, ymax = Inf, xmin = mn, xmax = mx),
    fill = 'lightgray') +
  geom_line(data = d,
            aes(x = doy, y = rkm,
                group = transmitter, alpha = doy),
            show.legend = F) +
  #RKM 4 (boy scout dock)
  geom_hline(yintercept = 4) +
  geom_vline(data = d[, .(mx = max(doy)), by = c('year', 'transmitter')][
    , .(med = median(mx)), by = 'year'],
    aes(xintercept = med), col='red') +
  labs(x = NULL) +
  facet_wrap(~year) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))

