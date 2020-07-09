library(TelemetryR)

dat <- vemsort('p:/obrien/biotelemetry/detections/dnr/marshyhope')

library(data.table)
dat <- setDT(dat)

dat <- dat[transmitter %in% paste0('A69-9001-', c(seq(21063, 21072, 1),
                                                  seq(23900, 23904, 1),
                                                  seq(26350, 26354, 1),
                                                  seq(27543, 27547, 1),
                                                  seq(18009, 18010, 1),
                                                  seq(18977, 18979, 1)))]

dat <- dat[, year := year(date.local)]
