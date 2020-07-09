library(TelemetryR)

dat <- vemsort('p:/obrien/biotelemetry/detections/dnr/marshyhope')

library(data.table)
dat <- setDT(dat)
unique(dat, by = c('date.utc', 'receiver', 'transmitter'))

dat <- dat[transmitter %in% paste0('A69-9001-', c(seq(21063, 21072, 1),
                                                    seq(23900, 23904, 1),
                                                    seq(26350, 26354, 1),
                                                    seq(27543, 27547, 1),
                                                    seq(18009, 18010, 1),
                                                    seq(18977, 18979, 1)))]



dat <- dat[, station := gsub(".*/|\\d.|'| Rd.", '', station)]
dat <- dat[, station := ifelse(station == 'Grey Cabin', 'Palmers Mill', station)]
dat <- dat[!year(date.utc) %in% c(2014, 2015)]
dat <- unique(dat, by = c('date.utc', 'station', 'transmitter'))
dat <- dat[, station := factor(station,
                                 levels = c('Marshyhope confluence',
                                            'Red Bank',
                                            'Walnut Landing',
                                            'Above Walnut Landing',
                                            'Below Brookview',
                                            'Brookview buoy',
                                            'Above Brookview Bridge',
                                            'Below Puckums Branch',
                                            'Puckums Branch',
                                            'Below Bridge',
                                            'Palmers Mill',
                                            'Pump House',
                                            'Wright',
                                            'Below Yellow House',
                                            'Yellow House',
                                            'Fooks pier',
                                            'VFW speed limit buoy',
                                            'Federalsburg'),
                                 ordered = T)]

dat <- dat[, year := year(date.local)]

tab <- as.data.frame(prop.table(xtabs(~station + year, data = dat), margin = 2)*100)

library(ggplot2)

ggplot() +
  geom_col(data = tab, aes(x = station, y = Freq,
                            fill = as.factor(year)),
           position = position_dodge2(preserve = 'single')) +
  annotate('rect', ymin = -0.05, ymax = 17, xmin = 5.5, xmax = 7.5,
           color = 'red', fill = NA, size = 1) +
  annotate('rect', ymin = -0.05, ymax = 17, xmin = 9.5, xmax = 10.5,
           color = 'red', fill = NA, size = 1) +
  scale_fill_viridis_d() +
  labs(x = '', y = '% Detections per Year', fill = 'Year') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, size = 11, hjust = 1),
        legend.position = c(0.05, 0.85),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12))
 
