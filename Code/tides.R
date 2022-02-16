library(data.table)
events <- fread('p:/obrien/biotelemetry/marshyhope/vps 2021/vue_export_20210930.csv')

tilt <- events[grepl('Tilt', Description)]

tilt[, Data := as.numeric(Data)]


library(lubridate)


tilt[, hr4 := floor_date(`Date and Time (UTC)`, '4 hours')]

k <- tilt[, .(d4 = median(Data)), by = c('hr4', 'Receiver')]


library(ggplot2)
ggplot(data = k[hr4 > '2021-08-20']) +
  geom_line(aes(x = hr4, y = d4, group = Receiver)) +
  facet_wrap(~ Receiver, scales = 'free_y')

plot(x = k[grepl('467', Receiver)]$d4 - median(k[grepl('467', Receiver)]$d4),
     y = k[grepl('471', Receiver)]$d4 - median(k[grepl('471', Receiver)]$d4), data = k)
abline(a = 0,b=1)

tides <- fread('p:/obrien/biotelemetry/marshyhope/sharptown_tides.txt',
               skip = 2)
tides <- tides[-(1:2)]
setnames(tides, c('agency', 'station', 'date', 'tz', 'data', 'code'))

tides[, date := as.POSIXct(date, tz = 'America/New_York')]
tides[, hr4 := floor_date(date, '4 hours')]
tides <- tides[, .(level = median(as.numeric(data))), by = 'hr4']

# plot.ts(tides$level)
ggplot(data = tides, aes(x = hr4, y= level)) +
  geom_line()+
  geom_smooth(formula = level ~ mgcv::s(hr4))

