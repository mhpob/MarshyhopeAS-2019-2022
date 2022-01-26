library(data.table)

vps <- list.files('p:/obrien/biotelemetry/marshyhope/vps 2021/files',
                  pattern = '*.csv', full.names = T)


vps <- lapply(vps, fread, col.names = function(.) tolower(gsub('[) ()]', '', .)),
              fill = T)

vps <- rbindlist(vps)
unique(vps$transmitter)

vps[grepl('9001', transmitter),
    .(min = min(dateandtimeutc),
      max = max(dateandtimeutc)),
    by = transmitter]

events <- fread('p:/obrien/biotelemetry/marshyhope/vps 2021/VUE_Export_20210930.csv')
events <- events[grepl('Temp', Description)]
events[, Data := as.numeric(Data)]
events <- events[`Date and Time (UTC)` > '2021-08-19 21:00:00']

library(ggplot2)

k <- vps[grepl('9001', transmitter)]

ggplot(data = events) +
  geom_line(aes(x = `Date and Time (UTC)`, y = Data, group = Receiver)) +
  geom_vline(xintercept = k$dateandtimeutc) +
  labs(y = 'Temperature (C)') +
  theme_minimal()


