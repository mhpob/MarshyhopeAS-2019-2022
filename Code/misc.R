library(data.table)

# List the VPS CSV files
vps_files <- list.files('p:/obrien/biotelemetry/marshyhope/vps/', pattern = '.csv', full.names = T)

# Import each file into a slot of a list
vps_files <- lapply(vps_files, fread,
                    col.names = function(.) tolower(gsub('[) (]', '_', .)))

# Combine the files into a data frame
vps <- rbindlist(vps_files)



known_tags <- data.table(
  transmitter = c(paste('A69-9001',
                        c(18009, 18010, 18980, 18981, 18983, 21065,
                          21069, 21071, 23903, 23904, 26352,
                          26353, 27543, 27545, 10157, 21909),
                        sep = '-'),
                  'A69-1602-29927',
                  paste('A69-1601',
                        c(60675, 60767, 60769, 60787, 60921, 60929,
                          60931, 60933, 60935, 60936),
                        sep = '-'),
                  'A69-1602-29926',
                  'A69-9006-12727'),
  type = c(rep('fish', 16), 'float through', rep('sync', 12)),
  owner = c(rep('MDNR', 14), 'DNREC', 'VCU', rep('UMCES', 13))
)


sturg_vps <- vps[known_tags[type == 'fish'], on = 'transmitter', nomatch = 0]

sturg_summ <- sturg_vps[, .N, by = .(yday(date_and_time__utc_), receiver)]


library(ggplot2)

ggplot(data = sturg_summ) +
  geom_line(aes(x = yday, y = N)) +
  facet_wrap(~receiver) +
  geom_vline(xintercept = yday(c('2020-08-27', '2020-10-11')), lty = 'dashed')
