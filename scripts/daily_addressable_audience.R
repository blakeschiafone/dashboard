#' this file pulls daily addressable audience counts by channel


library(bigrquery)
setwd('/home/rstudio/scripts/dashboard')

table_dates <- seq(from = lubridate::floor_date(Sys.Date(), unit = 'month'), by = '-1 month', length.out = 2)
table_dates <- gsub('-', '', table_dates)

query <- paste0("select
substr(_TABLE_SUFFIX, 1, length(_TABLE_SUFFIX) - 9) as table_id,
day,
channel,
count
from `tap-nexus.kahuna_addressable_audience.*`
where substr(_TABLE_SUFFIX, -8, 8) in ('", table_dates[1], "' , '", table_dates[2], "') and
not _TABLE_SUFFIX like '%kahuna%' and
not _TABLE_SUFFIX like '%demo%' and
not _TABLE_SUFFIX like '%test%' and
not _TABLE_SUFFIX like '%_qa%' and
not _TABLE_SUFFIX like '%_sandbox%' and
measure = 'addressable_audience'")


results <- query_exec(query = query, project = 'kahuna-bq-access', max_pages = Inf, useLegacySql = FALSE)


write.csv(results, file = '/home/rstudio/scripts/dashboard/data/addressable_audience.csv', row.names = FALSE)