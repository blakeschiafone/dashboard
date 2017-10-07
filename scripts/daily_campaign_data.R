#' this file pulls campaign counts across different channels and campaign types
#' for each day.  the metrics pulled are: 
#' 1. campaign count by channel: push, email, in-app
#' 2. campaign count by type: experiences, lifecycle, conversion, adaptive, one-time, scheduled


library(RPostgreSQL)
source('/home/rstudio/scripts/db_connection.R')


#' get data and count campaigns by channel
channel_campaign_count <- dbGetQuery(db_connection,
                                   "select 
                                    'channel' as type,
                                    table_id, 
                                    date,
                                    channel,
                                    count(distinct(campaign_group_id)) as campaigns
                                    from bq.counters
                                    where name in ('delivered', 'ia_delivered') and date >= '2017-01-01' and label = 'non-control' and total > 0
                                    group by type, table_id, date, channel
                                   ")


#' get data and count campaigns by type
type_campaign_count <- dbGetQuery(db_connection,
                                     "select 
                                    'campaign' as type,
                                    table_id, 
                                    date,
                                    campaign_type,
                                    count(distinct(campaign_group_id)) as campaigns
                                    from bq.counters
                                    where name in ('delivered', 'ia_delivered') and date >= '2017-01-01' and label = 'non-control' and total > 0
                                    group by type, table_id, date, campaign_type
                                   ")

#' combine data-frames to one
campaign_count_data <- as.data.frame(Map(c, channel_campaign_count, type_campaign_count))

write.csv(campaign_count_data, file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv', row.names = FALSE)