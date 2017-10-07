#' this file pulls product adoption data at the namespace level
#' for each day.  the metrics pulled are: 
#' 1. optimization adoption
#' 2. personalization adoption
#' 3. auto adoption


library(RPostgreSQL)
source('/home/rstudio/scripts/db_connection.R')

#' get data and group proportion of messages sent (conversion + lifecycle + experiences / total volume)
adoption_auto_volume <- dbGetQuery(db_connection,
                                   "select 
                                    'automatic' as type,
                                    table_id, 
                                    date,
                                    sum(case when campaign_type in ('auto', 'trigger', 'program_message') then total else 0 end) as auto_volume,
                                    sum(total) as total_volume
                                    from bq.counters
                                    where name in ('delivered', 'ia_delivered') and date >= '2017-01-01' and label = 'non-control'
                                    group by type, table_id, date
                                   ")


#' get data and group proportion of messages sent (message copy personalization / total volume)
adoption_personalization_volume <- dbGetQuery(db_connection,
                                    "select 
                                    'personalized' as type,
                                    table_id,
                                    date,
                                    sum(case when message_text LIKE '%{{%' then total else 0 end) as personalization_volume,
                                    sum(total) as total_volume
                                    from
                                    (select * from bq.counters
                                    where name in ('delivered', 'ia_delivered') and date >= '2017-01-01' and label = 'non-control') as counters
                                    left join
                                    (select * from bq.campaigns) as campaigns
                                    on counters.key = campaigns.key
                                    group by type, table_id, date
                                   ")


#' get data and group proportion of messages sent (optimiztation / total volume)
adoption_optimization_volume <- dbGetQuery(db_connection,
                                    "select 
                                    'optimized' as type,
                                    table_id,
                                    date,
                                    sum(case when algo_flag = 'true' then total else 0 end) as optimization_volume,
                                    sum(total) as total_volume
                                    from
                                    bq.counters
                                    where name in ('delivered', 'ia_delivered') and date >= '2017-01-01' and label = 'non-control'
                                    group by type, table_id, date
                                   ")


#' create dataframe to hold 3 files
adoption_data <- data.frame(Map(c,adoption_auto_volume, adoption_personalization_volume, adoption_optimization_volume))
names(adoption_data) <- c('type', 'table_id', 'date', 'adoption_volume', 'total_volume')

#' write data
write.csv(adoption_data, file = '/home/rstudio/scripts/dashboard/data/adoption_data.csv', row.names = FALSE)
