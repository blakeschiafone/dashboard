#' this file gets a list of namespaces that are tracked for each billing day
#' use this list as a lookup-table to determine what namespaces are 'active' 
#' for each billing date.  this is useful because it will help filter dashboard
#' namespace level tables to only include 'active' namespaces.
library(bigrquery)
setwd('/home/rstudio/scripts/dashboard')

map_query <- paste0("SELECT 
namespace, 
timestamp(date) as date
FROM [tap-nexus:kahuna_users.kahuna_billing]
where current_total_users > 0 and not
namespace contains '_qa' and not 
namespace contains '_dev' and not
namespace contains 'kahuna'")

map_volume <- query_exec(query = map_query, project = 'kahuna-bq-access', max_pages = Inf)


write.csv(map_volume, file = '/home/rstudio/scripts/dashboard/data/daily_active_namespaces.csv', row.names = FALSE)