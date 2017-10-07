#' get daily data from client success
library(httr)

auth_key <- function(x){
  #' get auth key
  auth_key <- POST(url = 'https://api.clientsuccess.com/v1/auth',
                   body = list(username='blake@kahuna.com', password = 'bskahuna17'),
                   encode = 'form')
  
  auth_key <- httr::content(auth_key)$access_token
}


employee_list <- function(auth_key){
  #' get client list, only possible by specifying each CSM
  employees <- GET(url = 'https://api.clientsuccess.com/v1/employees',
                    add_headers('Authorization' = auth_key))
  
  employees <- httr::content(employees)
  
  #' get CSM index value
  csm_index <- which(map_chr(employees, 'lastName') %in% c('Cho', 'Kim', 'Harewood', 'Lazarevsky'))
  csm_id <- map_int(employees, 'id')[csm_index]
  csm_id
}


client_list <- function(employee_id, auth_key){
  #' returns all current customers plus some additional information
  clients <- GET(url = sprintf('https://api.clientsuccess.com/v1/clients?assignedCsmId=%s&activeOnly=true', employee_id),
                 add_headers('Authorization' = auth_key))
  clients <- httr::content(clients)
}


client_details <- function(client_id, auth_key){
  #' returns all details about a specific customer
  details <- GET(url = sprintf('https://api.clientsuccess.com/v1/clients/%s', client_id),
                 add_headers('Authorization' = auth_key))
  details <- httr::content(details)
}


client_contacts <- function(client_id, auth_key){
  #' returns all contacts attached to a specific client_id
  contacts <- GET(url = sprintf('https://api.clientsuccess.com/v1/clients/%s/contacts', client_id),
                  add_headers('Authorization' = auth_key))
  contacts <- httr::content(contacts)
  contacts
}
