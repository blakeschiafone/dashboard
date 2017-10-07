library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(DT)
library(tidyr)
library(sparkline)
options(shiny.sanitize.errors = FALSE)


shinyServer(function(input,output,session){
  vals<-reactiveValues()
  vals$collapsed=FALSE
  observeEvent(input$SideBar_col_react,
               {
                 vals$collapsed=!vals$collapsed
               }
  )
  
  output$graph_differentiator <- renderPlot(height = 350, {
    differentiator <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/adoption_data.csv')
    
    #'####################################    
    #' get last value for each type value
    #'####################################
    differentiator %>%
      group_by(type, date) %>%
      summarize_if(is_numeric, sum) %>%
      mutate(ratio = adoption_volume / total_volume,
             auto_7 = zoo::rollsum(adoption_volume, 7, na.pad = TRUE),
             total_7 = zoo::rollsum(total_volume, 7, na.pad = TRUE),
             ratio_7 = auto_7 / total_7) -> differentiator
    
    differentiator %>%
      arrange(desc(date)) %>%
      slice(which(row_number() %in% c(1, 4))) %>%
      select(type, date, ratio, ratio_7) %>%
      mutate(ratio = ifelse(is.na(ratio_7), ratio, ratio_7)) %>%
      select(-c(ratio_7)) -> last_ratio_values
    
    differentiator %>%
      ggplot(., aes(x = date, y = ratio_7, group = type, color = type)) + 
      geom_line() +
      geom_line(data = last_ratio_values, aes(x = date, y = ratio, group = type, color = type), linetype = '21') +
      scale_color_manual(values = c('personalized' = '#ae0a45ff',
                                    'optimized' = '#27ab7eff',
                                    'automatic' = '#2a5191')) +
      geom_point(data = last_ratio_values[last_ratio_values$type == 'personalized' & last_ratio_values$date == Sys.Date() - 1,], 
                 aes(x = date, y = ratio), color = '#ae0a45ff', size = 3) + 
      geom_point(data = last_ratio_values[last_ratio_values$type == 'optimized' & last_ratio_values$date == Sys.Date() - 1,], 
                 aes(x = date, y = ratio), color = '#27ab7eff', size = 3) + 
      geom_point(data = last_ratio_values[last_ratio_values$type == 'automatic' & last_ratio_values$date == Sys.Date() - 1,], 
                 aes(x = date, y = ratio), color = '#2a5191', size = 3) + 
      geom_text(data = last_ratio_values[last_ratio_values$date == Sys.Date() - 1,], 
                aes(x = date, y = ratio, label = type), size = 3.5, hjust = 'left', nudge_x = 1) + 
      scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
      scale_x_date(limits = c(as.Date('2017-01-01'), lubridate::ceiling_date(last_ratio_values$date[1], 'month')), date_breaks = '1 month', date_labels = '%b %Y') +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        rect = element_blank(),
        line = element_blank(),
        legend.position = 'none'
      ) +
      guides(size = FALSE) +
      labs(
        x = '',
        y = ''
      ) -> differentiator
    
    print(differentiator)
  })
  
  #'####################################
  #' generate valueboxes for tile graphs
  #'####################################
  output$valuebox_push_count <- renderValueBox({
    campaign_count <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv')
    
    lapply(split(campaign_count[campaign_count$type == 'channel',], campaign_count$channel[campaign_count$type == 'channel']), function(df) {
      df %>%
        group_by(type, date) %>%
        summarize(campaigns = sum(campaigns)) %>%
        arrange(date) %>%
        mutate(roll_7 = zoo::rollmean(campaigns, 7, na.pad = TRUE, align = 'right')) %>%
        slice(which(row_number() == n()))
      }) -> campaign_count
    
    value_diff <- scales::percent((campaign_count$push$campaigns / campaign_count$push$roll_7) - 1)
    valueBox(value = paste0(scales::comma(campaign_count$push$campaigns), '  (', scales::comma(round(campaign_count$push$roll_7)), ')'), 
             subtitle = 'Push Campaigns', icon = icon('mobile'), color = ifelse(!grepl('-', value_diff), 'green', 'red'), width = 4)
  })
  
  output$valuebox_email_count <- renderValueBox({
    campaign_count <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv')
    
    lapply(split(campaign_count[campaign_count$type == 'channel',], campaign_count$channel[campaign_count$type == 'channel']), function(df) {
      df %>%
        group_by(type, date) %>%
        summarize(campaigns = sum(campaigns)) %>%
        arrange(date) %>%
        mutate(roll_7 = zoo::rollmean(campaigns, 7, na.pad = TRUE, align = 'right')) %>%
        slice(which(row_number() == n()))
    }) -> campaign_count
    
    value_diff <- scales::percent((campaign_count$email$campaigns / campaign_count$email$roll_7) - 1)
    valueBox(value = paste0(scales::comma(campaign_count$email$campaigns), '  (', scales::comma(round(campaign_count$email$roll_7)), ')'), 
             subtitle = 'Email Campaigns', icon = icon('envelope-o'), color = ifelse(!grepl('-', value_diff), 'green', 'red'), width = 4)
  })
  
  output$valuebox_inapp_count <- renderValueBox({
    campaign_count <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv')
    
    lapply(split(campaign_count[campaign_count$type == 'channel',], campaign_count$channel[campaign_count$type == 'channel']), function(df) {
      df %>%
        group_by(type, date) %>%
        summarize(campaigns = sum(campaigns)) %>%
        arrange(date) %>%
        mutate(roll_7 = zoo::rollmean(campaigns, 7, na.pad = TRUE, align = 'right')) %>%
        slice(which(row_number() == n()))
    }) -> campaign_count
    
    value_diff <- scales::percent((campaign_count$in_app$campaigns / campaign_count$in_app$roll_7) - 1)
    valueBox(value = paste0(scales::comma(campaign_count$in_app$campaigns), '  (', scales::comma(round(campaign_count$in_app$roll_7)), ')'), 
             subtitle = 'In-App Campaigns', icon = icon('picture-o'), color = ifelse(!grepl('-', value_diff), 'green', 'red'), width = 4)
  })
  
  output$infobox_experience_count <- renderInfoBox({
    campaign_count <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv')
      campaign_count %>%
        filter(channel %in% c('program_message'),
               type == 'campaign') %>%
        group_by(channel, date) %>%
        summarize(campaigns = sum(campaigns)) %>%
        arrange(date) %>%
        mutate(roll_7 = zoo::rollmean(campaigns, 7, na.pad = TRUE, align = 'right')) %>%
        slice(which(row_number() == n())) -> campaign_count
    
    value_diff <- scales::percent((campaign_count$campaigns / campaign_count$roll_7) - 1)
    infoBox(value = paste0(scales::comma(campaign_count$campaigns), '  (', scales::comma(round(campaign_count$roll_7)), ')'), 
             title = 'Experiences', color = ifelse(!grepl('-', value_diff), 'green', 'red'), width = 2.4)
  })
  
  output$infobox_lifecycle_count <- renderInfoBox({
    campaign_count <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv')
    campaign_count %>%
      filter(channel %in% c('auto'),
             type == 'campaign') %>%
      group_by(date) %>%
      summarize(campaigns = sum(campaigns)) %>%
      arrange(date) %>%
      mutate(roll_7 = zoo::rollmean(campaigns, 7, na.pad = TRUE, align = 'right')) %>%
      slice(which(row_number() == n())) -> campaign_count
    
    value_diff <- scales::percent((campaign_count$campaigns / campaign_count$roll_7) - 1)
    infoBox(value = paste0(scales::comma(campaign_count$campaigns), '  (', scales::comma(round(campaign_count$roll_7)), ')'), 
            title = 'Lifecycle', color = ifelse(!grepl('-', value_diff), 'green', 'red'), width = 3)
  })

  output$infobox_onetime_count <- renderInfoBox({
    campaign_count <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv')
    campaign_count %>%
      filter(channel %in% c('one_time'),
             type == 'campaign') %>%
      group_by(channel, date) %>%
      summarize(campaigns = sum(campaigns)) %>%
      arrange(date) %>%
      mutate(roll_7 = zoo::rollmean(campaigns, 7, na.pad = TRUE, align = 'right')) %>%
      slice(which(row_number() == n())) -> campaign_count
    
    value_diff <- scales::percent((campaign_count$campaigns / campaign_count$roll_7) - 1)
    infoBox(value = paste0(scales::comma(campaign_count$campaigns), '  (', scales::comma(round(campaign_count$roll_7)), ')'), 
            title = 'One Time', color = ifelse(!grepl('-', value_diff), 'green', 'red'), width = 3)
  })
  
  output$infobox_adaptive_count <- renderInfoBox({
    campaign_count <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv')
    campaign_count %>%
      filter(channel %in% c('adaptive'),
             type == 'campaign') %>%
      group_by(channel, date) %>%
      summarize(campaigns = sum(campaigns)) %>%
      arrange(date) %>%
      mutate(roll_7 = zoo::rollmean(campaigns, 7, na.pad = TRUE, align = 'right')) %>%
      slice(which(row_number() == n())) -> campaign_count
    
    value_diff <- scales::percent((campaign_count$campaigns / campaign_count$roll_7) - 1)
    infoBox(value = paste0(scales::comma(campaign_count$campaigns), '  (', scales::comma(round(campaign_count$roll_7)), ')'), 
            title = 'Adaptive', color = ifelse(!grepl('-', value_diff), 'green', 'red'), width = 3)
  })
  
  output$infobox_conversion_count <- renderInfoBox({
    campaign_count <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv')
    campaign_count %>%
      filter(channel %in% c('trigger', 'immediate_trigger'),
             type == 'campaign') %>%
      group_by(date) %>%
      summarize(campaigns = sum(campaigns)) %>%
      arrange(date) %>%
      mutate(roll_7 = zoo::rollmean(campaigns, 7, na.pad = TRUE, align = 'right')) %>%
      slice(which(row_number() == n())) -> campaign_count
    
    value_diff <- scales::percent((campaign_count$campaigns / campaign_count$roll_7) - 1)
    infoBox(value = paste0(scales::comma(campaign_count$campaigns), '  (', scales::comma(round(campaign_count$roll_7)), ')'), 
            title = 'Conversion', color = ifelse(!grepl('-', value_diff), 'green', 'red'), width = 3)
  })
  
  output$infobox_flex_count <- renderInfoBox({
    campaign_count <- readr::read_csv(file = '/home/rstudio/scripts/dashboard/data/campaign_count_data.csv')
    campaign_count %>%
      filter(channel %in% c('flex'),
             type == 'campaign') %>%
      group_by(date) %>%
      summarize(campaigns = sum(campaigns)) %>%
      arrange(date) %>%
      mutate(roll_7 = zoo::rollmean(campaigns, 7, na.pad = TRUE, align = 'right')) %>%
      slice(which(row_number() == n())) -> campaign_count
    
    value_diff <- scales::percent((campaign_count$campaigns / campaign_count$roll_7) - 1)
    infoBox(value = paste0(scales::comma(campaign_count$campaigns), '  (', scales::comma(round(campaign_count$roll_7)), ')'), 
            title = 'Flex', color = ifelse(!grepl('-', value_diff), 'green', 'red'), width = 3)
  })
  
  
  #' build namespace formattable table
  output$namespace_table <- renderDataTable({
    namespace_table.df <- load('/home/rstudio/scripts/dashboard/data/namespace_table_data.RData')
    active_namespaces <- readr::read_csv('/home/rstudio/scripts/dashboard/data/daily_active_namespaces.csv')
    active_namespaces <- active_namespaces %>% group_by(date) %>% arrange(date) %>% nest()
    
    #' return the most recent daily table
    return_namespace_table <- function(namespace_table, active_name, date_input = FALSE){
      a <- namespace_table
      b <- active_name
      date_input <- Sys.Date() - 1
      
      date_2nd_max <- b$date[order(b$date, decreasing = TRUE)][2]
      
      a %>% filter(date == date_2nd_max) %>% unnest() -> a
      b %>% filter(date == date_2nd_max) %>% unnest() -> b
      # b %>% filter(date == date_2nd_max) -> b
      
      #' filter to active namespaces
      #a <- a[a$table_id %in% b$namespace,] #' not using because sometimes b (active_name) excludes customers
      return(a)
    }
    
    current_namespace_data <- return_namespace_table(namespace.df, active_namespaces)
    date_2nd_max <- namespace.df$date[1]
    current_namespace_data <- current_namespace_data[,c(2:ncol(current_namespace_data))]
    current_namespace_data$health_index <- round(100 * ((current_namespace_data$automatic * .333) + (current_namespace_data$optimized * .333) + (current_namespace_data$personalized * .333)))
    current_namespace_data$lag_30 <- strsplit(current_namespace_data$lagged, ' ') %>% 
                                      map(., as.integer) %>% 
                                      lapply(., FUN = function(x){
                                        x <- tail(x, 30) 
                                        x <- median(x, na.rm = TRUE)
                                        return(round(x))
                                      }
                                      )
    current_namespace_data$lagged <- NULL
    current_namespace_data <- current_namespace_data[,c(1, 11, 2, 12, 3:5, 10, 9, 8, 6:7)]
    names(current_namespace_data) <- c('Namespace', 'Adoption', 'Message Vol.', 'Avg. Vol.' , 'Auto', 'Optimized', 'Personalized', 
                                       'Users Messaged', 'Reach: Total', 'Reach: Push', 'Reach: Email', 'Reach: In-App')
    
    datatable(current_namespace_data,
              extensions = 'Buttons',
              options = list(dom = 'Bfrtip', 
                             buttons = list(list(extend = 'csv',
                                                 filename = paste('namespace_summary_', date_2nd_max)), 
                                            list(extend = 'excel',
                                                 filename = paste('namespace_summary_', date_2nd_max))),
                             order = list(2, 'desc'),
                             columnDefs=list(list(targets=1:3, class="dt-right")),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}"),
              searchHighlight = TRUE,
              lengthMenu = c(25, 50, 100)),
              rownames = FALSE) %>%
      formatPercentage(c('Auto', 'Optimized', 'Personalized', 'Users Messaged'), 1) %>%
      formatCurrency(c('Message Vol.', 'Adoption', 'Avg. Vol.', 'Reach: Total', 'Reach: Push', 'Reach: Email', 'Reach: In-App'), '', digits = 0) %>%
      formatStyle(
        c('Auto', 'Optimized', 'Personalized', 'Users Messaged'),
        color = styleInterval(c(.33, .66), c('red', 'orange', 'green'))
      ) %>%
      formatStyle(
        'Adoption',
        color = styleInterval(c(40, 70), c('red', 'orange', 'green')),
        fontWeight = 'bold'
      )
    })
  
  output$date_last <- renderText({
    namespace_table.df <- load('/home/rstudio/scripts/dashboard/data/namespace_table_data.RData')
    date <- namespace.df$date[nrow(namespace.df)]
    date <- format(date, '%B %d, %Y')
  })
  
  output$Semi_collapsible_sidebar<-renderMenu({
    if (vals$collapsed)
      sidebarMenu(
        menuItem(NULL, tabName = "vitals", icon = icon("dashboard"))
        # menuItem(NULL, icon = icon("th"), tabName = "widgets", badgeColor = "green"),
        # menuItem(NULL, icon = icon("bar-chart-o"),
        #          menuSubItem(span(class="collapsed_text","Namespace"), tabName = "subitem1"),
        #          menuSubItem(span(class="collapsed_text","Optimizations"), tabName = "subitem2")
      )
    else
      sidebarMenu(
        menuItem("Health", tabName = "vitals", icon = icon("dashboard"))
        # menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new", badgeColor = "green"),
        # menuItem("Charts", icon = icon("bar-chart-o"),
        #          menuSubItem("Namespace", tabName = "subitem1"),
        #          menuSubItem("Optimizations", tabName = "subitem2")
      )
  })
})


  # output$tabselected <- renderText({input$messages_delivered})
  # #filter_date <- ifelse(output$tabselected == '1 mn', 31, 365)
  # 
  # output$messages_1yr <- renderPlot(height = 200, {
  #   message_1yr <- message_volume %>%
  #     filter(date >= Sys.Date() - 365) %>%
  #     ggplot(., aes(x = date, y = sum)) +
  #     geom_line() +
  #     geom_line(aes(x = date, y = ma_30, color = 'blue')) +
  #     geom_line(aes(x = date, y = ma_7, color = 'green')) +
  #     geom_point(aes(x = last_x, y = last_y), color = 'red') +
  #     geom_hline(yintercept = last_y, linetype = 'dashed', color = 'red', alpha = .5) +
  #     scale_y_continuous(labels = scales::comma) +
  #     theme_bw() +
  #     theme(
  #       axis.text.y = element_text(size = 10),
  #       axis.text.x = element_text(size = 10),
  #       rect = element_blank(),
  #       line = element_blank(),
  #       legend.position = 'none'
  #     ) +
  #     guides(size = FALSE) +
  #     labs(
  #       x = '',
  #       y = ''
  #     )
  # 
  #   print(message_1yr)
  # })
  # output$messages_6mn <- renderPlot(height = 200, {
  #   message_1yr <- message_volume %>%
  #     filter(date >= Sys.Date() - 180) %>%
  #     ggplot(., aes(x = date, y = sum)) +
  #     geom_line() +
  #     geom_line(aes(x = date, y = ma_30, color = 'blue')) +
  #     geom_line(aes(x = date, y = ma_7, color = 'green')) +
  #     geom_point(aes(x = last_x, y = last_y), color = 'red') +
  #     geom_hline(yintercept = last_y, linetype = 'dashed', color = 'red', alpha = .5) +
  #     scale_y_continuous(labels = scales::comma) +
  #     theme_bw() +
  #     theme(
  #       axis.text.y = element_text(size = 10),
  #       axis.text.x = element_text(size = 10),
  #       rect = element_blank(),
  #       line = element_blank(),
  #       legend.position = 'none'
  #     ) +
  #     guides(size = FALSE) +
  #     labs(
  #       x = '',
  #       y = ''
  #     )
  # 
  #   print(message_1yr)
  # })
  # output$messages_1mn <- renderPlot(height = 200, {
  #   message_1yr <- message_volume %>%
  #     filter(date >= Sys.Date() - 30) %>%
  #     ggplot(., aes(x = date, y = sum)) +
  #     geom_line() +
  #     geom_line(aes(x = date, y = ma_30, color = 'blue')) +
  #     geom_line(aes(x = date, y = ma_7, color = 'green')) +
  #     geom_point(aes(x = last_x, y = last_y), color = 'red') +
  #     geom_hline(yintercept = last_y, linetype = 'dashed', color = 'red', alpha = .5) +
  #     scale_y_continuous(labels = scales::comma) +
  #     theme_bw() +
  #     theme(
  #       axis.text.y = element_text(size = 10),
  #       axis.text.x = element_text(size = 10),
  #       rect = element_blank(),
  #       line = element_blank(),
  #       legend.position = 'none'
  #     ) +
  #     guides(size = FALSE) +
  #     labs(
  #       x = '',
  #       y = ''
  #     )
  # 
  #   print(message_1yr)
  # })
  
  #' generate data for message volume
  # messages <- read.csv(file = '/home/rstudio/scripts/dashboard/data/messages.csv', stringsAsFactors = FALSE)
  # message_volume <- messages %>%
  #   group_by(date) %>%
  #   summarize(sum = sum(sum)) %>%
  #   mutate(date = as.Date(date),
  #          ma_30 = zoo::rollmean(sum, 30, na.pad = TRUE),
  #          ma_7 = zoo::rollmean(sum, 7, na.pad = TRUE)) %>%
  #   arrange(date)
  # last_x <- message_volume$date[nrow(message_volume)]
  # last_y <- message_volume$sum[nrow(message_volume)]