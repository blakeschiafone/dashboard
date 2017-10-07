library(shiny)
library(shinydashboard)
library(DT)


#' main body for product adoption
body_adoption <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  #' graph for differentiator values
  #' graphing: optimized, personalized, automatic message volume as a ratio
  #' the lines graphs are a 7-day moving average
  #' the geom_point is yesterday's current value
  h2('Proportion Messages Sent with Kahuna Differentiator'),
  plotOutput("graph_differentiator", height = "auto", width = "auto"),
  
  br(),
  br(),
  br(),
  br(),
  
  h2('Campaigns Running Yesterday (vs. 7-day average)'),
  fluidRow(
    valueBoxOutput("valuebox_push_count"),
    valueBoxOutput("valuebox_email_count"),
    valueBoxOutput("valuebox_inapp_count")
  ),
  
  br(),
  
  fluidRow(
    infoBoxOutput("infobox_experience_count"),
    infoBoxOutput("infobox_lifecycle_count"),
    infoBoxOutput("infobox_onetime_count"),
    infoBoxOutput("infobox_adaptive_count"),
    infoBoxOutput("infobox_conversion_count"),
    infoBoxOutput("infobox_flex_count")
  ),
  
  br(),
  br(),
  br(),
  br(),
  
  h2(textOutput('date_last')),
  fluidRow(
    dataTableOutput("namespace_table",
                    width = '98%'), align = 'center'
  )
)



dashboardPage(
  dashboardHeader(title="Kahuna",
                  titleWidth = 110),
  dashboardSidebar(width = 110,
                   sidebarMenuOutput("Semi_collapsible_sidebar"),              
                   tags$script("$(document).on('click', '.sidebar-toggle', function () {Shiny.onInputChange('SideBar_col_react', Math.random())});"),
                   tags$script("$(document).on('click', '.treeview.active', function () {
                                $(this).removeClass('active');
                                $(this).find( 'ul' ).removeClass('menu-open'); 
                                $(this).find( 'ul' ).css('display', 'none'); 
                                            });")),
  body_adoption
  # dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")))
)



#' main body for dashboard
# body_dashboard <- dashboardBody(
#   tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
#   fluidRow(
#     tabBox(
#       title = h5("Messages Delivered"),
#       # The id lets us use input$tabset1 on the server to find the current tab
#       id = "messages_delivered", height = "275px", width = 4,
#       side = "right",
#       selected = "1 yr",
#       tabPanel("1 mn", id = "messages_delivered_1mn",  plotOutput("messages_1mn", height = "auto"), verbatimTextOutput('tabselected')),
#       tabPanel("6 mn", id = "messages_delivered_6mn",  plotOutput("messages_6mn", height = "auto")),
#       tabPanel("1 yr", id = "messages_delivered_1yr", plotOutput("messages_1yr", height = "auto"))
#     ),
#     tabBox(
#       title = h5("Channel Volume"),
#       id = "channel_volume", height = "275px", width = 4,
#       side = "right",
#       selected = "1 yr",
#       tabPanel("1 mn", " "),
#       tabPanel("6 mn", " "),
#       tabPanel("1 yr", " ")
#     ),
#     tabBox(
#       title = h5("Campaigns Created"),
#       id = "campaigns_created", height = "275px", width = 4,
#       side = "right",
#       selected = "1 yr",
#       tabPanel("1 mn", " "),
#       tabPanel("6 mn", " "),
#       tabPanel("1 yr", " ")
#     )
#   )
# )
