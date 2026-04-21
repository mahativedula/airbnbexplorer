library(bslib)

ui <- navbarPage(
  title = "Airbnb Explorer: NYC vs LA",
  

  # ── Tab 1: Market Overview ──────────────────────────────────────────────
  tabPanel("Market Overview",
           fluidRow(
             column(10, offset = 1,
                    br(),
                    div(style = "font-size: 15px; line-height: 1.7; color: #333;",
                        p('When people think "Airbnb," they think of one core business model: hosts
            listing their homes, and travelers booking short-term stays. However, beneath
            that simplicity lies a more nuanced reality. Airbnb culture varies vastly
            across countries, cities, and sometimes even neighborhoods, influenced by
            geography, architecture, travel demand, the concentration of wealth, local
            business, culture, and hundreds of other factors. This culture, in turn,
            shapes listing availability, hosts\' rules, price, and booking rates.'),
                        p("Our dashboard explores these dynamics by comparing Airbnb across two major U.S. cities:"),
                        tags$ul(
                          tags$li(HTML("<b>New York City</b> — a fast-paced East Coast hub for finance
              and business, known for its dense skyline of high-rise skyscrapers, strong
              public transport system, limited space, and large tourism scene.")),
                          tags$li(HTML("<b>Los Angeles</b> — a sprawling West Coast metropolis at the
              heart of the entertainment industry, defined by its connection to Hollywood,
              concentration of celebrities, and expansive car-dependent urban layout."))
                        ),
                        p("By contrasting these two cities, we uncover how local context shapes the way
            Airbnb operates — and what that means for hosts and travelers alike.")
                    ),
                    hr()
             )
           ),
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("overview_month", "Snapshot Month", choices = NULL),
                          selectInput("overview_metric", "Map Metric", choices = c(
                            "Listing count"                = "listing_count",
                            "Multi-listing host share"     = "multi_share",
                            "Median days available / year" = "median_availability"
                          ))
             ),
             mainPanel(width = 9,
                       fluidRow(
                         column(6, plotOutput("market_map_nyc",   height = "420px")),
                         column(6, plotOutput("market_map_la",    height = "420px"))
                       ),
                       br(),
                       fluidRow(
                         column(6, plotOutput("overview_plot_nyc", height = "380px")),
                         column(6, plotOutput("overview_plot_la",  height = "380px"))
                       ),
                       br(),
                       fluidRow(
                         column(6, plotOutput("room_type_plot",       height = "340px")),
                         column(6, plotOutput("room_type_trend_plot", height = "340px"))
                       ),
                       br(),
                       uiOutput("overview_note")
             )
           )
  ),
  
  # ── Tab 2: Pricing ──────────────────────────────────────────────────────
  tabPanel("Pricing",
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("price_month", "Snapshot Month", choices = NULL),
                          hr(),
                          p(style = "font-size: 12px; color: #666;",
                            "City-level and room type charts show both cities. 
         Neighborhood and listing-level charts filter by the city selected below."),
                          selectInput("price_city", "City (neighborhood & listing charts)",
                                      choices = c("Both cities" = "all", "NYC", "LA"))
             ),
             mainPanel(width = 9,
                       
                       # Row 1: City-level overview
                       h4("City-Level Pricing"),
                       fluidRow(
                         column(6, plotOutput("price_city_plot",     height = "320px")),
                         column(6, plotOutput("price_room_plot",     height = "320px"))
                       ),
                       br(),
                       
                       # Row 2: Neighborhood breakdown
                       h4("Pricing by Neighborhood"),
                       fluidRow(
                         column(6, plotOutput("price_neighbourhood_nyc", height = "360px")),
                         column(6, plotOutput("price_neighbourhood_la",  height = "360px"))
                       ),
                       br(),
                       
                       # Row 3: Price drivers
                       h4("What Drives Price?"),
                       fluidRow(
                         column(6, plotOutput("price_accommodates_plot", height = "320px")),
                         column(6, plotOutput("price_host_type_plot",    height = "320px"))
                       ),
                       br(),
                       fluidRow(
                         column(12, plotOutput("price_review_plot",      height = "360px"))
                       ),
                       br(),
                       uiOutput("price_note")
             )
           )
  ),
  
  tabPanel("Host Activity",
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("host_city", "City",
                                      choices = c("Both cities" = "all", "NYC", "LA")),
                          selectInput("host_month", "Snapshot Month", choices = NULL)
             ),
             mainPanel(width = 9,
                       
                       h4("Who Are the Hosts?"),
                       fluidRow(
                         column(6, plotOutput("host_share_plot",        height = "320px")),
                         column(6, plotOutput("host_concentration_plot", height = "320px"))
                       ),
                       br(),
                       
                       h4("How Do Host Types Behave?"),
                       fluidRow(
                         column(6, plotOutput("host_availability_plot", height = "320px")),
                         column(6, plotOutput("host_room_type_plot",    height = "320px"))
                       ),
                       br(),
                       
                       h4("Does Host Quality Command a Premium?"),
                       fluidRow(
                         column(12, plotOutput("host_superhost_plot",   height = "360px"))
                       ),
                       br(),
                       uiOutput("host_note")
             )
           )
  )
)