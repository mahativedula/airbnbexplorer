library(bslib)

ui <- navbarPage(
  title = "Airbnb Explorer: NYC vs LA",

  tabPanel(
    "Market Overview",
    fluidRow(
      column(
        10, offset = 1,
        br(),
        div(
          style = "font-size: 15px; line-height: 1.7; color: #333;",
          p('Airbnb may look like one standardized platform, but the market it creates can vary
            sharply from one city to another. Local geography, tourism demand, housing stock,
            and business activity all shape how listings are priced, managed, and made available
            throughout the year.'),
          p("This dashboard compares those patterns across two major U.S. cities:"),
          tags$ul(
            tags$li(HTML("<b>New York City</b> - a fast-paced East Coast hub for finance
              and business, known for its dense skyline of high-rise skyscrapers, strong
              public transport system, limited space, and large tourism scene.")),
            tags$li(HTML("<b>Los Angeles</b> - a sprawling West Coast metropolis at the
              heart of the entertainment industry, defined by its connection to Hollywood,
              concentration of celebrities, and expansive car-dependent urban layout."))
          ),
          p("By putting the two side by side, we can see how local context changes the structure
            of the Airbnb market for both hosts and travelers.")
        ),
        hr()
      )
    ),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("overview_month", "Snapshot Month", choices = NULL),
        selectInput(
          "overview_metric", "Map Metric", choices = c(
            "Listing count" = "listing_count",
            "Multi-listing host share" = "multi_share",
            "Median days available / year" = "median_availability"
          )
        )
      ),
      mainPanel(
        width = 9,
        uiOutput("selected_neighbourhood_ui"),
        fluidRow(
          column(6, plotOutput("market_map_nyc", height = "420px", click = "nyc_map_click")),
          column(6, plotOutput("market_map_la", height = "420px", click = "la_map_click"))
        ),
        br(),
        fluidRow(
          column(6, plotOutput("overview_plot_nyc", height = "380px", click = "nyc_bar_click")),
          column(6, plotOutput("overview_plot_la", height = "380px", click = "la_bar_click"))
        ),
        br(),
        p(
          style = "font-size: 13px; color: #666;",
          "The room type composition chart uses the selected snapshot month. The availability chart below compares all listing snapshots for the selected city view."
        ),
        selectInput(
          "overview_city_detail", "City (composition and availability charts)",
          choices = c("Both cities" = "all", "NYC", "LA"),
          selected = "all"
        ),
        fluidRow(
          column(6, plotOutput("room_type_plot", height = "340px")),
          column(6, plotOutput("availability_trend_plot", height = "340px"))
        ),
        br(),
        uiOutput("overview_note")
      )
    )
  ),

  tabPanel(
    "Pricing",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("price_month", "Snapshot Month", choices = NULL),
        hr(),
        p(
          style = "font-size: 12px; color: #666;",
          "All pricing charts respond to the city filter below. The neighborhood ranking shows both cities when Both cities is selected and only one city when NYC or LA is selected."
        ),
        selectInput(
          "price_city", "City filter",
          choices = c("Both cities" = "all", "NYC", "LA")
        )
      ),
      mainPanel(
        width = 9,
        h4("City-Level Pricing"),
        fluidRow(
          column(6, plotOutput("price_city_plot", height = "320px")),
          column(6, plotOutput("price_room_plot", height = "320px"))
        ),
        br(),
        h4("Pricing by Neighborhood"),
        uiOutput("price_neighbourhood_ui"),
        br(),
        h4("What Drives Price?"),
        fluidRow(
          column(6, plotOutput("price_accommodates_plot", height = "320px")),
          column(6, plotOutput("price_host_type_plot", height = "320px"))
        ),
        br(),
        fluidRow(
          column(12, plotOutput("price_review_plot", height = "360px"))
        ),
        br(),
        uiOutput("price_note")
      )
    )
  ),

  tabPanel(
    "Host Activity",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("host_city", "City", choices = c("Both cities" = "all", "NYC", "LA")),
        selectInput("host_month", "Snapshot Month", choices = NULL)
      ),
      mainPanel(
        width = 9,
        h4("Who Are the Hosts?"),
        fluidRow(
          column(6, plotOutput("host_share_plot", height = "320px")),
          column(6, plotOutput("host_concentration_plot", height = "320px"))
        ),
        br(),
        h4("How Do Host Types Behave?"),
        fluidRow(
          column(6, plotOutput("host_availability_plot", height = "320px")),
          column(6, plotOutput("host_room_type_plot", height = "320px"))
        ),
        br(),
        h4("Does Host Quality Command a Premium?"),
        fluidRow(
          column(12, plotOutput("host_superhost_plot", height = "360px"))
        ),
        br(),
        uiOutput("host_note")
      )
    )
  )
)

