library(bslib)

ui <- navbarPage(
  title = "Airbnb Explorer: NYC vs LA",

  tabPanel(
    "Data Story",
    fluidRow(
      column(
        10, offset = 1,
        br(),
        div(
          style = "font-size: 15px; line-height: 1.7; color: #333;",
          h3("Two cities, two Airbnb market models"),
          p("New York City and Los Angeles sit on the same platform, serve many of the same travelers, and rank among Airbnb's largest U.S. markets. They ought to look similar. Instead, side by side, they begin to look like different businesses."),
          p("The question behind this story is simple: are these cities just larger and smaller versions of the same Airbnb market, or does the platform take on a different shape in each place? The clearest answer in the data is that Los Angeles looks more like a whole-home, commercially supplied market, while New York remains tighter, more mixed, and more dependent on private-room listings."),
          p(
            style = "font-size: 13px; color: #666;",
            "Room mix, pricing, and host structure are shown for September 2025, the latest shared priced snapshot. Availability compares all three listing snapshots: June, September, and December 2025."
          )
        ),
        br(),
        fluidRow(
          column(7, plotOutput("story_room_type_plot", height = "360px")),
          column(
            5,
            h4("The split begins with what is being listed"),
            p("The first clue is the room mix. Los Angeles is dominated by entire-home listings, while New York keeps a far larger private-room segment. That is more than a catalog difference. It suggests that the two cities are not just pricing differently; they are drawing on different kinds of hosts and serving different kinds of stays.")
          )
        ),
        br(),
        fluidRow(
          column(
            5,
            h4("Availability makes that difference harder to ignore"),
            p("Availability sharpens the divide. Across all three listing snapshots, Los Angeles listings remain available for more days of the year. That is the pattern of a market that behaves more like standing inventory. New York looks more constrained: denser, less open, and more shaped by part-time or room-based hosting.")
          ),
          column(7, plotOutput("story_availability_plot", height = "360px"))
        ),
        br(),
        fluidRow(
          column(7, plotOutput("story_price_plot", height = "360px")),
          column(
            5,
            h4("Price tells a story inside each city, not just between them"),
            p("The pricing story is not just New York versus Los Angeles. It is also neighborhood versus neighborhood, and listing type versus listing type. Entire homes command the clearest premium, but location does almost as much work. In both cities, a relatively small slice of the market pulls far above the median.")
          )
        ),
        br(),
        fluidRow(
          column(
            5,
            h4("Who controls the listings matters too"),
            p("Then there is the question of control. In both cities, a relatively small group of multi-listing hosts controls a large share of supply. But combined with the room mix and availability patterns, that concentration looks especially consequential in Los Angeles, where the market already leans toward whole-home, high-availability listings."),
            p(HTML("<b>What the reader should walk away knowing:</b> these are not just two Airbnb markets with different prices. They reflect different market models. Los Angeles looks more commercialized and whole-home oriented. New York remains denser, more mixed, and more private-room heavy."))
          ),
          column(7, plotOutput("story_host_plot", height = "360px"))
        ),
        br()
      )
    )
  ),

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

