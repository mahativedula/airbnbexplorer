library(shiny)

fluidPage(
  titlePanel("Airbnb Explorer: NYC vs LA"),

  wellPanel(
    h4("What this version shows"),
    p(
      "This version combines a guided data story with focused interactive views for neighborhood patterns, pricing, and host activity.",
      "Pricing is only shown for the shared priced snapshots available for both cities."
    )
  ),

  tabsetPanel(
    tabPanel(
      "Data Story",
      br(),
      h3("Two cities, two Airbnb market models"),
      p(
        "At first glance, New York City and Los Angeles both look like the same kind of Airbnb market: large, tourist-heavy, and full of listings.",
        "But once we compare what kinds of listings dominate, how available they are, and who manages them, the two cities begin to look very different."
      ),
      p(
        "Our main question is not just which city has more listings or higher prices. It is whether the two markets are structured in the same way.",
        "So far, the answer looks like no."
      ),
      hr(),
      h4("1. The listing mix is different from the start"),
      p(
        "Los Angeles is much more dominated by entire-home listings, while New York has a much larger private-room market.",
        "That suggests the two cities are serving somewhat different kinds of hosts and guests."
      ),
      plotOutput("story_room_type_plot", height = "420px"),
      br(),
      h4("2. Listing availability points the same way"),
      p(
        "The gap does not stop at room type. Across the June, September, and December listing snapshots, Los Angeles listings tend to be available for more days of the year than New York listings.",
        "That pattern fits a market that looks more year-round and commercial in LA, and more mixed in NYC."
      ),
      plotOutput("story_availability_plot", height = "420px"),
      br(),
      h4("3. Host structure reinforces that difference"),
      p(
        "The host breakdown points in the same direction. In both cities, multi-listing hosts are important, but Los Angeles is more dominated by that kind of inventory.",
        "New York still has a large multi-listing presence, but the market looks less fully shaped by it."
      ),
      plotOutput("story_host_plot", height = "420px"),
      br(),
      h4("What we want readers to take away"),
      p(
        "The main takeaway is that NYC and LA are not just different in size or price. They seem to represent two different Airbnb market models.",
        "Los Angeles looks more commercialized and more centered on whole-home, year-round supply. New York looks more mixed, with a stronger private-room presence and more signs of listings that are not available year-round."
      )
    ),

    tabPanel(
      "Market Overview",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "overview_city",
            "City",
            choices = c("NYC", "LA"),
            selected = "NYC"
          ),
          selectInput(
            "overview_month",
            "Snapshot month",
            choices = NULL
          ),
          radioButtons(
            "overview_metric",
            "Neighborhood metric",
            choices = c(
              "Listing count" = "listing_count",
              "Multi-listing host share" = "multi_share",
              "Median days available per year" = "median_availability"
            ),
            selected = "listing_count"
          )
        ),
        mainPanel(
          fluidRow(
            column(7, plotOutput("market_map_plot", height = "500px")),
            column(5, plotOutput("overview_plot", height = "500px"))
          ),
          br(),
          htmlOutput("overview_note")
        )
      )
    ),

    tabPanel(
      "Pricing Snapshots",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "price_month",
            "Snapshot month",
            choices = NULL
          ),
          selectInput(
            "price_city",
            "Neighborhood price view",
            choices = c("NYC", "LA"),
            selected = "NYC"
          )
        ),
        mainPanel(
          fluidRow(
            column(6, plotOutput("price_city_plot", height = "360px")),
            column(6, plotOutput("price_room_plot", height = "360px"))
          ),
          fluidRow(
            column(12, plotOutput("price_neighbourhood_plot", height = "430px"))
          ),
          br(),
          htmlOutput("price_note")
        )
      )
    ),

    tabPanel(
      "Host Activity",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "host_city",
            "City",
            choices = c("NYC", "LA"),
            selected = "NYC"
          ),
          selectInput(
            "host_month",
            "Snapshot month",
            choices = NULL
          )
        ),
        mainPanel(
          fluidRow(
            column(6, plotOutput("host_plot", height = "430px")),
            column(6, plotOutput("host_availability_plot", height = "430px"))
          ),
          br(),
          htmlOutput("host_note")
        )
      )
    )
  )
)
