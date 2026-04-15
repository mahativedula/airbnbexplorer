library(shiny)

fluidPage(
  titlePanel("Airbnb Explorer: NYC vs LA"),

  wellPanel(
    h4("What this version shows"),
    fluidRow(
      column(
        8,
        p(
          "This version focuses on market density, room types, host activity, and shared-month pricing.",
          "Pricing is only shown for the common priced snapshots we have for both cities."
        )
      ),
      column(
        4,
        selectInput(
          "snapshot_month",
          "Shared priced snapshot",
          choices = NULL
        )
      )
    )
  ),

  tabsetPanel(
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
          radioButtons(
            "overview_metric",
            "Neighborhood metric",
            choices = c(
              "Listing count" = "listing_count",
              "Multi-listing host share" = "multi_share",
              "Median availability" = "median_availability"
            ),
            selected = "listing_count"
          ),
          sliderInput(
            "top_n",
            "Neighborhoods shown in ranking",
            min = 5,
            max = 15,
            value = 10
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
      "Room Types",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "room_city",
            "Cities",
            choices = c("NYC", "LA"),
            selected = c("NYC", "LA")
          )
        ),
        mainPanel(
          plotOutput("room_type_plot", height = "450px"),
          br(),
          htmlOutput("room_type_note")
        )
      )
    ),

    tabPanel(
      "Pricing Snapshots",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "price_city",
            "Neighborhood price view",
            choices = c("NYC", "LA"),
            selected = "NYC"
          ),
          sliderInput(
            "price_top_n",
            "Neighborhoods shown",
            min = 5,
            max = 15,
            value = 10
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
      "Availability Trends",
      fluidRow(
        column(12, plotOutput("availability_plot", height = "450px"))
      ),
      br(),
      htmlOutput("availability_note")
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
