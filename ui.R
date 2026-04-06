library(shiny)

fluidPage(
  titlePanel("Airbnb Explorer: NYC vs LA"),

  wellPanel(
    h4("What this version shows"),
    p(
      "This interim version focuses on market density, room types, availability, and host activity.",
      "Pricing is still missing from the current Inside Airbnb download, so price comparisons are not included yet."
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
      "Availability Trends",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "availability_metric",
            "Metric",
            choices = c(
              "Mean availability rate" = "mean_availability_rate",
              "Active listing count" = "active_listing_count"
            ),
            selected = "mean_availability_rate"
          )
        ),
        mainPanel(
          plotOutput("availability_plot", height = "450px"),
          br(),
          htmlOutput("availability_note")
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
