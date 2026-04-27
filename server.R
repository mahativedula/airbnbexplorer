library(shiny)
library(tidyverse)
library(scales)
library(sf)
sf::sf_use_s2(FALSE)

theme_set(theme_classic(base_size = 12))

shared_months                 <- readRDS(file.path("data", "pricing_shared_months.rds"))
snapshot_city_summary         <- readRDS(file.path("data", "snapshot_city_summary.rds"))
neighbourhood_shapes          <- readRDS(file.path("data", "neighbourhoods_clean.rds")) %>% sf::st_make_valid()
snapshot_overview_summary     <- readRDS(file.path("data", "snapshot_overview_summary.rds"))
snapshot_room_type_summary    <- readRDS(file.path("data", "snapshot_room_type_summary.rds"))
snapshot_host_summary         <- readRDS(file.path("data", "snapshot_host_summary.rds"))
pricing_city_summary          <- readRDS(file.path("data", "pricing_city_summary.rds"))
pricing_room_type_summary     <- readRDS(file.path("data", "pricing_room_type_summary.rds"))
pricing_neighbourhood_summary <- readRDS(file.path("data", "pricing_neighbourhood_summary.rds"))
pricing_listings_clean        <- readRDS(file.path("data", "pricing_listings_clean.rds"))
pricing_host_summary          <- readRDS(file.path("data", "pricing_host_summary.rds"))
host_summary                  <- readRDS(file.path("data", "host_summary.rds"))

listing_months <- snapshot_overview_summary %>%
  distinct(snapshot_month) %>%
  pull(snapshot_month) %>%
  sort()

pricing_months <- sort(shared_months)

listing_month_choices <- setNames(as.character(listing_months), format(listing_months, "%b %Y"))
pricing_month_choices <- setNames(as.character(pricing_months), format(pricing_months, "%b %Y"))
story_priced_month <- max(pricing_months)

map_summary <- neighbourhood_shapes %>%
  left_join(
    snapshot_overview_summary,
    by = c("city", "neighbourhood_group", "neighbourhood"),
    relationship = "many-to-many"
  )

snapshot_label <- function(x) {
  format(as.Date(x), "%b %Y")
}

story_priced_label <- snapshot_label(story_priced_month)

main_room_types <- c("Entire home/apt", "Private room")
map_palette <- c("#d6e6f5", "#a8cce6", "#78b1d6", "#4d8fc0", "#2867a8", "#0b3d78")

build_map_scale <- function(values, labels, transform = NULL) {
  values <- values[is.finite(values)]
  breaks <- unique(as.numeric(quantile(
    values,
    probs = seq(0, 1, length.out = 7),
    na.rm = TRUE,
    names = FALSE
  )))

  if (length(breaks) < 3) {
    breaks <- pretty(values, n = 6)
  }

  scale_args <- list(
    colours = map_palette,
    breaks = breaks,
    labels = labels,
    na.value = "grey95",
    show.limits = TRUE
  )

  if (!is.null(transform)) {
    scale_args$trans <- transform
  }

  do.call(scale_fill_stepsn, scale_args)
}

server <- function(input, output, session) {
  updateSelectInput(
    session, "overview_month",
    choices = listing_month_choices,
    selected = tail(listing_month_choices, 1)
  )
  updateSelectInput(
    session, "price_month",
    choices = pricing_month_choices,
    selected = tail(pricing_month_choices, 1)
  )
  updateSelectInput(
    session, "host_month",
    choices = pricing_month_choices,
    selected = tail(pricing_month_choices, 1)
  )

  selected_overview_month <- reactive({
    req(input$overview_month)
    as.Date(input$overview_month)
  })

  overview_city_filter <- reactive({
    if (is.null(input$overview_city_detail) || input$overview_city_detail == "all") {
      c("NYC", "LA")
    } else {
      input$overview_city_detail
    }
  })

  selected_price_month <- reactive({
    req(input$price_month)
    as.Date(input$price_month)
  })

  selected_host_month <- reactive({
    req(input$host_month)
    as.Date(input$host_month)
  })

  selected_neighbourhood <- reactiveVal(NULL)

  observeEvent(input$nyc_map_click, {
    click <- input$nyc_map_click
    if (is.null(click)) {
      return()
    }

    pt <- sf::st_sfc(sf::st_point(c(click$x, click$y)), crs = 4326)
    nyc_shapes <- map_summary %>%
      filter(city == "NYC", snapshot_month == selected_overview_month())
    idx <- which(sf::st_within(pt, nyc_shapes, sparse = FALSE))

    if (length(idx) > 0) {
      selected_neighbourhood(nyc_shapes$neighbourhood[idx[1]])
    } else {
      selected_neighbourhood(NULL)
    }
  })

  observeEvent(input$la_map_click, {
    click <- input$la_map_click
    if (is.null(click)) {
      return()
    }

    pt <- sf::st_sfc(sf::st_point(c(click$x, click$y)), crs = 4326)
    la_shapes <- map_summary %>%
      filter(city == "LA", snapshot_month == selected_overview_month())
    idx <- which(sf::st_within(pt, la_shapes, sparse = FALSE))

    if (length(idx) > 0) {
      selected_neighbourhood(la_shapes$neighbourhood[idx[1]])
    } else {
      selected_neighbourhood(NULL)
    }
  })

  observeEvent(input$nyc_bar_click, {
    plot_data <- snapshot_overview_summary %>%
      filter(city == "NYC", snapshot_month == selected_overview_month()) %>%
      arrange(desc(.data[[input$overview_metric]])) %>%
      slice_head(n = 10)

    clicked <- nearPoints(
      plot_data,
      input$nyc_bar_click,
      xvar = "neighbourhood",
      yvar = input$overview_metric,
      threshold = 20,
      maxpoints = 1
    )

    if (nrow(clicked) > 0) {
      selected_neighbourhood(clicked$neighbourhood[1])
    }
  })

  observeEvent(input$la_bar_click, {
    plot_data <- snapshot_overview_summary %>%
      filter(city == "LA", snapshot_month == selected_overview_month()) %>%
      arrange(desc(.data[[input$overview_metric]])) %>%
      slice_head(n = 10)

    clicked <- nearPoints(
      plot_data,
      input$la_bar_click,
      xvar = "neighbourhood",
      yvar = input$overview_metric,
      threshold = 20,
      maxpoints = 1
    )

    if (nrow(clicked) > 0) {
      selected_neighbourhood(clicked$neighbourhood[1])
    }
  })

  observeEvent(input$clear_neighbourhood, {
    selected_neighbourhood(NULL)
  })

  output$selected_neighbourhood_ui <- renderUI({
    hood <- selected_neighbourhood()

    if (is.null(hood)) {
      return(NULL)
    }

    div(
      style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
      tags$span(
        style = "font-weight: bold; font-size: 14px;",
        paste("Selected neighborhood:", hood)
      ),
      actionButton(
        "clear_neighbourhood",
        "Clear selection",
        style = "padding: 2px 10px; font-size: 12px;"
      )
    )
  })

  output$market_map_nyc <- renderPlot({
    metric_labels <- c(
      listing_count = "Listing count",
      multi_share = "Multi-listing host share",
      median_availability = "Median days available per year"
    )

    plot_data <- map_summary %>%
      filter(city == "NYC", snapshot_month == selected_overview_month())
    selected_shape <- if (is.null(selected_neighbourhood())) {
      plot_data[0, ]
    } else {
      plot_data %>% filter(neighbourhood == selected_neighbourhood())
    }

    fill_scale <- if (input$overview_metric == "multi_share") {
      build_map_scale(plot_data$multi_share, label_percent(accuracy = 1))
    } else if (input$overview_metric == "listing_count") {
      build_map_scale(plot_data$listing_count, label_comma(), transform = "log10")
    } else {
      build_map_scale(plot_data$median_availability, label_number())
    }

    ggplot(plot_data) +
      geom_sf(aes(fill = .data[[input$overview_metric]]), color = "white", linewidth = 0.15) +
      geom_sf(data = selected_shape, fill = NA, color = "#F28E2B", linewidth = 1.1) +
      fill_scale +
      labs(
        title = paste("NYC -", metric_labels[[input$overview_metric]]),
        subtitle = snapshot_label(selected_overview_month()),
        fill = NULL
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })

  output$market_map_la <- renderPlot({
    metric_labels <- c(
      listing_count = "Listing count",
      multi_share = "Multi-listing host share",
      median_availability = "Median days available per year"
    )

    plot_data <- map_summary %>%
      filter(city == "LA", snapshot_month == selected_overview_month())
    selected_shape <- if (is.null(selected_neighbourhood())) {
      plot_data[0, ]
    } else {
      plot_data %>% filter(neighbourhood == selected_neighbourhood())
    }

    fill_scale <- if (input$overview_metric == "multi_share") {
      build_map_scale(plot_data$multi_share, label_percent(accuracy = 1))
    } else if (input$overview_metric == "listing_count") {
      build_map_scale(plot_data$listing_count, label_comma(), transform = "log10")
    } else {
      build_map_scale(plot_data$median_availability, label_number())
    }

    ggplot(plot_data) +
      geom_sf(aes(fill = .data[[input$overview_metric]]), color = "white", linewidth = 0.15) +
      geom_sf(data = selected_shape, fill = NA, color = "#F28E2B", linewidth = 1.1) +
      fill_scale +
      labs(
        title = paste("LA -", metric_labels[[input$overview_metric]]),
        subtitle = snapshot_label(selected_overview_month()),
        fill = NULL
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })

  output$overview_plot_nyc <- renderPlot({
    metric_labels <- c(
      listing_count = "Listing count",
      multi_share = "Multi-listing host share",
      median_availability = "Median days available per year"
    )
    value_labels <- list(
      listing_count = label_comma(),
      multi_share = label_percent(accuracy = 1),
      median_availability = label_number()
    )

    plot_data <- snapshot_overview_summary %>%
      filter(city == "NYC", snapshot_month == selected_overview_month()) %>%
      arrange(desc(.data[[input$overview_metric]])) %>%
      slice_head(n = 10) %>%
      mutate(
        neighbourhood = forcats::fct_reorder(neighbourhood, .data[[input$overview_metric]]),
        is_selected = if (is.null(selected_neighbourhood())) FALSE else neighbourhood == selected_neighbourhood()
      )

    ggplot(plot_data, aes(x = neighbourhood, y = .data[[input$overview_metric]], fill = is_selected)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("FALSE" = "#4E79A7", "TRUE" = "#F28E2B"), guide = "none") +
      scale_y_continuous(labels = value_labels[[input$overview_metric]]) +
      labs(
        title = "Top 10 Neighborhoods - NYC",
        subtitle = paste(metric_labels[[input$overview_metric]], "-", snapshot_label(selected_overview_month())),
        x = NULL,
        y = NULL
      )
  })

  output$overview_plot_la <- renderPlot({
    metric_labels <- c(
      listing_count = "Listing count",
      multi_share = "Multi-listing host share",
      median_availability = "Median days available per year"
    )
    value_labels <- list(
      listing_count = label_comma(),
      multi_share = label_percent(accuracy = 1),
      median_availability = label_number()
    )

    plot_data <- snapshot_overview_summary %>%
      filter(city == "LA", snapshot_month == selected_overview_month()) %>%
      arrange(desc(.data[[input$overview_metric]])) %>%
      slice_head(n = 10) %>%
      mutate(
        neighbourhood = forcats::fct_reorder(neighbourhood, .data[[input$overview_metric]]),
        is_selected = if (is.null(selected_neighbourhood())) FALSE else neighbourhood == selected_neighbourhood()
      )

    ggplot(plot_data, aes(x = neighbourhood, y = .data[[input$overview_metric]], fill = is_selected)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("FALSE" = "#E15759", "TRUE" = "#F28E2B"), guide = "none") +
      scale_y_continuous(labels = value_labels[[input$overview_metric]]) +
      labs(
        title = "Top 10 Neighborhoods - LA",
        subtitle = paste(metric_labels[[input$overview_metric]], "-", snapshot_label(selected_overview_month())),
        x = NULL,
        y = NULL
      )
  })

  output$room_type_plot <- renderPlot({
    plot_data <- snapshot_room_type_summary %>%
      filter(
        snapshot_month == selected_overview_month(),
        city %in% overview_city_filter()
      ) %>%
      mutate(room_type_group = if_else(room_type %in% main_room_types, room_type, "Other")) %>%
      group_by(city, room_type_group) %>%
      summarise(share = sum(share), .groups = "drop") %>%
      mutate(room_type_group = forcats::fct_relevel(room_type_group, "Entire home/apt", "Private room", "Other"))

    ggplot(plot_data, aes(x = room_type_group, y = share, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = percent(share, accuracy = 0.1)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.8
      ) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 0.85)) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      labs(
        title = if (identical(overview_city_filter(), c("NYC", "LA"))) {
          "Room Type Composition"
        } else {
          paste("Room Type Composition -", overview_city_filter())
        },
        subtitle = snapshot_label(selected_overview_month()),
        x = NULL,
        y = "Share of listings",
        fill = "City"
      )
  })

  output$availability_trend_plot <- renderPlot({
    plot_data <- snapshot_city_summary %>%
      filter(city %in% overview_city_filter()) %>%
      arrange(snapshot_month)

    show_both_cities <- identical(sort(overview_city_filter()), c("LA", "NYC"))

    ggplot(plot_data, aes(x = snapshot_month, y = median_availability_365, color = city, group = city)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.6) +
      geom_text(
        aes(label = round(median_availability_365, 0)),
        vjust = -0.5,
        size = 3.6,
        show.legend = FALSE
      ) +
      scale_color_manual(
        values = c("NYC" = "#4E79A7", "LA" = "#E15759"),
        guide = if (show_both_cities) guide_legend(title = "City") else "none"
      ) +
      scale_y_continuous(labels = label_number()) +
      scale_x_date(date_labels = "%b\n%Y", breaks = sort(unique(plot_data$snapshot_month))) +
      labs(
        title = if (show_both_cities) {
          "Days Available per Year Across Snapshot Months"
        } else {
          paste("Days Available per Year Across Snapshot Months -", overview_city_filter())
        },
        subtitle = if (show_both_cities) {
          "Median listing availability across the June, September, and December snapshots"
        } else {
          paste(
            "Median listing availability in",
            overview_city_filter(),
            "across the June, September, and December snapshots"
          )
        },
        x = NULL,
        y = "Median days available per year",
        color = "City"
      )
  })

  output$overview_note <- renderUI({
    HTML("<b>Takeaway:</b> NYC listings concentrate more tightly in dense, transit-rich areas,
         while LA spreads across several destination neighborhoods. The lower charts reinforce
         that split: NYC keeps a larger private-room segment, while LA stays more whole-home oriented.")
  })

  price_city_filter <- reactive({
    if (input$price_city == "all") c("NYC", "LA") else input$price_city
  })

  output$story_room_type_plot <- renderPlot({
    plot_data <- snapshot_room_type_summary %>%
      filter(snapshot_month == story_priced_month) %>%
      mutate(room_type_group = if_else(room_type %in% main_room_types, room_type, "Other")) %>%
      group_by(city, room_type_group) %>%
      summarise(share = sum(share), .groups = "drop") %>%
      mutate(room_type_group = forcats::fct_relevel(room_type_group, "Entire home/apt", "Private room", "Other"))

    ggplot(plot_data, aes(x = room_type_group, y = share, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = percent(share, accuracy = 0.1)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.8
      ) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 0.85)) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      labs(
        title = "Room Type Composition",
        subtitle = story_priced_label,
        x = NULL,
        y = "Share of listings",
        fill = "City"
      )
  })

  output$story_availability_plot <- renderPlot({
    plot_data <- snapshot_city_summary %>%
      arrange(snapshot_month)

    ggplot(plot_data, aes(x = snapshot_month, y = median_availability_365, color = city, group = city)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.6) +
      geom_text(
        aes(label = round(median_availability_365, 0)),
        vjust = -0.5,
        size = 3.6,
        show.legend = FALSE
      ) +
      scale_color_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = label_number()) +
      scale_x_date(date_labels = "%b\n%Y", breaks = sort(unique(plot_data$snapshot_month))) +
      labs(
        title = "Days Available per Year Across Snapshot Months",
        subtitle = "Median listing availability across the June, September, and December snapshots",
        x = NULL,
        y = "Median days available per year",
        color = "City"
      )
  })

  output$story_price_plot <- renderPlot({
    plot_data <- pricing_room_type_summary %>%
      filter(snapshot_month == story_priced_month, room_type %in% main_room_types) %>%
      mutate(room_type = forcats::fct_relevel(room_type, "Entire home/apt", "Private room"))

    ggplot(plot_data, aes(x = room_type, y = median_price, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = dollar(median_price)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.6
      ) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Median Price by Room Type",
        subtitle = paste(story_priced_label, "- hotel and shared rooms excluded"),
        x = NULL,
        y = "Median nightly price",
        fill = "City"
      )
  })

  output$story_host_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(snapshot_month == story_priced_month)

    ggplot(plot_data, aes(x = host_type, y = share, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = percent(share, accuracy = 0.1)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.8
      ) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 0.85)) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      labs(
        title = "Share of Listings by Host Type",
        subtitle = story_priced_label,
        x = NULL,
        y = "Share of listings",
        fill = "City"
      )
  })

  output$price_city_plot <- renderPlot({
    plot_data <- pricing_city_summary %>%
      filter(snapshot_month == selected_price_month(),
             city %in% price_city_filter())

    show_both_cities <- identical(sort(price_city_filter()), c("LA", "NYC"))

    ggplot(plot_data, aes(x = city, y = median_price, fill = city)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = dollar(median_price)), vjust = -0.35, size = 4) +
      scale_fill_manual(
        values = c("NYC" = "#4E79A7", "LA" = "#E15759"),
        guide = if (show_both_cities) guide_legend(title = "City") else "none"
      ) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = if (show_both_cities) {
          paste("Median Price by City -", snapshot_label(selected_price_month()))
        } else {
          paste("Median Price -", price_city_filter(), "-", snapshot_label(selected_price_month()))
        },
        subtitle = "Based on listings with valid price values",
        x = NULL,
        y = NULL,
        fill = "City"
      )
  })

  output$price_room_plot <- renderPlot({
    plot_data <- pricing_room_type_summary %>%
      filter(snapshot_month == selected_price_month(),
             room_type %in% main_room_types,
             city %in% price_city_filter()) %>%
      mutate(room_type = forcats::fct_relevel(room_type, "Entire home/apt", "Private room"))

    show_both_cities <- identical(sort(price_city_filter()), c("LA", "NYC"))

    ggplot(plot_data, aes(x = room_type, y = median_price, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = dollar(median_price)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.6
      ) +
      scale_fill_manual(
        values = c("NYC" = "#4E79A7", "LA" = "#E15759"),
        guide = if (show_both_cities) guide_legend(title = "City") else "none"
      ) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = if (show_both_cities) {
          "Median Price by Room Type"
        } else {
          paste("Median Price by Room Type -", price_city_filter())
        },
        subtitle = paste(snapshot_label(selected_price_month()), "- hotel and shared rooms excluded"),
        x = NULL,
        y = NULL,
        fill = "City"
      )
  })

  output$price_neighbourhood_ui <- renderUI({
    if (input$price_city == "all") {
      fluidRow(
        column(6, plotOutput("price_neighbourhood_nyc", height = "360px")),
        column(6, plotOutput("price_neighbourhood_la", height = "360px"))
      )
    } else if (input$price_city == "NYC") {
      fluidRow(
        column(12, plotOutput("price_neighbourhood_nyc", height = "360px"))
      )
    } else {
      fluidRow(
        column(12, plotOutput("price_neighbourhood_la", height = "360px"))
      )
    }
  })

  output$price_neighbourhood_nyc <- renderPlot({
    plot_data <- pricing_neighbourhood_summary %>%
      filter(city == "NYC", snapshot_month == selected_price_month()) %>%
      arrange(desc(median_price)) %>%
      slice_head(n = 10) %>%
      mutate(neighbourhood = forcats::fct_reorder(neighbourhood, median_price))

    ggplot(plot_data, aes(x = neighbourhood, y = median_price)) +
      geom_col(fill = "#4E79A7") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Top 10 Neighborhoods by Price - NYC",
        subtitle = snapshot_label(selected_price_month()),
        x = NULL,
        y = NULL
      )
  })

  output$price_neighbourhood_la <- renderPlot({
    plot_data <- pricing_neighbourhood_summary %>%
      filter(city == "LA", snapshot_month == selected_price_month()) %>%
      arrange(desc(median_price)) %>%
      slice_head(n = 10) %>%
      mutate(neighbourhood = forcats::fct_reorder(neighbourhood, median_price))

    ggplot(plot_data, aes(x = neighbourhood, y = median_price)) +
      geom_col(fill = "#E15759") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Top 10 Neighborhoods by Price - LA",
        subtitle = snapshot_label(selected_price_month()),
        x = NULL,
        y = NULL
      )
  })

  output$price_accommodates_plot <- renderPlot({
    plot_data <- pricing_listings_clean %>%
      filter(
        snapshot_month == selected_price_month(),
        city %in% price_city_filter(),
        accommodates <= 10,
        price > 0,
        price < 1500
      ) %>%
      group_by(city, accommodates) %>%
      summarise(median_price = median(price, na.rm = TRUE), .groups = "drop")

    ggplot(plot_data, aes(x = accommodates, y = median_price, fill = city)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_continuous(breaks = 1:10) +
      labs(
        title = "Median Price by Guest Capacity",
        subtitle = "How much more do larger listings charge?",
        x = "Guests accommodated",
        y = "Median nightly price",
        fill = "City"
      )
  })

  output$price_host_type_plot <- renderPlot({
    plot_data <- pricing_host_summary %>%
      filter(snapshot_month == selected_price_month(), city %in% price_city_filter()) %>%
      mutate(
        host_type = if_else(is_multi_listing_host, "Multi-listing", "Single-listing"),
        superhost = if_else(host_is_superhost, "Superhost", "Regular"),
        label = paste(host_type, "\n", superhost)
      )

    ggplot(plot_data, aes(x = label, y = median_price, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = dollar(median_price)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.2
      ) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Median Price by Host Type",
        subtitle = "Do superhosts or commercial hosts charge more?",
        x = NULL,
        y = "Median nightly price",
        fill = "City"
      )
  })

  output$price_review_plot <- renderPlot({
    plot_data <- pricing_listings_clean %>%
      filter(
        city %in% price_city_filter(),
        snapshot_month == selected_price_month(),
        room_type %in% main_room_types,
        !is.na(review_scores_rating),
        price > 0,
        price < 1000
      )

    p <- ggplot(plot_data, aes(x = review_scores_rating, y = price, color = room_type)) +
      geom_point(alpha = 0.15, size = 0.8) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
      scale_color_manual(values = c("Entire home/apt" = "#4E79A7", "Private room" = "#F28E2B")) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Review Score vs. Nightly Price",
        subtitle = "Does a higher rating mean a higher price?",
        x = "Overall review score",
        y = "Nightly price",
        color = "Room type"
      )

    if (input$price_city == "all") {
      p + facet_wrap(~ city)
    } else {
      p
    }
  })

  output$price_note <- renderUI({
    HTML("<b>Takeaway:</b> Price differences are driven less by review scores and more by what is being
         offered: larger listings, premium neighborhoods, and some host types consistently command higher prices.
         The biggest gaps appear within each city, not just between the two cities.")
  })

  host_city_filter <- reactive({
    if (input$host_city == "all") c("NYC", "LA") else input$host_city
  })

  output$host_share_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(city %in% host_city_filter(), snapshot_month == selected_host_month())

    show_both_cities <- identical(sort(host_city_filter()), c("LA", "NYC"))

    ggplot(plot_data, aes(x = host_type, y = share, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = percent(share, accuracy = 0.1)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.8
      ) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 0.85)) +
      scale_fill_manual(
        values = c("NYC" = "#4E79A7", "LA" = "#E15759"),
        guide = if (show_both_cities) guide_legend(title = "City") else "none"
      ) +
      labs(
        title = if (show_both_cities) {
          "Share of Listings by Host Type"
        } else {
          paste("Share of Listings by Host Type -", host_city_filter())
        },
        subtitle = snapshot_label(selected_host_month()),
        x = NULL,
        y = "Share of listings",
        fill = "City"
      )
  })

  output$host_concentration_plot <- renderPlot({
    plot_data <- pricing_listings_clean %>%
      filter(city %in% host_city_filter(), snapshot_month == selected_host_month()) %>%
      group_by(city, host_id) %>%
      summarise(n_listings = max(host_listing_count, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        bucket = case_when(
          n_listings == 1 ~ "1",
          n_listings <= 3 ~ "2-3",
          n_listings <= 10 ~ "4-10",
          n_listings <= 25 ~ "11-25",
          TRUE ~ "26+"
        )
      ) %>%
      mutate(bucket = forcats::fct_relevel(bucket, "1", "2-3", "4-10", "11-25", "26+")) %>%
      count(city, bucket)

    ggplot(plot_data, aes(x = bucket, y = n, fill = city)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = label_comma()) +
      labs(
        title = "Number of Hosts by Listing Count",
        subtitle = "How concentrated is host ownership?",
        x = "Listings owned by host",
        y = "Number of hosts",
        fill = "City"
      )
  })

  output$host_availability_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(city %in% host_city_filter(), snapshot_month == selected_host_month())

    show_both_cities <- identical(sort(host_city_filter()), c("LA", "NYC"))

    ggplot(plot_data, aes(x = host_type, y = median_availability, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = round(median_availability, 0)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.8
      ) +
      scale_fill_manual(
        values = c("NYC" = "#4E79A7", "LA" = "#E15759"),
        guide = if (show_both_cities) guide_legend(title = "City") else "none"
      ) +
      labs(
        title = if (show_both_cities) {
          "Median Days Available by Host Type"
        } else {
          paste("Median Days Available by Host Type -", host_city_filter())
        },
        subtitle = snapshot_label(selected_host_month()),
        x = NULL,
        y = "Median days available per year",
        fill = "City"
      )
  })

  output$host_room_type_plot <- renderPlot({
    plot_data <- host_summary %>%
      filter(city %in% host_city_filter(), snapshot_month == selected_host_month(), room_type %in% main_room_types)

    ggplot(plot_data, aes(x = room_type, y = median_price, fill = host_type)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = dollar(median_price)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.4
      ) +
      scale_fill_manual(values = c("Single-listing host" = "#59A14F", "Multi-listing host" = "#F28E2B")) +
      scale_y_continuous(labels = dollar_format()) +
      facet_wrap(~ city) +
      labs(
        title = "Median Price by Host Type and Room Type",
        subtitle = "Do commercial hosts price differently?",
        x = NULL,
        y = "Median nightly price",
        fill = "Host type"
      )
  })

  output$host_superhost_plot <- renderPlot({
    plot_data <- pricing_listings_clean %>%
      filter(
        city %in% host_city_filter(),
        snapshot_month == selected_host_month(),
        room_type %in% main_room_types,
        price > 0,
        price < 1000
      ) %>%
      mutate(superhost_label = if_else(host_is_superhost, "Superhost", "Regular host"))

    ggplot(plot_data, aes(x = superhost_label, y = price, fill = city)) +
      geom_boxplot(outlier.alpha = 0.1, width = 0.5) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      facet_wrap(~ room_type) +
      labs(
        title = "Nightly Price: Superhost vs Regular Host",
        subtitle = "Does superhost status translate to higher prices?",
        x = NULL,
        y = "Nightly price",
        fill = "City"
      )
  })

  output$host_note <- renderUI({
    HTML("<b>Takeaway:</b> A relatively small group of multi-listing hosts controls a large share of the market.
         Comparing host mix, availability, and pricing helps show where Airbnb looks more like casual hosting
         and where it starts to resemble a more commercial operation.")
  })
}

