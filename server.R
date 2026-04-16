library(shiny)
library(tidyverse)
library(scales)
library(sf)

theme_set(theme_classic(base_size = 12))

shared_months <- readRDS(file.path("data", "pricing_shared_months.rds"))
snapshot_city_summary <- readRDS(file.path("data", "snapshot_city_summary.rds"))
neighbourhood_shapes <- readRDS(file.path("data", "neighbourhoods_clean.rds"))
snapshot_overview_summary <- readRDS(file.path("data", "snapshot_overview_summary.rds"))
snapshot_room_type_summary <- readRDS(file.path("data", "snapshot_room_type_summary.rds"))
snapshot_host_summary <- readRDS(file.path("data", "snapshot_host_summary.rds"))
pricing_city_summary <- readRDS(file.path("data", "pricing_city_summary.rds"))
pricing_room_type_summary <- readRDS(file.path("data", "pricing_room_type_summary.rds"))
pricing_neighbourhood_summary <- readRDS(file.path("data", "pricing_neighbourhood_summary.rds"))

listing_months <- snapshot_overview_summary %>%
  distinct(snapshot_month) %>%
  pull(snapshot_month) %>%
  sort()

pricing_months <- sort(shared_months)

listing_month_choices <- setNames(as.character(listing_months), format(listing_months, "%b %Y"))
pricing_month_choices <- setNames(as.character(pricing_months), format(pricing_months, "%b %Y"))
story_snapshot_month <- max(pricing_months)

map_summary <- neighbourhood_shapes %>%
  left_join(
    snapshot_overview_summary,
    by = c("city", "neighbourhood_group", "neighbourhood")
  )

snapshot_label <- function(x) {
  format(as.Date(x), "%b %Y")
}

main_room_types <- c("Entire home/apt", "Private room")

function(input, output, session) {
  updateSelectInput(
    session,
    "overview_month",
    choices = listing_month_choices,
    selected = tail(listing_month_choices, 1)
  )

  updateSelectInput(
    session,
    "price_month",
    choices = pricing_month_choices,
    selected = tail(pricing_month_choices, 1)
  )

  updateSelectInput(
    session,
    "host_month",
    choices = listing_month_choices,
    selected = tail(listing_month_choices, 1)
  )

  selected_overview_month <- reactive({
    req(input$overview_month)
    as.Date(input$overview_month)
  })

  selected_price_month <- reactive({
    req(input$price_month)
    as.Date(input$price_month)
  })

  selected_host_month <- reactive({
    req(input$host_month)
    as.Date(input$host_month)
  })

  story_room_type_data <- reactive({
    snapshot_room_type_summary %>%
      filter(snapshot_month == story_snapshot_month) %>%
      mutate(room_type_group = if_else(room_type %in% main_room_types, room_type, "Other")) %>%
      group_by(city, snapshot_month, room_type_group) %>%
      summarise(share = sum(share), .groups = "drop") %>%
      mutate(room_type_group = forcats::fct_relevel(room_type_group, "Entire home/apt", "Private room", "Other"))
  })

  output$story_room_type_plot <- renderPlot({
    ggplot(story_room_type_data(), aes(x = room_type_group, y = share, fill = city)) +
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
        title = paste("Room Type Composition in", snapshot_label(story_snapshot_month)),
        subtitle = "The main difference starts with what kind of listings dominate each city.",
        x = NULL,
        y = "Share of listings",
        fill = "City"
      )
  })

  output$story_availability_plot <- renderPlot({
    ggplot(snapshot_city_summary, aes(x = month_start, y = mean_availability_365, color = city)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.4) +
      scale_color_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = label_number(accuracy = 1)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      labs(
        title = "Days Available per Year Across Snapshot Months",
        subtitle = "Across the three listing snapshots, LA listings are available for more days per year on average.",
        x = NULL,
        y = "Average days available per year",
        color = "City"
      )
  })

  output$story_host_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(snapshot_month == story_snapshot_month)

    ggplot(plot_data, aes(x = host_type, y = share, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = percent(share, accuracy = 0.1)),
        position = position_dodge(width = 0.9),
        vjust = -0.35,
        size = 3.8
      ) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 0.8)) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      labs(
        title = paste("Host Structure in", snapshot_label(story_snapshot_month)),
        subtitle = "Los Angeles is more dominated by multi-listing hosts.",
        x = NULL,
        y = "Share of listings",
        fill = "City"
      )
  })

  output$market_map_plot <- renderPlot({
    metric_labels <- c(
      listing_count = "Listing count",
      multi_share = "Multi-listing host share",
      median_availability = "Median days available per year"
    )

    plot_data <- map_summary %>%
      filter(city == input$overview_city, snapshot_month == selected_overview_month())

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

    fill_scale <- if (input$overview_metric == "multi_share") {
      build_map_scale(plot_data$multi_share, label_percent(accuracy = 1))
    } else if (input$overview_metric == "listing_count") {
      build_map_scale(plot_data$listing_count, label_comma(), transform = "log10")
    } else {
      build_map_scale(plot_data$median_availability, label_number())
    }

    ggplot(plot_data) +
      geom_sf(aes(fill = .data[[input$overview_metric]]), color = "white", linewidth = 0.15) +
      fill_scale +
      labs(
        title = paste(input$overview_city, "Neighborhood Map"),
        subtitle = paste(metric_labels[[input$overview_metric]], "for", snapshot_label(selected_overview_month())),
        fill = NULL
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })

  output$overview_plot <- renderPlot({
    plot_data <- snapshot_overview_summary %>%
      filter(city == input$overview_city, snapshot_month == selected_overview_month()) %>%
      arrange(desc(.data[[input$overview_metric]])) %>%
      slice_head(n = 10) %>%
      mutate(neighbourhood = forcats::fct_reorder(neighbourhood, .data[[input$overview_metric]]))

    metric_labels <- c(
      listing_count = "Listing count",
      multi_share = "Multi-listing host share",
      median_availability = "Median days available per year"
    )

    value_labels <- c(
      listing_count = label_comma(),
      multi_share = label_percent(accuracy = 1),
      median_availability = label_number()
    )

    ggplot(plot_data, aes(x = neighbourhood, y = .data[[input$overview_metric]])) +
      geom_col(fill = "#4E79A7") +
      coord_flip() +
      scale_y_continuous(labels = value_labels[[input$overview_metric]]) +
      labs(
        title = "Top 10 Neighborhoods",
        subtitle = paste(metric_labels[[input$overview_metric]], "for", snapshot_label(selected_overview_month())),
        x = NULL,
        y = NULL
      )
  })

  output$overview_note <- renderUI({
    if (input$overview_city == "NYC") {
      HTML("<b>Takeaway:</b> NYC listings are spread across both tourist-heavy and dense residential neighborhoods, especially Manhattan and Brooklyn.")
    } else {
      HTML("<b>Takeaway:</b> LA listings are especially concentrated in places like Hollywood, Venice, Santa Monica, and West Hollywood.")
    }
  })

  output$price_city_plot <- renderPlot({
    plot_data <- pricing_city_summary %>%
      filter(snapshot_month == selected_price_month())

    ggplot(plot_data, aes(x = city, y = median_price, fill = city)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = dollar(median_price)), vjust = -0.35, size = 4) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = paste("Median Price by City in", snapshot_label(selected_price_month())),
        subtitle = "Based on listings with usable price values",
        x = NULL,
        y = NULL,
        fill = NULL
      )
  })

  output$price_room_plot <- renderPlot({
    plot_data <- pricing_room_type_summary %>%
      filter(snapshot_month == selected_price_month(), room_type %in% main_room_types) %>%
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
        title = "Median Price by Main Room Type",
        subtitle = paste(snapshot_label(selected_price_month()), "(hotel and shared rooms excluded)"),
        x = NULL,
        y = NULL,
        fill = "City"
      )
  })

  output$price_neighbourhood_plot <- renderPlot({
    plot_data <- pricing_neighbourhood_summary %>%
      filter(city == input$price_city, snapshot_month == selected_price_month()) %>%
      arrange(desc(median_price)) %>%
      slice_head(n = 10) %>%
      mutate(neighbourhood = forcats::fct_reorder(neighbourhood, median_price))

    ggplot(plot_data, aes(x = neighbourhood, y = median_price)) +
      geom_col(fill = "#B07AA1") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = paste("Top 10 Neighborhoods by Median Price in", input$price_city),
        subtitle = snapshot_label(selected_price_month()),
        x = NULL,
        y = NULL
      )
  })

  output$price_note <- renderUI({
    HTML("<b>Note:</b> Pricing is shown only for the shared months with usable listing prices in both cities. The room-type price chart focuses on entire homes and private rooms because hotel and shared rooms are too small and too extreme to be useful in the main comparison.")
  })

  output$host_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(city == input$host_city, snapshot_month == selected_host_month())

    ggplot(plot_data, aes(x = host_type, y = share, fill = host_type)) +
      geom_col(width = 0.65) +
      geom_text(
        aes(label = percent(share, accuracy = 0.1)),
        vjust = -0.4,
        size = 4
      ) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 0.8)) +
      scale_fill_manual(values = c("Single-listing host" = "#59A14F", "Multi-listing host" = "#F28E2B")) +
      labs(
        title = paste("Host Structure in", input$host_city),
        subtitle = snapshot_label(selected_host_month()),
        x = NULL,
        y = "Share of listings",
        fill = NULL
      )
  })

  output$host_availability_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(city == input$host_city, snapshot_month == selected_host_month())

    ggplot(plot_data, aes(x = host_type, y = median_availability, fill = host_type)) +
      geom_col(width = 0.65) +
      geom_text(
        aes(label = round(median_availability, 0)),
        vjust = -0.4,
        size = 4
      ) +
      scale_fill_manual(values = c("Single-listing host" = "#59A14F", "Multi-listing host" = "#F28E2B")) +
      labs(
        title = paste("Days Available per Year by Host Type in", input$host_city),
        subtitle = snapshot_label(selected_host_month()),
        x = NULL,
        y = "Median days available per year",
        fill = NULL
      )
  })

  output$host_note <- renderUI({
    city_text <- if (input$host_city == "NYC") {
      "NYC still has a strong multi-listing host presence, but it is less dominant than in LA and single-listing hosts tend to be available for fewer days overall."
    } else {
      "LA looks more commercialized, with more multi-listing hosts and listings that stay available for more days overall."
    }

    HTML(paste0("<b>Takeaway:</b> ", city_text))
  })
}
