library(shiny)
library(tidyverse)
library(scales)
library(sf)

theme_set(theme_classic(base_size = 12))

shared_months <- readRDS(file.path("data", "pricing_shared_months.rds"))
city_month_summary <- readRDS(file.path("data", "city_month_summary.rds"))
neighbourhood_shapes <- readRDS(file.path("data", "neighbourhoods_clean.rds"))
snapshot_overview_summary <- readRDS(file.path("data", "snapshot_overview_summary.rds"))
snapshot_room_type_summary <- readRDS(file.path("data", "snapshot_room_type_summary.rds"))
snapshot_host_summary <- readRDS(file.path("data", "snapshot_host_summary.rds"))
pricing_city_summary <- readRDS(file.path("data", "pricing_city_summary.rds"))
pricing_room_type_summary <- readRDS(file.path("data", "pricing_room_type_summary.rds"))
pricing_neighbourhood_summary <- readRDS(file.path("data", "pricing_neighbourhood_summary.rds"))

snapshot_choices <- setNames(as.character(shared_months), format(shared_months, "%b %Y"))

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
    "snapshot_month",
    choices = snapshot_choices,
    selected = snapshot_choices[[1]]
  )

  selected_month <- reactive({
    req(input$snapshot_month)
    as.Date(input$snapshot_month)
  })

  output$market_map_plot <- renderPlot({
    metric_labels <- c(
      listing_count = "Listing count",
      multi_share = "Multi-listing host share",
      median_availability = "Median availability"
    )

    plot_data <- map_summary %>%
      filter(city == input$overview_city, snapshot_month == selected_month())

    fill_scale <- if (input$overview_metric == "multi_share") {
      scale_fill_viridis_c(labels = label_percent(accuracy = 1), na.value = "grey90")
    } else if (input$overview_metric == "listing_count") {
      scale_fill_viridis_c(labels = label_comma(), na.value = "grey90")
    } else {
      scale_fill_viridis_c(labels = label_number(), na.value = "grey90")
    }

    ggplot(plot_data) +
      geom_sf(aes(fill = .data[[input$overview_metric]]), color = "white", linewidth = 0.15) +
      fill_scale +
      labs(
        title = paste(input$overview_city, "Neighborhood Map"),
        subtitle = paste(metric_labels[[input$overview_metric]], "for", snapshot_label(selected_month())),
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
      filter(city == input$overview_city, snapshot_month == selected_month()) %>%
      arrange(desc(.data[[input$overview_metric]])) %>%
      slice_head(n = input$top_n) %>%
      mutate(neighbourhood = forcats::fct_reorder(neighbourhood, .data[[input$overview_metric]]))

    metric_labels <- c(
      listing_count = "Listing count",
      multi_share = "Multi-listing host share",
      median_availability = "Median availability"
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
        title = paste("Top", input$top_n, "Neighborhoods"),
        subtitle = paste(metric_labels[[input$overview_metric]], "for", snapshot_label(selected_month())),
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

  output$room_type_plot <- renderPlot({
    validate(need(length(input$room_city) > 0, "Select at least one city."))

    plot_data <- snapshot_room_type_summary %>%
      filter(city %in% input$room_city, snapshot_month == selected_month()) %>%
      mutate(room_type_group = if_else(room_type %in% main_room_types, room_type, "Other")) %>%
      group_by(city, snapshot_month, room_type_group) %>%
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
        title = paste("Room Type Composition in", snapshot_label(selected_month())),
        subtitle = "The main comparison is between entire homes and private rooms; hotel and shared rooms are grouped into Other.",
        x = NULL,
        y = "Share of listings",
        fill = "City"
      )
  })

  output$room_type_note <- renderUI({
    HTML("<b>Takeaway:</b> The big difference is still the same: LA leans much more toward whole-home listings, while NYC has a much larger private-room share.")
  })

  output$price_city_plot <- renderPlot({
    plot_data <- pricing_city_summary %>%
      filter(snapshot_month == selected_month())

    ggplot(plot_data, aes(x = city, y = median_price, fill = city)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = dollar(median_price)), vjust = -0.35, size = 4) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = paste("Median Price by City in", snapshot_label(selected_month())),
        subtitle = "Based on listings with usable price values",
        x = NULL,
        y = NULL,
        fill = NULL
      )
  })

  output$price_room_plot <- renderPlot({
    plot_data <- pricing_room_type_summary %>%
      filter(snapshot_month == selected_month(), room_type %in% main_room_types) %>%
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
        subtitle = paste(snapshot_label(selected_month()), "(hotel and shared rooms excluded)"),
        x = NULL,
        y = NULL,
        fill = "City"
      )
  })

  output$price_neighbourhood_plot <- renderPlot({
    plot_data <- pricing_neighbourhood_summary %>%
      filter(city == input$price_city, snapshot_month == selected_month()) %>%
      arrange(desc(median_price)) %>%
      slice_head(n = input$price_top_n) %>%
      mutate(neighbourhood = forcats::fct_reorder(neighbourhood, median_price))

    ggplot(plot_data, aes(x = neighbourhood, y = median_price)) +
      geom_col(fill = "#B07AA1") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = paste("Top", input$price_top_n, "Neighborhoods by Median Price in", input$price_city),
        subtitle = snapshot_label(selected_month()),
        x = NULL,
        y = NULL
      )
  })

  output$price_note <- renderUI({
    HTML("<b>Note:</b> Pricing is shown only for the shared months with usable listing prices in both cities. The room-type price chart focuses on entire homes and private rooms because hotel and shared rooms are too small and too extreme to be useful in the main comparison.")
  })

  output$availability_plot <- renderPlot({
    ggplot(city_month_summary, aes(x = month_start, y = mean_availability_rate, color = city)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.2) +
      scale_color_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = label_percent(accuracy = 1)) +
      labs(
        title = "Calendar Availability Trends",
        subtitle = "Mean availability rate",
        x = NULL,
        y = NULL,
        color = "City"
      )
  })

  output$availability_note <- renderUI({
    HTML("<b>Note:</b> This shows calendar availability, not confirmed bookings. Lower availability could mean booked dates, blocked dates, or both.")
  })

  output$host_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(city == input$host_city, snapshot_month == selected_month())

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
        subtitle = snapshot_label(selected_month()),
        x = NULL,
        y = "Share of listings",
        fill = NULL
      )
  })

  output$host_availability_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(city == input$host_city, snapshot_month == selected_month())

    ggplot(plot_data, aes(x = host_type, y = median_availability, fill = host_type)) +
      geom_col(width = 0.65) +
      geom_text(
        aes(label = round(median_availability, 0)),
        vjust = -0.4,
        size = 4
      ) +
      scale_fill_manual(values = c("Single-listing host" = "#59A14F", "Multi-listing host" = "#F28E2B")) +
      labs(
        title = paste("Availability by Host Type in", input$host_city),
        subtitle = snapshot_label(selected_month()),
        x = NULL,
        y = "Median availability_365",
        fill = NULL
      )
  })

  output$host_note <- renderUI({
    city_text <- if (input$host_city == "NYC") {
      "NYC still has a strong multi-listing host presence, but it is less dominant than in LA and single-listing hosts look much less available overall."
    } else {
      "LA looks more commercialized, with more multi-listing hosts and higher availability tied to that inventory."
    }

    HTML(paste0("<b>Takeaway:</b> ", city_text))
  })
}
