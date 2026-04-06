library(shiny)
library(tidyverse)
library(scales)
library(sf)

theme_set(theme_classic(base_size = 12))

read_listings_raw <- function(city) {
  read_csv(
    file.path("data-raw", city, paste0(city, " listings.csv")),
    show_col_types = FALSE
  ) %>%
    mutate(
      city = city,
      multi_host = calculated_host_listings_count > 1
    )
}

listings_raw <- bind_rows(
  read_listings_raw("NYC"),
  read_listings_raw("LA")
)

city_month_summary <- readRDS(file.path("data", "city_month_summary.rds"))
neighbourhood_shapes <- readRDS(file.path("data", "neighbourhoods_clean.rds"))

overview_summary <- listings_raw %>%
  filter(!is.na(neighbourhood), neighbourhood != "") %>%
  group_by(city, neighbourhood) %>%
  summarise(
    listing_count = n(),
    multi_share = mean(multi_host, na.rm = TRUE),
    median_availability = median(availability_365, na.rm = TRUE),
    .groups = "drop"
  )

map_summary <- neighbourhood_shapes %>%
  left_join(overview_summary, by = c("city", "neighbourhood"))

room_type_summary <- listings_raw %>%
  filter(!is.na(room_type), room_type != "") %>%
  count(city, room_type, name = "listing_count") %>%
  group_by(city) %>%
  mutate(share = listing_count / sum(listing_count)) %>%
  ungroup()

host_summary_plot <- listings_raw %>%
  mutate(
    host_type = case_when(
      multi_host ~ "Multi-listing host",
      TRUE ~ "Single-listing host"
    )
  ) %>%
  group_by(city, host_type) %>%
  summarise(
    listing_count = n(),
    median_availability = median(availability_365, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(city) %>%
  mutate(share = listing_count / sum(listing_count)) %>%
  ungroup()

function(input, output, session) {
  output$market_map_plot <- renderPlot({
    metric_labels <- c(
      listing_count = "Listing count",
      multi_share = "Multi-listing host share",
      median_availability = "Median availability"
    )

    plot_data <- map_summary %>%
      filter(city == input$overview_city)

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
        subtitle = metric_labels[[input$overview_metric]],
        fill = NULL
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })

  output$overview_plot <- renderPlot({
    plot_data <- overview_summary %>%
      filter(city == input$overview_city) %>%
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
        subtitle = metric_labels[[input$overview_metric]],
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

    plot_data <- room_type_summary %>%
      filter(city %in% input$room_city) %>%
      mutate(room_type = forcats::fct_relevel(room_type, "Entire home/apt", "Private room", "Hotel room", "Shared room"))

    ggplot(plot_data, aes(x = room_type, y = share, fill = city)) +
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
        title = "Room Type Composition by City",
        subtitle = "LA has a larger whole-home market, while NYC has a much bigger private-room share.",
        x = NULL,
        y = "Share of listings",
        fill = "City"
      )
  })

  output$room_type_note <- renderUI({
    HTML("<b>Takeaway:</b> This is one of the clearest differences between the two cities and likely one of the strongest parts of the final story.")
  })

  output$availability_plot <- renderPlot({
    metric_label <- case_when(
      input$availability_metric == "mean_availability_rate" ~ "Mean availability rate",
      TRUE ~ "Active listing count"
    )

    y_scale <- if (input$availability_metric == "mean_availability_rate") {
      scale_y_continuous(labels = label_percent(accuracy = 1))
    } else {
      scale_y_continuous(labels = label_comma())
    }

    ggplot(city_month_summary, aes(x = month_start, y = .data[[input$availability_metric]], color = city)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.2) +
      scale_color_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      y_scale +
      labs(
        title = "Calendar Availability Trends",
        subtitle = metric_label,
        x = NULL,
        y = NULL,
        color = "City"
      )
  })

  output$availability_note <- renderUI({
    HTML("<b>Note:</b> This shows calendar availability, not confirmed bookings. Lower availability could mean booked dates, blocked dates, or both.")
  })

  output$host_plot <- renderPlot({
    plot_data <- host_summary_plot %>%
      filter(city == input$host_city)

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
        subtitle = "Share of listings managed by single-listing vs multi-listing hosts",
        x = NULL,
        y = "Share of listings",
        fill = NULL
      )
  })

  output$host_availability_plot <- renderPlot({
    plot_data <- host_summary_plot %>%
      filter(city == input$host_city)

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
        subtitle = "Median annual availability",
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
