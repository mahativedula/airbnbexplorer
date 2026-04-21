library(shiny)
library(tidyverse)
library(scales)
library(sf)

theme_set(theme_classic(base_size = 12))

shared_months                 <- readRDS(file.path("data", "pricing_shared_months.rds"))
snapshot_city_summary         <- readRDS(file.path("data", "snapshot_city_summary.rds"))
neighbourhood_shapes          <- readRDS(file.path("data", "neighbourhoods_clean.rds"))
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
story_snapshot_month  <- max(pricing_months)

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
  
  # ── MARKET OVERVIEW ───────────────────────────────────────────────────────
  
  updateSelectInput(
    session,
    "overview_month",
    choices  = listing_month_choices,
    selected = tail(listing_month_choices, 1)
  )
  
  selected_overview_month <- reactive({
    req(input$overview_month)
    as.Date(input$overview_month)
  })
  
  # Shared map helpers
  map_palette <- c("#d6e6f5", "#a8cce6", "#78b1d6", "#4d8fc0", "#2867a8", "#0b3d78")
  
  build_map_scale <- function(values, labels, transform = NULL) {
    values <- values[is.finite(values)]
    breaks <- unique(as.numeric(quantile(
      values, probs = seq(0, 1, length.out = 7),
      na.rm = TRUE, names = FALSE
    )))
    if (length(breaks) < 3) breaks <- pretty(values, n = 6)
    scale_args <- list(
      colours = map_palette, breaks = breaks, labels = labels,
      na.value = "grey95", show.limits = TRUE
    )
    if (!is.null(transform)) scale_args$trans <- transform
    do.call(scale_fill_stepsn, scale_args)
  }
  
  # Chart 1a: Map — NYC
  output$market_map_nyc <- renderPlot({
    metric_labels <- c(
      listing_count       = "Listing count",
      multi_share         = "Multi-listing host share",
      median_availability = "Median days available per year"
    )
    
    plot_data <- map_summary %>%
      filter(city == "NYC", snapshot_month == selected_overview_month())
    
    fill_scale <- if (input$overview_metric == "multi_share") {
      build_map_scale(plot_data$multi_share, label_percent(accuracy = 1))
    } else if (input$overview_metric == "listing_count") {
      build_map_scale(plot_data$listing_count, label_comma(), transform = "log10")
    } else {
      build_map_scale(plot_data$median_availability, label_number())
    }
    
    ggplot(plot_data) +
      geom_sf(aes(fill = .data[[input$overview_metric]]),
              color = "white", linewidth = 0.15) +
      fill_scale +
      labs(
        title    = paste("NYC —", metric_labels[[input$overview_metric]]),
        subtitle = snapshot_label(selected_overview_month()),
        fill = NULL
      ) +
      theme(
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })
  
  # Chart 1b: Map — LA
  output$market_map_la <- renderPlot({
    metric_labels <- c(
      listing_count       = "Listing count",
      multi_share         = "Multi-listing host share",
      median_availability = "Median days available per year"
    )
    
    plot_data <- map_summary %>%
      filter(city == "LA", snapshot_month == selected_overview_month())
    
    fill_scale <- if (input$overview_metric == "multi_share") {
      build_map_scale(plot_data$multi_share, label_percent(accuracy = 1))
    } else if (input$overview_metric == "listing_count") {
      build_map_scale(plot_data$listing_count, label_comma(), transform = "log10")
    } else {
      build_map_scale(plot_data$median_availability, label_number())
    }
    
    ggplot(plot_data) +
      geom_sf(aes(fill = .data[[input$overview_metric]]),
              color = "white", linewidth = 0.15) +
      fill_scale +
      labs(
        title    = paste("LA —", metric_labels[[input$overview_metric]]),
        subtitle = snapshot_label(selected_overview_month()),
        fill = NULL
      ) +
      theme(
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })
  
  # Chart 2a: Top 10 neighborhoods — NYC
  output$overview_plot_nyc <- renderPlot({
    metric_labels <- c(
      listing_count       = "Listing count",
      multi_share         = "Multi-listing host share",
      median_availability = "Median days available per year"
    )
    value_labels <- list(
      listing_count       = label_comma(),
      multi_share         = label_percent(accuracy = 1),
      median_availability = label_number()
    )
    
    plot_data <- snapshot_overview_summary %>%
      filter(city == "NYC",
             snapshot_month == selected_overview_month()) %>%
      arrange(desc(.data[[input$overview_metric]])) %>%
      slice_head(n = 10) %>%
      mutate(neighbourhood = forcats::fct_reorder(
        neighbourhood, .data[[input$overview_metric]]))
    
    ggplot(plot_data, aes(x = neighbourhood,
                          y = .data[[input$overview_metric]])) +
      geom_col(fill = "#4E79A7") +
      coord_flip() +
      scale_y_continuous(labels = value_labels[[input$overview_metric]]) +
      labs(
        title    = "Top 10 Neighborhoods — NYC",
        subtitle = paste(metric_labels[[input$overview_metric]],
                         "—", snapshot_label(selected_overview_month())),
        x = NULL, y = NULL
      )
  })
  
  # Chart 2b: Top 10 neighborhoods — LA
  output$overview_plot_la <- renderPlot({
    metric_labels <- c(
      listing_count       = "Listing count",
      multi_share         = "Multi-listing host share",
      median_availability = "Median days available per year"
    )
    value_labels <- list(
      listing_count       = label_comma(),
      multi_share         = label_percent(accuracy = 1),
      median_availability = label_number()
    )
    
    plot_data <- snapshot_overview_summary %>%
      filter(city == "LA",
             snapshot_month == selected_overview_month()) %>%
      arrange(desc(.data[[input$overview_metric]])) %>%
      slice_head(n = 10) %>%
      mutate(neighbourhood = forcats::fct_reorder(
        neighbourhood, .data[[input$overview_metric]]))
    
    ggplot(plot_data, aes(x = neighbourhood,
                          y = .data[[input$overview_metric]])) +
      geom_col(fill = "#E15759") +
      coord_flip() +
      scale_y_continuous(labels = value_labels[[input$overview_metric]]) +
      labs(
        title    = "Top 10 Neighborhoods — LA",
        subtitle = paste(metric_labels[[input$overview_metric]],
                         "—", snapshot_label(selected_overview_month())),
        x = NULL, y = NULL
      )
  })
  
  # Chart 3: Room type composition — both cities, dodged bars
  output$room_type_plot <- renderPlot({
    plot_data <- snapshot_room_type_summary %>%
      filter(snapshot_month == selected_overview_month()) %>%
      mutate(room_type_group = if_else(
        room_type %in% main_room_types, room_type, "Other")) %>%
      group_by(city, room_type_group) %>%
      summarise(share = sum(share), .groups = "drop") %>%
      mutate(room_type_group = forcats::fct_relevel(
        room_type_group, "Entire home/apt", "Private room", "Other"))
    
    ggplot(plot_data, aes(x = room_type_group, y = share, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = percent(share, accuracy = 0.1)),
        position = position_dodge(width = 0.9),
        vjust = -0.35, size = 3.8
      ) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 0.85)) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      labs(
        title    = "Room Type Composition",
        subtitle = snapshot_label(selected_overview_month()),
        x = NULL, y = "Share of listings", fill = "City"
      )
  })
  
  # Chart 4: Room type share over time — both cities, one chart
  output$room_type_trend_plot <- renderPlot({
    plot_data <- snapshot_room_type_summary %>%
      filter(room_type %in% main_room_types)
    
    ggplot(plot_data, aes(x = as.Date(snapshot_month), y = share,
                          color = city, linetype = room_type)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.4) +
      scale_color_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = label_percent()) +
      scale_x_date(date_labels = "%b\n%Y") +
      labs(
        title    = "Room Type Share Over Time",
        subtitle = "Has the listing mix shifted across snapshots?",
        x = NULL, y = "Share of listings",
        color = "City", linetype = "Room type"
      )
  })
  
  output$overview_note <- renderUI({
    HTML("<b>Takeaway:</b> NYC listings cluster densely in Manhattan and Brooklyn,
         reflecting limited space and high tourism demand. LA listings spread across
         entertainment hubs like Hollywood and Venice, reflecting its car-dependent,
         sprawling layout. Both cities are dominated by entire home listings, though
         NYC shows a stronger private room presence.")
  })
  
  # ── PRICING ───────────────────────────────────────────────────────────────
  
  updateSelectInput(
    session,
    "price_month",
    choices  = pricing_month_choices,
    selected = tail(pricing_month_choices, 1)
  )
  
  selected_price_month <- reactive({
    req(input$price_month)
    as.Date(input$price_month)
  })
  
  # Chart 1: City-level median price — both cities (existing, unchanged)
  output$price_city_plot <- renderPlot({
    plot_data <- pricing_city_summary %>%
      filter(snapshot_month == selected_price_month(),
             )
    
    ggplot(plot_data, aes(x = city, y = median_price, fill = city)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = dollar(median_price)), vjust = -0.35, size = 4) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title    = paste("Median Price by City —",
                         snapshot_label(selected_price_month())),
        subtitle = "Based on listings with valid price values",
        x = NULL, y = NULL, fill = NULL
      )
  })
  
  # Chart 2: Price by room type — both cities (existing, unchanged)
  output$price_room_plot <- renderPlot({
    plot_data <- pricing_room_type_summary %>%
      filter(snapshot_month == selected_price_month(),
             room_type %in% main_room_types) %>%
      mutate(room_type = forcats::fct_relevel(
        room_type, "Entire home/apt", "Private room"))
    
    ggplot(plot_data, aes(x = room_type, y = median_price, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = dollar(median_price)),
        position = position_dodge(width = 0.9),
        vjust = -0.35, size = 3.6
      ) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title    = "Median Price by Room Type",
        subtitle = paste(snapshot_label(selected_price_month()),
                         "— hotel and shared rooms excluded"),
        x = NULL, y = NULL, fill = "City"
      )
  })
  
  # Chart 3a: Top 10 neighborhoods by price — NYC (updated to side-by-side)
  output$price_neighbourhood_nyc <- renderPlot({
    plot_data <- pricing_neighbourhood_summary %>%
      filter(city == "NYC",
             snapshot_month == selected_price_month()) %>%
      arrange(desc(median_price)) %>%
      slice_head(n = 10) %>%
      mutate(neighbourhood = forcats::fct_reorder(neighbourhood, median_price))
    
    ggplot(plot_data, aes(x = neighbourhood, y = median_price)) +
      geom_col(fill = "#4E79A7") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title    = "Top 10 Neighborhoods by Price — NYC",
        subtitle = snapshot_label(selected_price_month()),
        x = NULL, y = NULL
      )
  })
  
  # Chart 3b: Top 10 neighborhoods by price — LA (updated to side-by-side)
  output$price_neighbourhood_la <- renderPlot({
    plot_data <- pricing_neighbourhood_summary %>%
      filter(city == "LA",
             snapshot_month == selected_price_month()) %>%
      arrange(desc(median_price)) %>%
      slice_head(n = 10) %>%
      mutate(neighbourhood = forcats::fct_reorder(neighbourhood, median_price))
    
    ggplot(plot_data, aes(x = neighbourhood, y = median_price)) +
      geom_col(fill = "#E15759") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title    = "Top 10 Neighborhoods by Price — LA",
        subtitle = snapshot_label(selected_price_month()),
        x = NULL, y = NULL
      )
  })
  
  # Chart 4: Price by guest capacity — new
  output$price_accommodates_plot <- renderPlot({
    plot_data <- pricing_listings_clean %>%
      filter(
        snapshot_month == selected_price_month(),
        city == input$price_city,
        accommodates <= 10,
        price > 0, price < 1500
      ) %>%
      group_by(city, accommodates) %>%
      summarise(median_price = median(price, na.rm = TRUE), .groups = "drop")
    
    ggplot(plot_data, aes(x = accommodates, y = median_price, fill = city)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_continuous(breaks = 1:10) +
      labs(
        title    = "Median Price by Guest Capacity",
        subtitle = "How much more do larger listings charge?",
        x = "Guests accommodated", y = "Median nightly price",
        fill = "City"
      )
  })
  
  # Chart 5: Price by host type × superhost — new
  output$price_host_type_plot <- renderPlot({
    plot_data <- pricing_host_summary %>%
      filter(snapshot_month == selected_price_month(),
             city == input$price_city) %>%
      mutate(
        host_type = if_else(is_multi_listing_host,
                            "Multi-listing", "Single-listing"),
        superhost = if_else(host_is_superhost, "Superhost", "Regular"),
        label     = paste(host_type, "\n", superhost)
      )
    
    ggplot(plot_data, aes(x = label, y = median_price, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = dollar(median_price)),
        position = position_dodge(width = 0.9),
        vjust = -0.35, size = 3.2
      ) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title    = "Median Price by Host Type",
        subtitle = "Do superhosts or commercial hosts charge more?",
        x = NULL, y = "Median nightly price", fill = "City"
      )
  })
  
  # Chart 6: Review score vs price scatterplot — new
  output$price_review_plot <- renderPlot({
    plot_data <- pricing_listings_clean %>%
      filter(
        city == input$price_city,
        snapshot_month == selected_price_month(),
        room_type %in% main_room_types,
        !is.na(review_scores_rating),
        price > 0, price < 1000,
        city == input$price_city
      )
    
    city_color <- if (input$price_city == "NYC") "#4E79A7" else "#E15759"
    
    ggplot(plot_data, aes(x = review_scores_rating, y = price,
                          color = room_type)) +
      geom_point(alpha = 0.2, size = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
      scale_color_manual(values = c(
        "Entire home/apt" = "#4E79A7",
        "Private room"    = "#F28E2B"
      )) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title    = paste("Review Score vs. Nightly Price —", input$price_city),
        subtitle = "Does a higher rating mean a higher price?",
        x = "Overall review score", y = "Nightly price",
        color = "Room type"
      )
  })
  
  output$price_note <- renderUI({
    HTML("<b>Takeaway:</b> Guest capacity is the strongest price driver — each additional
       guest adds meaningfully to nightly cost in both cities. Superhosts charge a modest
       premium, but the gap is smaller than you might expect. Review scores show little
       correlation with price, suggesting hosts don't consistently price based on quality.")
  })
  
  # ── HOST ACTIVITY ─────────────────────────────────────────────────────────
  
  updateSelectInput(
    session,
    "host_month",
    choices  = listing_month_choices,
    selected = tail(listing_month_choices, 1)
  )
  
  selected_host_month <- reactive({
    req(input$host_month)
    as.Date(input$host_month)
  })
  
  host_city_filter <- reactive({
    if (input$host_city == "all") c("NYC", "LA") else input$host_city
  })
  
  # Chart 1: Host type share — both cities side by side
  output$host_share_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(city %in% host_city_filter(),
             snapshot_month == selected_host_month())
    
    ggplot(plot_data, aes(x = host_type, y = share, fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = percent(share, accuracy = 0.1)),
        position = position_dodge(width = 0.9),
        vjust = -0.35, size = 3.8
      ) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 0.85)) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      labs(
        title    = "Share of Listings by Host Type",
        subtitle = snapshot_label(selected_host_month()),
        x = NULL, y = "Share of listings", fill = "City"
      )
  })
  
  # Chart 2: Listings per host distribution — shows market concentration
  output$host_concentration_plot <- renderPlot({
    plot_data <- pricing_listings_clean %>%
      filter(
        city %in% host_city_filter(),
        snapshot_month == selected_price_month()
      ) %>%
      group_by(city, host_id) %>%
      summarise(n_listings = max(host_listing_count, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(bucket = case_when(
        n_listings == 1  ~ "1",
        n_listings <= 3  ~ "2-3",
        n_listings <= 10 ~ "4-10",
        n_listings <= 25 ~ "11-25",
        TRUE             ~ "26+"
      )) %>%
      mutate(bucket = forcats::fct_relevel(
        bucket, "1", "2-3", "4-10", "11-25", "26+")) %>%
      count(city, bucket)
    
    ggplot(plot_data, aes(x = bucket, y = n, fill = city)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = label_comma()) +
      labs(
        title    = "Number of Hosts by Listing Count",
        subtitle = "How concentrated is host ownership?",
        x = "Listings owned by host", y = "Number of hosts",
        fill = "City"
      )
  })
  
  # Chart 3: Availability by host type
  output$host_availability_plot <- renderPlot({
    plot_data <- snapshot_host_summary %>%
      filter(city %in% host_city_filter(),
             snapshot_month == selected_host_month())
    
    ggplot(plot_data, aes(x = host_type, y = median_availability,
                          fill = city)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = round(median_availability, 0)),
        position = position_dodge(width = 0.9),
        vjust = -0.35, size = 3.8
      ) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      labs(
        title    = "Median Days Available by Host Type",
        subtitle = snapshot_label(selected_host_month()),
        x = NULL, y = "Median days available per year",
        fill = "City"
      )
  })
  
  # Chart 4: Host type x room type x price
  output$host_room_type_plot <- renderPlot({
    plot_data <- host_summary %>%
      filter(city %in% host_city_filter(),
             snapshot_month == selected_host_month(),
             room_type %in% main_room_types)
    
    ggplot(plot_data, aes(x = room_type, y = median_price,
                          fill = host_type)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = dollar(median_price)),
        position = position_dodge(width = 0.9),
        vjust = -0.35, size = 3.4
      ) +
      scale_fill_manual(values = c(
        "Single-listing host" = "#59A14F",
        "Multi-listing host"  = "#F28E2B"
      )) +
      scale_y_continuous(labels = dollar_format()) +
      facet_wrap(~ city) +
      labs(
        title    = "Median Price by Host Type and Room Type",
        subtitle = "Do commercial hosts price differently?",
        x = NULL, y = "Median nightly price",
        fill = "Host type"
      )
  })
  
  # Chart 5: Superhost vs regular price — boxplot
  output$host_superhost_plot <- renderPlot({
    plot_data <- pricing_listings_clean %>%
      filter(
        city %in% host_city_filter(),
        snapshot_month == selected_price_month(),
        room_type %in% main_room_types,
        price > 0, price < 1000
      ) %>%
      mutate(
        superhost_label = if_else(host_is_superhost, "Superhost", "Regular host")
      )
    
    ggplot(plot_data, aes(x = superhost_label, y = price, fill = city)) +
      geom_boxplot(outlier.alpha = 0.1, width = 0.5) +
      scale_fill_manual(values = c("NYC" = "#4E79A7", "LA" = "#E15759")) +
      scale_y_continuous(labels = dollar_format()) +
      facet_wrap(~ room_type) +
      labs(
        title    = "Nightly Price: Superhost vs Regular Host",
        subtitle = "Does superhost status translate to higher prices?",
        x = NULL, y = "Nightly price", fill = "City"
      )
  })
  
  output$host_note <- renderUI({
    HTML("<b>Takeaway:</b> Both cities are dominated by multi-listing hosts in terms of
       inventory share, but LA shows a more commercialized market with higher availability
       from commercial operators. Superhost status commands only a modest price premium,
       suggesting guests value it more for trust than for paying extra. A small number of
       hosts control a disproportionate share of listings in both cities.")
  })
}