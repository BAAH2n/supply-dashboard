# ===============================================
# INTERACTIVE SUPPLY DASHBOARD
# ===============================================
# Just run: source("dashboard_interactive.R")
# Dashboard will open in RStudio Viewer

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)

# ===============================================
# DATA CHECK
# ===============================================

if(!exists("long_data")) {
  stop("Variable 'long_data' not found!\n",
       "Load data first:\n",
       "  long_data <- read.csv('your_file.csv')\n")
}

# Конвертація дати якщо потрібно
if(!inherits(long_data$date, "Date")) {
  suppressWarnings({
    current_year <- format(Sys.Date(), "%Y")
    
    # Спроба різних форматів
    if(is.character(long_data$date)) {
      # Формат "12-Jan" або "26-Jan"
      long_data$date <- as.Date(paste0(long_data$date, "-", current_year), 
                                 format = "%d-%b-%Y")
    } else {
      long_data$date <- as.Date(long_data$date)
    }
  })
}

# ===============================================
# CALCULATION FUNCTIONS
# ===============================================

calculate_change <- function(data, metric) {
  metric_data <- data %>%
    filter(metric_name == metric) %>%
    arrange(date)
  
  if(nrow(metric_data) < 2) {
    return(list(current = NA, previous = NA, change = NA, change_pct = NA))
  }
  
  current <- tail(metric_data$value_num, 1)
  previous <- metric_data$value_num[nrow(metric_data) - 1]
  change <- current - previous
  change_pct <- ifelse(previous != 0, (change / previous) * 100, 0)
  
  return(list(
    current = current,
    previous = previous,
    change = change,
    change_pct = change_pct
  ))
}

get_arrow_icon <- function(change) {
  if(is.na(change)) return("→")
  if(change > 0) return("↑")
  if(change < 0) return("↓")
  return("→")
}

get_change_color <- function(change, is_positive_good = TRUE) {
  if(is.na(change)) return("#6c757d")
  
  if(is_positive_good) {
    if(change > 0) return("#28a745")
    if(change < 0) return("#dc3545")
  } else {
    if(change > 0) return("#dc3545")
    if(change < 0) return("#28a745")
  }
  return("#6c757d")
}

# Function to format metric values with NA handling
format_metric_value <- function(value, add_percent = TRUE, decimal = 1) {
  if(is.na(value)) return("No data")
  formatted <- round(value, decimal)
  if(add_percent) {
    return(paste0(formatted, "%"))
  }
  return(as.character(formatted))
}

# Function to format change
format_change <- function(change_obj) {
  if(is.na(change_obj$change) || is.na(change_obj$change_pct)) {
    return("No data")
  }
  # FIXED: show absolute change, not percentage change from previous value
  paste0(get_arrow_icon(change_obj$change), " ",
         abs(round(change_obj$change, 1)), " p.p.")
}

# ===============================================
# METRICS CALCULATION
# ===============================================

availability_avg <- calculate_change(long_data, "Availability Average")
availability_low <- calculate_change(long_data, "Availability Lowest")
completion <- calculate_change(long_data, "%Completion")
rejects <- calculate_change(long_data, "%Rejects")

# PU Retention
pu_retention <- calculate_change(long_data, "PU Retention")

# CR Free to Paid
cr_free_to_paid <- calculate_change(long_data, "CR Free to Paid")

# NDA violations
nda_latest <- long_data %>%
  filter(metric_name == "NDA & AI usage abuse") %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  pull(value_raw)

nda_violations <- 0
if(!is.na(nda_latest) && nda_latest != "N/A") {
  nda_match <- regmatches(nda_latest, regexec("NDA - ([0-9]+)", nda_latest))
  if(length(nda_match[[1]]) > 1) {
    nda_violations <- as.numeric(nda_match[[1]][2])
  }
  ai_match <- regmatches(nda_latest, regexec("AI abuse - ([0-9]+)", nda_latest))
  if(length(ai_match[[1]]) > 1) {
    nda_violations <- nda_violations + as.numeric(ai_match[[1]][2])
  }
}

# ===============================================
# CHART FUNCTIONS
# ===============================================

plot_availability <- function(data) {
  avg_data <- data %>%
    filter(metric_name == "Availability Average") %>%
    arrange(date)
  
  lowest_data <- data %>%
    filter(metric_name == "Availability Lowest") %>%
    arrange(date)
  
  # Check if data exists
  if(nrow(avg_data) == 0 || nrow(lowest_data) == 0 || all(is.na(avg_data$value_num))) {
    # Empty plot with message
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = 8, color = "#6c757d") +
      theme_void() +
      labs(title = "Supply Liquidity (Availability)")
    return(ggplotly(p))
  }
  
  plot_data <- avg_data %>%
    select(date, avg = value_num) %>%
    left_join(lowest_data %>% select(date, lowest = value_num), by = "date") %>%
    filter(!is.na(avg) | !is.na(lowest))
  
  # Get only dates that have data
  dates_with_data <- plot_data$date
  
  p <- ggplot(plot_data, aes(x = date)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 35,
             fill = "#dc3545", alpha = 0.15) +
    annotate("text", x = plot_data$date[1], y = 32,
             label = "Risk Zone (<35%)", hjust = 0, color = "#dc3545", size = 3) +
    geom_ribbon(aes(ymin = lowest, ymax = avg), fill = "#007bff", alpha = 0.25) +
    geom_line(aes(y = avg, color = "Average"), linewidth = 1.2) +
    geom_point(aes(y = avg), color = "#007bff", size = 3) +
    geom_line(aes(y = lowest, color = "Lowest"), linetype = "dashed", linewidth = 1) +
    geom_point(aes(y = lowest), color = "#0056b3", size = 2) +
    geom_text(aes(y = avg, label = paste0(round(avg, 1), "%")), 
              vjust = -0.8, size = 3.5, fontface = "bold") +
    geom_text(aes(y = lowest, label = paste0(round(lowest, 1), "%")), 
              vjust = 1.5, size = 3, color = "#0056b3") +
    scale_color_manual(name = "", values = c("Average" = "#007bff", "Lowest" = "#0056b3")) +
    labs(title = "Supply Liquidity (Availability)", x = NULL, y = "Availability (%)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_x_date(breaks = dates_with_data, date_labels = "%b %d")
  
  ggplotly(p) %>%
    layout(hovermode = "x unified")
}

plot_utilization <- function(data) {
  util_data <- data %>%
    filter(metric_name == "Utilization") %>%
    arrange(date) %>%
    filter(!is.na(value_num))
  
  # Check if data exists
  if(nrow(util_data) == 0 || all(is.na(util_data$value_num))) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = 8, color = "#6c757d") +
      theme_void() +
      labs(title = "Utilization")
    return(ggplotly(p))
  }
  
  target <- util_data$target_num[1]
  dates_with_data <- util_data$date
  
  p <- ggplot(util_data, aes(x = date)) +
    geom_hline(yintercept = target, linetype = "dashed", color = "#6c757d", linewidth = 1) +
    annotate("text", x = util_data$date[1], y = target + 2, 
             label = paste0("Target: ", target, "%"), hjust = 0, color = "#6c757d", size = 3.5) +
    geom_line(aes(y = value_num), color = "#28a745", linewidth = 1.2) +
    geom_point(aes(y = value_num), color = "#28a745", size = 3) +
    geom_text(aes(y = value_num, label = paste0(round(value_num, 1), "%")), 
              vjust = -1, size = 3.5, fontface = "bold") +
    labs(title = "Utilization", x = NULL, y = "Utilization (%)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
    scale_x_date(breaks = dates_with_data, date_labels = "%b %d")
  
  ggplotly(p) %>%
    layout(hovermode = "x unified")
}

plot_csat <- function(data) {
  csat_data <- data %>%
    filter(metric_name %in% c("CSAT overall", "CSAT trial")) %>%
    arrange(date, metric_name) %>%
    filter(!is.na(value_num))
  
  # Check if data exists
  if(nrow(csat_data) == 0 || all(is.na(csat_data$value_num))) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = 8, color = "#6c757d") +
      theme_void() +
      labs(title = "CSAT Metrics")
    return(ggplotly(p))
  }
  
  dates_with_data <- unique(csat_data$date)
  
  p <- ggplot(csat_data, aes(x = date, y = value_num, color = metric_name, group = metric_name)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_text(aes(label = round(value_num, 2)), vjust = -1, size = 3, show.legend = FALSE) +
    labs(title = "CSAT Metrics", x = NULL, y = "CSAT Score", color = "Metric") +
    scale_color_manual(values = c("CSAT overall" = "#007bff", "CSAT trial" = "#17a2b8")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          panel.grid.minor = element_blank()) +
    scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 0.5)) +
    scale_x_date(breaks = dates_with_data, date_labels = "%b %d")
  
  ggplotly(p) %>%
    layout(hovermode = "x unified")
}

plot_frt <- function(data) {
  frt_data <- data %>%
    filter(metric_name == "FRT") %>%
    arrange(date) %>%
    filter(!is.na(value_num))
  
  # Check if data exists
  if(nrow(frt_data) == 0 || all(is.na(frt_data$value_num))) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = 8, color = "#6c757d") +
      theme_void() +
      labs(title = "First Response Time (FRT)")
    return(ggplotly(p))
  }
  
  target <- frt_data$target_num[1]
  dates_with_data <- frt_data$date
  
  p <- ggplot(frt_data, aes(x = date)) +
    geom_hline(yintercept = target, linetype = "dashed", color = "#6c757d", linewidth = 1) +
    annotate("text", x = frt_data$date[1], y = target + 0.5, 
             label = paste0("Target: ", target, " min"), hjust = 0, color = "#6c757d", size = 3.5) +
    geom_line(aes(y = value_num), color = "#ff6b6b", linewidth = 1.2) +
    geom_point(aes(y = value_num), color = "#ff6b6b", size = 3) +
    geom_text(aes(y = value_num, label = round(value_num, 1)), 
              vjust = -1, size = 3.5, fontface = "bold") +
    labs(title = "First Response Time (FRT)", x = NULL, y = "FRT (minutes)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          panel.grid.minor = element_blank()) +
    scale_x_date(breaks = dates_with_data, date_labels = "%b %d")
  
  ggplotly(p) %>%
    layout(hovermode = "x unified")
}

plot_communication <- function(data) {
  comm_data <- data %>%
    filter(metric_name %in% c("Pings coverage", "Messenger answer rate", "Engagement messages coverage")) %>%
    arrange(date) %>%
    filter(!is.na(value_num))
  
  # Check if data exists
  if(nrow(comm_data) == 0 || all(is.na(comm_data$value_num))) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = 8, color = "#6c757d") +
      theme_void() +
      labs(title = "Communication Metrics")
    return(ggplotly(p))
  }
  
  dates_with_data <- unique(comm_data$date)
  
  p <- ggplot(comm_data, aes(x = date, y = value_num, fill = metric_name)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(aes(label = paste0(round(value_num, 1), "%")), 
              position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
    labs(title = "Communication Metrics", x = NULL, y = "Percentage (%)", fill = "Metric") +
    scale_fill_manual(values = c("Pings coverage" = "#ffc107",
                                  "Messenger answer rate" = "#28a745",
                                  "Engagement messages coverage" = "#17a2b8")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          panel.grid.minor = element_blank()) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_x_date(breaks = dates_with_data, date_labels = "%b %d")
  
  ggplotly(p) %>%
    layout(hovermode = "x unified")
}

plot_reviews <- function(data) {
  reviews_data <- data %>%
    filter(metric_name == "% Positive reviews") %>%
    arrange(date) %>%
    filter(!is.na(value_num))
  
  # Check if data exists
  if(nrow(reviews_data) == 0 || all(is.na(reviews_data$value_num))) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = 8, color = "#6c757d") +
      theme_void() +
      labs(title = "% Positive Reviews")
    return(ggplotly(p))
  }
  
  target <- reviews_data$target_num[1]
  dates_with_data <- reviews_data$date
  
  p <- ggplot(reviews_data, aes(x = date)) +
    geom_hline(yintercept = target, linetype = "dashed", color = "#6c757d", linewidth = 1) +
    geom_line(aes(y = value_num), color = "#28a745", linewidth = 1.2) +
    geom_point(aes(y = value_num), color = "#28a745", size = 3) +
    geom_text(aes(y = value_num, label = paste0(round(value_num, 1), "%")), 
              vjust = -1, size = 3.5, fontface = "bold") +
    labs(title = "% Positive Reviews", x = NULL, y = "Percentage (%)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(limits = c(70, 100), breaks = seq(70, 100, 5)) +
    scale_x_date(breaks = dates_with_data, date_labels = "%b %d")
  
  ggplotly(p) %>%
    layout(hovermode = "x unified")
}

# ===============================================
# SHINY UI
# ===============================================

# Get the latest date from data for last update
last_data_date <- max(long_data$date, na.rm = TRUE)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
      .main-header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; 
                     padding: 30px; border-radius: 12px; margin-bottom: 20px; }
      .kpi-box { background: white; padding: 20px; border-radius: 12px; margin-bottom: 15px; 
                 box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
      .kpi-value { font-size: 36px; font-weight: 700; color: #212529; margin: 10px 0; }
      .kpi-label { font-size: 12px; color: #6c757d; font-weight: 600; text-transform: uppercase; }
      .kpi-change { font-size: 14px; font-weight: 600; margin-top: 5px; }
      .alert-box { padding: 20px; border-radius: 12px; margin: 20px 0; font-weight: 600; text-align: center; }
      .alert-safe { background: #d4edda; color: #155724; border: 2px solid #c3e6cb; }
      .alert-danger { background: #f8d7da; color: #721c24; border: 2px solid #f5c6cb; }
      .chart-box { background: white; padding: 20px; border-radius: 12px; margin-bottom: 15px; 
                   box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
      h2 { font-size: 18px; font-weight: 700; color: #212529; margin-bottom: 15px; }
    "))
  ),
  
  # Header
  div(class = "main-header",
      h1("Supply Metrics Dashboard", style = "margin: 0; font-size: 32px;"),
      p(paste("Last update:", format(last_data_date, "%d.%m.%Y")), 
        style = "margin: 10px 0 0 0; font-size: 14px; opacity: 0.9;")
  ),
  
  # Level 1: Key Metrics
  h2("Key Metrics", style = "margin: 20px 0 15px 0; font-size: 20px; font-weight: 700;"),
  
  fluidRow(
    column(3, div(class = "kpi-box",
                  div(class = "kpi-label", "AVAILABILITY AVERAGE"),
                  div(class = "kpi-value", format_metric_value(availability_avg$current)),
                  div(style = "font-size: 12px; color: #6c757d; margin: 5px 0;",
                      paste0("Target: ", ifelse(!is.na(long_data %>% 
                                                        filter(metric_name == "Availability Average") %>% 
                                                        pull(target_num) %>% first()),
                                                round(long_data %>% 
                                                        filter(metric_name == "Availability Average") %>% 
                                                        pull(target_num) %>% first(), 1), "—"), "%")),
                  div(class = "kpi-change", 
                      style = paste0("color: ", get_change_color(availability_avg$change)),
                      format_change(availability_avg))
    )),
    column(3, div(class = "kpi-box",
                  div(class = "kpi-label", "AVAILABILITY LOWEST"),
                  div(class = "kpi-value", format_metric_value(availability_low$current)),
                  div(style = "font-size: 12px; color: #6c757d; margin: 5px 0;",
                      paste0("Target: ", ifelse(!is.na(long_data %>% 
                                                        filter(metric_name == "Availability Lowest") %>% 
                                                        pull(target_num) %>% first()),
                                                round(long_data %>% 
                                                        filter(metric_name == "Availability Lowest") %>% 
                                                        pull(target_num) %>% first(), 1), "—"), "%")),
                  div(class = "kpi-change", 
                      style = paste0("color: ", get_change_color(availability_low$change)),
                      format_change(availability_low))
    )),
    column(3, div(class = "kpi-box",
                  div(class = "kpi-label", "% COMPLETION"),
                  div(class = "kpi-value", format_metric_value(completion$current)),
                  div(style = "font-size: 12px; color: #6c757d; margin: 5px 0;",
                      paste0("Target: ", ifelse(!is.na(long_data %>% 
                                                        filter(metric_name == "%Completion") %>% 
                                                        pull(target_num) %>% first()),
                                                round(long_data %>% 
                                                        filter(metric_name == "%Completion") %>% 
                                                        pull(target_num) %>% first(), 1), "—"), "%")),
                  div(class = "kpi-change", 
                      style = paste0("color: ", get_change_color(completion$change)),
                      format_change(completion))
    )),
    column(3, div(class = "kpi-box",
                  div(class = "kpi-label", "PU RETENTION"),
                  div(class = "kpi-value", format_metric_value(pu_retention$current)),
                  div(style = "font-size: 12px; color: #6c757d; margin: 5px 0;",
                      paste0("Target: ", ifelse(!is.na(long_data %>% 
                                                        filter(metric_name == "PU Retention") %>% 
                                                        pull(target_num) %>% first()),
                                                round(long_data %>% 
                                                        filter(metric_name == "PU Retention") %>% 
                                                        pull(target_num) %>% first(), 1), "—"), "%")),
                  div(class = "kpi-change", 
                      style = paste0("color: ", get_change_color(pu_retention$change)),
                      format_change(pu_retention))
    ))
  ),
  
  # Level 2: Operational Metrics
  h2("Level 2: Operational Metrics"),
  fluidRow(
    column(6, div(class = "chart-box", plotlyOutput("plot_availability", height = "350px"))),
    column(3, div(class = "chart-box", plotlyOutput("plot_utilization", height = "350px"))),
    column(3, div(class = "chart-box", 
                  div(style = "text-align: center; padding: 50px 0;",
                      div(class = "kpi-label", "% REJECTS"),
                      div(class = "kpi-value", format_metric_value(rejects$current, add_percent = TRUE, decimal = 2)),
                      if(!is.na(rejects$current)) {
                        div(style = paste0("padding: 8px 20px; border-radius: 20px; display: inline-block; margin-top: 15px; ",
                                           ifelse(rejects$current <= 1.0, 
                                                  "background: #d4edda; color: #155724;",
                                                  "background: #f8d7da; color: #721c24;")),
                            ifelse(rejects$current <= 1.0, "OK", "ALERT"))
                      } else {
                        div(style = "margin-top: 15px; color: #6c757d; font-size: 14px;", "—")
                      }
                  )
    ))
  ),
  
  # Level 3: Quality and Business Metrics
  h2("Level 3: Quality & Business Metrics"),
  
  # CSAT та CR Free to Paid
  fluidRow(
    column(6, div(class = "chart-box", plotlyOutput("plot_csat", height = "350px"))),
    column(6, div(class = "chart-box",
                  h3("CR Free to Paid", style = "font-size: 16px; font-weight: 700; margin-bottom: 20px;"),
                  if(!is.na(cr_free_to_paid$current)) {
                    div(style = "text-align: center; padding: 50px 0;",
                        div(class = "kpi-value", format_metric_value(cr_free_to_paid$current, add_percent = TRUE, decimal = 2)),
                        div(class = "kpi-change",
                            style = paste0("color: ", get_change_color(cr_free_to_paid$change)),
                            format_change(cr_free_to_paid)),
                        div(style = "margin-top: 20px; color: #6c757d; font-size: 14px;",
                            paste0("Previous: ", format_metric_value(cr_free_to_paid$previous, add_percent = TRUE, decimal = 2)))
                    )
                  } else {
                    div(style = "text-align: center; padding: 80px 0; color: #6c757d; font-size: 18px;",
                        "No data")
                  }
    ))
  ),
  
  fluidRow(
    column(6, div(class = "chart-box", plotlyOutput("plot_frt", height = "350px"))),
    column(6, div(class = "chart-box", plotlyOutput("plot_reviews", height = "350px")))
  ),
  
  fluidRow(
    column(12, div(class = "chart-box", plotlyOutput("plot_communication", height = "350px")))
  ),
  
  # Risk Alert
  div(class = ifelse(nda_violations > 0, "alert-box alert-danger", "alert-box alert-safe"),
      ifelse(nda_violations > 0, 
             paste0("⚠️ ALERT: ", nda_violations, " NDA/AI Violations detected"),
             "✓ No NDA or AI Usage Violations")
  )
)

# ===============================================
# SHINY SERVER
# ===============================================

server <- function(input, output, session) {
  
  output$plot_availability <- renderPlotly({
    plot_availability(long_data)
  })
  
  output$plot_utilization <- renderPlotly({
    plot_utilization(long_data)
  })
  
  output$plot_csat <- renderPlotly({
    plot_csat(long_data)
  })
  
  output$plot_frt <- renderPlotly({
    plot_frt(long_data)
  })
  
  output$plot_communication <- renderPlotly({
    plot_communication(long_data)
  })
  
  output$plot_reviews <- renderPlotly({
    plot_reviews(long_data)
  })
}

# ===============================================
# LAUNCH APPLICATION
# ===============================================

cat("\n")
cat("==============================================\n")
cat("🚀 LAUNCHING SUPPLY DASHBOARD\n")
cat("==============================================\n")
cat("Dashboard is opening in RStudio Viewer...\n")
cat("To stop, press ESC or Stop\n")
cat("==============================================\n\n")

shinyApp(ui = ui, server = server)
