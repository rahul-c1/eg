# Large Scale Trading Analysis Shiny App - Fixed Version
# Author: Trading Analysis System
# Version: 2.1 Fixed
# Description: Comprehensive trading analysis for large-scale stock screening and performance evaluation

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(shinyWidgets)
library(shinycssloaders)
library(data.table)
library(tidyr)
library(viridis)  # Added for viridis color scales

# Load data from CSV file with error handling
tryCatch({
  trading_data <- data.table::fread("latest_metrics.csv")
  }, error = function(e) {
  # Create sample data if file doesn't exist
  set.seed(123)
  n_stocks <- 1000
  trading_data <- data.table(
    symbol = paste0("STOCK", sprintf("%03d", 1:n_stocks)),
    date = rep(as.Date("2025-09-19"), n_stocks),
    close = runif(n_stocks, 5, 500),
    volume = sample(100000:10000000, n_stocks, replace = TRUE),
    roc_ytd = rnorm(n_stocks, 10, 30),
    roc5 = runif(n_stocks, 0.95, 1.05),
    roc10 = runif(n_stocks, 0.90, 1.10),
    up_day_percentage = runif(n_stocks, 30, 70),
    max_up_streak = sample(1:30, n_stocks, replace = TRUE),
    recent_up_trend_21d = sample(5:21, n_stocks, replace = TRUE),
    recent_up_trend_63d = sample(15:63, n_stocks, replace = TRUE),
    ma_4d = runif(n_stocks, 5, 500),
    ma_7d = runif(n_stocks, 5, 500),
    ma_21d = runif(n_stocks, 5, 500),
    ma_42d = runif(n_stocks, 5, 500),
    total_up_days_252d = sample(80:200, n_stocks, replace = TRUE),
    consecutive_up_days = sample(0:10, n_stocks, replace = TRUE),
    strong_up_days = sample(10:100, n_stocks, replace = TRUE),
    first_date = rep(as.Date("2024-01-01"), n_stocks)
  )
  cat("Using sample data since file not found\n")
})

# Filter for valid stocks
symbol_vol_gt_100k <- trading_data %>% 
  group_by(symbol) %>% 
  filter(any(close > 5, na.rm = TRUE)) %>% 
  filter(any(volume > 100000, na.rm = TRUE)) %>% 
  filter(any(date == max(date, na.rm = TRUE), na.rm = TRUE)) %>%
  pull(symbol) %>% 
  unique()

trading_data <- trading_data %>% 
  filter(symbol %in% symbol_vol_gt_100k) %>% 
  setDT()

# Convert date columns to Date type if they exist as character
if("date" %in% names(trading_data)) {
  trading_data$date <- as.Date(trading_data$date)
}
if("first_date" %in% names(trading_data)) {
  trading_data$first_date <- as.Date(trading_data$first_date)
}

# Calculate composite performance score
calculate_performance_score <- function(data) {
  data %>%
    mutate(
      # Ensure numeric columns and handle NAs
      across(c(roc_ytd, roc5, roc10, close, volume, up_day_percentage, 
               max_up_streak, recent_up_trend_21d, recent_up_trend_63d,
               ma_4d, ma_7d, ma_21d, ma_42d), as.numeric),
      
      # Handle missing values with reasonable defaults
      roc_ytd = coalesce(roc_ytd, 0),
      roc5 = coalesce(roc5, 1),
      roc10 = coalesce(roc10, 1),
      up_day_percentage = coalesce(up_day_percentage, 50),
      max_up_streak = coalesce(max_up_streak, 5),
      recent_up_trend_21d = coalesce(recent_up_trend_21d, 10),
      recent_up_trend_63d = coalesce(recent_up_trend_63d, 30),
      
      # Normalize metrics to 0-100 scale
      roc_ytd_score = pmax(0, pmin(100, (roc_ytd + 50) * 1)),
      momentum_score = pmax(0, pmin(100, ((roc5 - 0.95) * 1000 + (roc10 - 0.90) * 500) / 15)),
      trend_score = pmax(0, (recent_up_trend_21d / 21 + recent_up_trend_63d / 63) * 50),
      consistency_score = pmax(0, pmin(100, up_day_percentage)),
      volume_score = pmax(0, pmin(100, log10(pmax(1000, volume)) * 10)),
      streak_score = pmax(0, pmin(100, max_up_streak * 4)),
      
      # Composite score (weighted average)
      performance_score = (
        roc_ytd_score * 0.3 +
          momentum_score * 0.2 +
          trend_score * 0.2 +
          consistency_score * 0.15 +
          volume_score * 0.1 +
          streak_score * 0.05
      ),
      
      # Risk-adjusted score
      volatility_proxy = abs(roc5 - 1) + abs(roc10 - 1),
      risk_adjusted_score = performance_score / (1 + volatility_proxy * 10),
      
      # Categories
      performance_category = case_when(
        performance_score >= 80 ~ "Excellent",
        performance_score >= 65 ~ "Good",
        performance_score >= 50 ~ "Average",
        performance_score >= 35 ~ "Below Average",
        TRUE ~ "Poor"
      ),
      
      # Technical strength - handle missing MA values
      ma_alignment = case_when(
        !is.na(close) & !is.na(ma_4d) & !is.na(ma_7d) & !is.na(ma_21d) &
          close > ma_4d & ma_4d > ma_7d & ma_7d > ma_21d ~ "Strong Uptrend",
        !is.na(close) & !is.na(ma_21d) & close > ma_21d ~ "Uptrend",
        !is.na(close) & !is.na(ma_21d) & !is.na(ma_42d) & 
          close < ma_21d & close > ma_42d ~ "Sideways",
        TRUE ~ "Downtrend"
      )
    )
}

# Calculate performance scores
trading_data <- calculate_performance_score(trading_data)

# Add sectors if not present
if(!"sector" %in% names(trading_data)) {
  set.seed(123)  # For reproducible sectors
  trading_data$sector <- sample(
    c("Technology", "Healthcare", "Finance", "Energy", "Consumer", "Industrial", "Materials", "Utilities"), 
    nrow(trading_data), replace = TRUE, 
    prob = c(0.2, 0.15, 0.15, 0.1, 0.15, 0.1, 0.1, 0.05)
  )
}

# Add missing columns if they don't exist
required_cols <- c("total_up_days_252d", "consecutive_up_days", "strong_up_days")
for(col in required_cols) {
  if(!col %in% names(trading_data)) {
    set.seed(123)
    if(col == "total_up_days_252d") {
      trading_data[[col]] <- sample(80:200, nrow(trading_data), replace = TRUE)
    } else if(col == "consecutive_up_days") {
      trading_data[[col]] <- sample(0:10, nrow(trading_data), replace = TRUE)
    } else if(col == "strong_up_days") {
      trading_data[[col]] <- sample(10:100, nrow(trading_data), replace = TRUE)
    }
  }
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Advanced Trading Analysis Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stock Screener", tabName = "screener", icon = icon("search")),
      menuItem("Top Performers", tabName = "topperformers", icon = icon("trophy")),
      menuItem("Performance Analysis", tabName = "performance", icon = icon("chart-line")),
      menuItem("Technical Analysis", tabName = "technical", icon = icon("chart-bar")),
      menuItem("Risk Analysis", tabName = "risk", icon = icon("shield-alt")),
      menuItem("Sector Analysis", tabName = "sector", icon = icon("industry")),
      menuItem("Advanced Filters", tabName = "filters", icon = icon("filter")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .small-box { margin-bottom: 15px; }
        .filter-box { background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px; }
        .navbar-custom-menu > .nav > li > .dropdown-menu { width: 300px; }
      "))
    ),
    
    tabItems(
      # Stock Screener Tab
      tabItem(tabName = "screener",
              fluidRow(
                box(
                  title = "Stock Screening Filters", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  fluidRow(
                    column(3,
                           numericRangeInput("ytd_range", "YTD Return Range (%)", 
                                             value = c(-50, 100), min = -100, max = 200)
                    ),
                    column(3,
                           numericRangeInput("volume_range", "Volume Range", 
                                             value = c(10000, 10000000), min = 1000, max = 50000000)
                    ),
                    column(3,
                           numericRangeInput("score_range", "Performance Score", 
                                             value = c(0, 100), min = 0, max = 100)
                    ),
                    column(3,
                           selectInput("ma_trend", "MA Trend", 
                                       choices = c("All", "Strong Uptrend", "Uptrend", "Sideways", "Downtrend"),
                                       selected = "All")
                    )
                  ),
                  fluidRow(
                    column(4,
                           numericInput("min_up_percentage", "Min Up Day %", value = 45, min = 0, max = 100)
                    ),
                    column(4,
                           numericInput("min_streak", "Min Up Streak", value = 5, min = 0, max = 50)
                    ),
                    column(4,
                           numericInput("top_n", "Show Top N Stocks", value = 50, min = 10, max = 500)
                    )
                  ),
                  actionButton("apply_filters", "Apply Filters", class = "btn-primary", style = "margin-top: 10px;")
                )
              ),
              fluidRow(
                valueBoxOutput("filtered_count"),
                valueBoxOutput("avg_performance"),
                valueBoxOutput("top_performer")
              ),
              fluidRow(
                box(
                  title = "Filtered Stock Results", status = "success", solidHeader = TRUE,
                  width = 12, height = 600,
                  withSpinner(DT::dataTableOutput("screener_table"))
                )
              )
      ),
      
      # Top Performers Tab
      tabItem(tabName = "topperformers",
              fluidRow(
                box(
                  title = "Top Performers Filters", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE, collapsed = FALSE,
                  fluidRow(
                    column(3,
                           numericRangeInput("price_range_top", "Price Range ($)", 
                                             value = c(1, 500), min = 0.1, max = 1000, step = 1)
                    ),
                    column(3,
                           numericRangeInput("volume_range_top", "Volume Range", 
                                             value = c(10000, 50000000), min = 1000, max = 100000000, step = 10000)
                    ),
                    column(3,
                           numericInput("top_n_performers", "Show Top N Stocks", 
                                        value = 50, min = 10, max = 200, step = 10)
                    ),
                    column(3,
                           selectInput("perf_category_filter", "Performance Category", 
                                       choices = c("All", "Excellent", "Good", "Average", "Below Average", "Poor"),
                                       selected = "All")
                    )
                  ),
                  fluidRow(
                    column(6,
                           sliderInput("min_perf_score_top", "Minimum Performance Score", 
                                       min = 0, max = 100, value = 30, step = 5)
                    ),
                    column(6,
                           actionButton("apply_top_filters", "Apply Filters", class = "btn-primary", style = "margin-top: 25px;")
                    )
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("total_stocks"),
                valueBoxOutput("filtered_top_count"),
                valueBoxOutput("avg_filtered_score")
              ),
              fluidRow(
                box(
                  title = "Top Performing Stocks (Filtered)", status = "primary", solidHeader = TRUE,
                  width = 6, height = 500,
                  withSpinner(plotlyOutput("top_performers_chart"))
                ),
                box(
                  title = "Filtered Performance Score Distribution", status = "success", solidHeader = TRUE,
                  width = 6, height = 500,
                  withSpinner(plotlyOutput("score_distribution"))
                )
              ),
              fluidRow(
                box(
                  title = "Price vs Performance (Filtered)", status = "warning", solidHeader = TRUE,
                  width = 6, height = 450,
                  withSpinner(plotlyOutput("price_performance_scatter"))
                ),
                box(
                  title = "Volume vs Performance (Filtered)", status = "info", solidHeader = TRUE,
                  width = 6, height = 450,
                  withSpinner(plotlyOutput("volume_performance_scatter"))
                )
              ),
              fluidRow(
                box(
                  title = "Filtered Top Performers Table", status = "success", solidHeader = TRUE,
                  width = 12, height = 500,
                  withSpinner(DT::dataTableOutput("top_performers_table"))
                )
              )
      ),
      
      # Performance Analysis Tab
      tabItem(tabName = "performance",
              fluidRow(
                box(
                  title = "YTD Returns Heatmap (Top 100)", status = "primary", solidHeader = TRUE,
                  width = 12, height = 500,
                  withSpinner(plotlyOutput("returns_heatmap"))
                )
              ),
              fluidRow(
                box(
                  title = "Momentum Analysis (Top 200)", status = "success", solidHeader = TRUE,
                  width = 6, height = 450,
                  withSpinner(plotlyOutput("momentum_analysis"))
                ),
                box(
                  title = "Trend Strength Analysis (Top 100)", status = "warning", solidHeader = TRUE,
                  width = 6, height = 450,
                  withSpinner(plotlyOutput("trend_analysis"))
                )
              )
      ),
      
      # Technical Analysis Tab
      tabItem(tabName = "technical",
              fluidRow(
                box(
                  title = "Moving Average Alignment (Top 50)", status = "primary", solidHeader = TRUE,
                  width = 12, height = 500,
                  withSpinner(plotlyOutput("ma_alignment_chart"))
                )
              ),
              fluidRow(
                box(
                  title = "Volume vs YTD Performance (Top 500)", status = "success", solidHeader = TRUE,
                  width = 6, height = 450,
                  withSpinner(plotlyOutput("volume_performance"))
                ),
                box(
                  title = "Consecutive Up Days Analysis (Top 200)", status = "info", solidHeader = TRUE,
                  width = 6, height = 450,
                  withSpinner(plotlyOutput("consecutive_days"))
                )
              )
      ),
      
      # Risk Analysis Tab
      tabItem(tabName = "risk",
              fluidRow(
                box(
                  title = "Risk-Return Efficient Frontier (Top 500)", status = "primary", solidHeader = TRUE,
                  width = 6, height = 500,
                  withSpinner(plotlyOutput("efficient_frontier"))
                ),
                box(
                  title = "Risk-Adjusted Performance Rankings", status = "warning", solidHeader = TRUE,
                  width = 6, height = 500,
                  withSpinner(plotlyOutput("volatility_analysis"))
                )
              ),
              fluidRow(
                box(
                  title = "Risk-Adjusted Top Performers", status = "success", solidHeader = TRUE,
                  width = 12, height = 500,
                  withSpinner(DT::dataTableOutput("risk_adjusted_table"))
                )
              )
      ),
      
      # Sector Analysis Tab
      tabItem(tabName = "sector",
              fluidRow(
                box(
                  title = "Sector Performance Comparison", status = "primary", solidHeader = TRUE,
                  width = 12, height = 500,
                  withSpinner(plotlyOutput("sector_performance"))
                )
              ),
              fluidRow(
                box(
                  title = "Sector Risk-Return Profile", status = "success", solidHeader = TRUE,
                  width = 6, height = 450,
                  withSpinner(plotlyOutput("sector_risk_return"))
                ),
                box(
                  title = "Best Stock by Sector", status = "info", solidHeader = TRUE,
                  width = 6, height = 450,
                  withSpinner(DT::dataTableOutput("sector_best"))
                )
              )
      ),
      
      # Advanced Filters Tab
      tabItem(tabName = "filters",
              fluidRow(
                box(
                  title = "Advanced Multi-Criteria Stock Finder", status = "primary", solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4,
                           h4("Performance Criteria", style = "color: #3c8dbc;"),
                           sliderInput("min_ytd", "Minimum YTD Return (%)", min = -50, max = 100, value = 5),
                           sliderInput("min_momentum", "Minimum 5-day ROC", min = 0.9, max = 1.1, value = 0.98, step = 0.01),
                           sliderInput("min_trend_21d", "Min Recent Trend (21d)", min = 5, max = 21, value = 10)
                    ),
                    column(4,
                           h4("Volume & Liquidity", style = "color: #00a65a;"),
                           sliderInput("min_volume", "Minimum Volume", min = 1000, max = 5000000, value = 50000),
                           sliderInput("min_up_days", "Min Up Days (252d)", min = 80, max = 200, value = 110),
                           sliderInput("min_consistency", "Min Up Day %", min = 40, max = 70, value = 48)
                    ),
                    column(4,
                           h4("Technical Strength", style = "color: #f39c12;"),
                           sliderInput("min_streak_days", "Min Max Up Streak", min = 5, max = 30, value = 8),
                           sliderInput("min_strong_days", "Min Strong Up Days", min = 10, max = 100, value = 50),
                           selectInput("trend_filter", "Trend Direction", 
                                       choices = c("All", "Strong Uptrend", "Uptrend"), selected = "All")
                    )
                  ),
                  div(style = "text-align: center; margin: 20px;",
                      actionButton("apply_advanced_filters", "Find Best Stocks", 
                                   class = "btn-success btn-lg", 
                                   style = "padding: 10px 30px; font-size: 16px;")
                  ),
                  verbatimTextOutput("filter_summary", placeholder = TRUE)
                )
              ),
              fluidRow(
                box(
                  title = "Advanced Filtered Results - Elite Performing Stocks", status = "success", solidHeader = TRUE,
                  width = 12, height = 600,
                  withSpinner(DT::dataTableOutput("advanced_filtered_table"))
                )
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Correlation Matrix - Key Performance Metrics", status = "primary", solidHeader = TRUE,
                  width = 12, height = 600,
                  withSpinner(plotOutput("correlation_plot"))
                )
              ),
              fluidRow(
                box(
                  title = "Complete Trading Data Explorer", status = "info", solidHeader = TRUE,
                  width = 12, height = 600,
                  withSpinner(DT::dataTableOutput("data_table"))
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Helper function for empty plotly plots
  plotly_empty <- function() {
    plot_ly() %>% layout(
      title = list(text = "No data available", x = 0.5),
      xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    )
  }
  
  # Reactive filtered data for screener
  filtered_data <- eventReactive(input$apply_filters, {
    req(nrow(trading_data) > 0)
    
    data <- trading_data
    
    # Apply filters
    if (input$ma_trend != "All") {
      data <- data %>% filter(ma_alignment == input$ma_trend)
    }
    
    data <- data %>%
      filter(
        roc_ytd >= input$ytd_range[1] & roc_ytd <= input$ytd_range[2],
        volume >= input$volume_range[1] & volume <= input$volume_range[2],
        performance_score >= input$score_range[1] & performance_score <= input$score_range[2],
        up_day_percentage >= input$min_up_percentage,
        max_up_streak >= input$min_streak
      ) %>%
      arrange(desc(performance_score)) %>%
      head(input$top_n)
    
    return(data)
  }, ignoreNULL = FALSE)
  
  # Initialize with default data
  filtered_data_init <- reactive({
    trading_data %>% 
      arrange(desc(performance_score)) %>% 
      head(50)
  })
  
  # Reactive filtered data for top performers tab
  top_performers_filtered <- eventReactive(input$apply_top_filters, {
    req(nrow(trading_data) > 0)
    
    data <- trading_data
    
    # Apply price and volume filters
    data <- data %>%
      filter(
        close >= input$price_range_top[1] & close <= input$price_range_top[2],
        volume >= input$volume_range_top[1] & volume <= input$volume_range_top[2],
        performance_score >= input$min_perf_score_top
      )
    
    # Apply performance category filter
    if (input$perf_category_filter != "All") {
      data <- data %>% filter(performance_category == input$perf_category_filter)
    }
    
    # Return top N performers
    data %>%
      arrange(desc(performance_score)) %>%
      head(input$top_n_performers)
  }, ignoreNULL = FALSE)
  
  # Initialize top performers
  top_performers_init <- reactive({
    trading_data %>% 
      arrange(desc(performance_score)) %>% 
      head(50)
  })
  
  # Advanced filtered data
  advanced_filtered_data <- eventReactive(input$apply_advanced_filters, {
    req(nrow(trading_data) > 0)
    
    data <- trading_data %>%
      filter(
        roc_ytd >= input$min_ytd,
        roc5 >= input$min_momentum,
        recent_up_trend_21d >= input$min_trend_21d,
        volume >= input$min_volume,
        total_up_days_252d >= input$min_up_days,
        up_day_percentage >= input$min_consistency,
        max_up_streak >= input$min_streak_days,
        strong_up_days >= input$min_strong_days
      )
    
    if (input$trend_filter != "All") {
      data <- data %>% filter(ma_alignment == input$trend_filter)
    }
    
    data %>%
      arrange(desc(performance_score)) %>%
      head(100)
  })
  
  # Value boxes for screener
  output$filtered_count <- renderValueBox({
    count <- if(input$apply_filters == 0) {
      nrow(filtered_data_init())
    } else {
      nrow(filtered_data())
    }
    valueBox(value = count, subtitle = "Filtered Stocks", icon = icon("filter"), color = "blue")
  })
  
  output$avg_performance <- renderValueBox({
    avg_perf <- if(input$apply_filters == 0) {
      data <- filtered_data_init()
      round(mean(data$performance_score, na.rm = TRUE), 1)
    } else {
      data <- filtered_data()
      if(nrow(data) > 0) round(mean(data$performance_score, na.rm = TRUE), 1) else 0
    }
    valueBox(value = avg_perf, subtitle = "Avg Performance Score", icon = icon("chart-line"), color = "green")
  })
  
  output$top_performer <- renderValueBox({
    top_stock <- if(input$apply_filters == 0) {
      data <- filtered_data_init()
      if(nrow(data) > 0) data$symbol[1] else "None"
    } else {
      data <- filtered_data()
      if(nrow(data) > 0) data$symbol[1] else "None"
    }
    valueBox(value = top_stock, subtitle = "Top Performer", icon = icon("trophy"), color = "yellow")
  })
  
  # Value boxes for top performers
  output$total_stocks <- renderValueBox({
    valueBox(value = format(nrow(trading_data), big.mark = ","), subtitle = "Total Stocks", icon = icon("list"), color = "blue")
  })
  
  output$filtered_top_count <- renderValueBox({
    count <- if(input$apply_top_filters == 0) {
      nrow(top_performers_init())
    } else {
      nrow(top_performers_filtered())
    }
    valueBox(value = count, subtitle = "Filtered Top Stocks", icon = icon("filter"), color = "green")
  })
  
  output$avg_filtered_score <- renderValueBox({
    avg_score <- if(input$apply_top_filters == 0) {
      data <- top_performers_init()
      round(mean(data$performance_score, na.rm = TRUE), 1)
    } else {
      data <- top_performers_filtered()
      if(nrow(data) > 0) round(mean(data$performance_score, na.rm = TRUE), 1) else 0
    }
    valueBox(value = avg_score, subtitle = "Avg Filtered Score", icon = icon("calculator"), color = "purple")
  })
  
  # Screener table
  output$screener_table <- DT::renderDataTable({
    data_to_show <- if(input$apply_filters == 0) {
      filtered_data_init()
    } else {
      filtered_data()
    }
    
    if(nrow(data_to_show) == 0) {
      return(DT::datatable(data.frame(Message = "No stocks match the selected criteria")))
    }
    
    data_to_show %>%
      select(symbol, performance_score, roc_ytd, close, volume, up_day_percentage, 
             max_up_streak, ma_alignment, performance_category) %>%
      mutate(
        performance_score = round(performance_score, 1),
        roc_ytd = round(roc_ytd, 2),
        close = round(close, 2),
        up_day_percentage = round(up_day_percentage, 1),
        volume = format(volume, big.mark = ",")
      ) %>%
      DT::datatable(
        options = list(pageLength = 25, scrollX = TRUE, order = list(list(1, 'desc'))),
        colnames = c("Symbol", "Performance Score", "YTD Return (%)", "Price ($)", "Volume", 
                     "Up Day %", "Max Up Streak", "MA Trend", "Category")
      ) %>%
      DT::formatStyle("performance_score", backgroundColor = DT::styleInterval(c(50, 70, 80), c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
  # Top performers chart
  output$top_performers_chart <- renderPlotly({
    data_to_plot <- if(input$apply_top_filters == 0) {
      top_performers_init()
    } else {
      top_performers_filtered()
    }
    
    if(nrow(data_to_plot) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(data_to_plot, aes(x = reorder(symbol, performance_score), y = performance_score, fill = performance_category)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      labs(title = paste("Top", nrow(data_to_plot), "Performance Scores"), x = "Symbol", y = "Performance Score") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 6)) +
      scale_fill_brewer(palette = "Spectral")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Performance score distribution
  output$score_distribution <- renderPlotly({
    data_to_plot <- if(input$apply_top_filters == 0) {
      trading_data %>% arrange(desc(performance_score)) %>% head(1000)
    } else {
      data <- trading_data %>%
        filter(
          close >= input$price_range_top[1] & close <= input$price_range_top[2],
          volume >= input$volume_range_top[1] & volume <= input$volume_range_top[2],
          performance_score >= input$min_perf_score_top
        )
      
      if (input$perf_category_filter != "All") {
        data <- data %>% filter(performance_category == input$perf_category_filter)
      }
      data
    }
    
    if(nrow(data_to_plot) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(data_to_plot, aes(x = performance_score)) +
      geom_histogram(bins = 30, fill = "#4CAF50", alpha = 0.7, color = "white") +
      geom_vline(xintercept = mean(data_to_plot$performance_score, na.rm = TRUE), linetype = "dashed", color = "red", size = 1) +
      labs(title = "Performance Score Distribution (Filtered)", x = "Performance Score", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Price vs Performance scatter
  output$price_performance_scatter <- renderPlotly({
    data_to_plot <- if(input$apply_top_filters == 0) {
      trading_data %>% arrange(desc(performance_score)) %>% head(500)
    } else {
      data <- trading_data %>%
        filter(
          close >= input$price_range_top[1] & close <= input$price_range_top[2],
          volume >= input$volume_range_top[1] & volume <= input$volume_range_top[2],
          performance_score >= input$min_perf_score_top
        )
      if (input$perf_category_filter != "All") {
        data <- data %>% filter(performance_category == input$perf_category_filter)
      }
      head(data, 500)
    }
    
    if (nrow(data_to_plot) == 0) return(plotly_empty())
    
    p <- ggplot(data_to_plot, aes(x = close, y = performance_score, color = performance_category, 
                                  size = volume, text = paste("Symbol:", symbol,
                                                              "<br>Price:", round(close, 2),
                                                              "<br>Performance Score:", round(performance_score, 1),
                                                              "<br>Volume:", format(volume, big.mark = ",")))) +
      geom_point(alpha = 0.6) +
      scale_color_brewer(palette = "Set2") +
      scale_size_continuous(range = c(1, 8), guide = "none") +
      labs(title = "Price vs Performance Score", x = "Price ($)", y = "Performance Score", color = "Category") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Volume vs Performance scatter  
  output$volume_performance_scatter <- renderPlotly({
    data_to_plot <- if(input$apply_top_filters == 0) {
      trading_data %>% arrange(desc(performance_score)) %>% head(500)
    } else {
      data <- trading_data %>%
        filter(
          close >= input$price_range_top[1] & close <= input$price_range_top[2],
          volume >= input$volume_range_top[1] & volume <= input$volume_range_top[2],
          performance_score >= input$min_perf_score_top
        )
      if (input$perf_category_filter != "All") {
        data <- data %>% filter(performance_category == input$perf_category_filter)
      }
      head(data, 500)
    }
    
    if (nrow(data_to_plot) == 0) return(plotly_empty())
    
    p <- ggplot(data_to_plot, aes(x = log10(volume), y = performance_score, color = close, 
                                  size = roc_ytd, text = paste("Symbol:", symbol,
                                                               "<br>Volume:", format(volume, big.mark = ","),
                                                               "<br>YTD Return:", round(roc_ytd, 2), "%",
                                                               "<br>Performance Score:", round(performance_score, 1)))) +
      geom_point(alpha = 0.6) +
      scale_color_viridis_c() +
      scale_size_continuous(range = c(1, 8), guide = "none") +
      labs(title = "Log10(Volume) vs Performance Score", x = "Log10(Volume)", y = "Performance Score", color = "Price ($)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Top performers table
  output$top_performers_table <- DT::renderDataTable({
    data_to_display <- if(input$apply_top_filters == 0) {
      top_performers_init()
    } else {
      top_performers_filtered()
    }
    
    if(nrow(data_to_display) == 0) {
      return(DT::datatable(data.frame(Message = "No data to display")))
    }
    
    DT::datatable(
      data_to_display %>%
        select(symbol, performance_score, risk_adjusted_score, roc_ytd, roc5, roc10,
               volume, close, ma_alignment) %>%
        mutate(
          performance_score = round(performance_score, 2),
          risk_adjusted_score = round(risk_adjusted_score, 2),
          roc_ytd = round(roc_ytd, 2),
          roc5 = round(roc5, 3),
          roc10 = round(roc10, 3),
          close = round(close, 2),
          volume = format(volume, big.mark = ",")
        ),
      options = list(pageLength = 25, scrollX = TRUE),
      colnames = c("Symbol", "Performance Score", "Risk-Adj Score", "YTD Return (%)", 
                   "5d ROC", "10d ROC", "Volume", "Price ($)", "MA Trend")
    )
  })
  
  # Returns heatmap
  output$returns_heatmap <- renderPlotly({
    top_100 <- trading_data %>% 
      filter(!is.na(performance_score), !is.na(roc_ytd)) %>%
      arrange(desc(performance_score)) %>% 
      head(100)
    
    if(nrow(top_100) == 0) return(plotly_empty())
    
    # Create a matrix for heatmap
    heatmap_matrix <- matrix(top_100$roc_ytd, nrow = 10, byrow = TRUE)
    heatmap_text <- matrix(paste(top_100$symbol, "<br>YTD:", round(top_100$roc_ytd, 2), "%"), nrow = 10, byrow = TRUE)
    
    plot_ly(x = 1:10, y = 1:10, z = heatmap_matrix, type = "heatmap",
            text = heatmap_text, hoverinfo = "text",
            colorscale = list(c(0, "red"), c(0.5, "yellow"), c(1, "green"))) %>%
      layout(title = "YTD Returns Heatmap - Top 100 Stocks",
             xaxis = list(title = "", showticklabels = FALSE), 
             yaxis = list(title = "", showticklabels = FALSE))
  })
  
  # Momentum analysis
  output$momentum_analysis <- renderPlotly({
    momentum_data <- trading_data %>%
      filter(!is.na(roc5), !is.na(roc10), !is.na(roc_ytd)) %>%
      arrange(desc(performance_score)) %>%
      head(200) %>%
      select(symbol, roc5, roc10, roc_ytd) %>%
      pivot_longer(cols = starts_with("roc"), names_to = "ROC_Period", values_to = "ROC_Value")
    
    if(nrow(momentum_data) == 0) return(plotly_empty())
    
    p <- ggplot(momentum_data, aes(x = symbol, y = ROC_Value, fill = ROC_Period,
                                   text = paste("Symbol:", symbol,
                                                "<br>Period:", ROC_Period,
                                                "<br>Value:", round(ROC_Value, 3)))) +
      geom_col(position = "dodge", alpha = 0.8) +
      labs(title = "Rate of Change (Momentum) Indicators - Top 200", x = "Symbol", y = "ROC Value") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Trend analysis
  output$trend_analysis <- renderPlotly({
    trend_data <- trading_data %>%
      filter(!is.na(recent_up_trend_21d), !is.na(recent_up_trend_63d)) %>%
      arrange(desc(performance_score)) %>%
      head(100)
    
    if(nrow(trend_data) == 0) return(plotly_empty())
    
    p <- ggplot(trend_data, aes(x = recent_up_trend_21d, y = recent_up_trend_63d, 
                                color = performance_score, size = volume,
                                text = paste("Symbol:", symbol,
                                             "<br>21d Trend:", recent_up_trend_21d,
                                             "<br>63d Trend:", recent_up_trend_63d,
                                             "<br>Performance Score:", round(performance_score, 1)))) +
      geom_point(alpha = 0.7) +
      scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 50) +
      scale_size_continuous(range = c(2, 10), guide = "none") +
      labs(title = "Trend Strength Analysis - 21d vs 63d", 
           x = "Recent Up Trend 21d", y = "Recent Up Trend 63d", color = "Performance Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # MA alignment chart
  output$ma_alignment_chart <- renderPlotly({
    ma_data <- trading_data %>%
      filter(!is.na(close), !is.na(ma_4d), !is.na(ma_7d), !is.na(ma_21d), !is.na(ma_42d)) %>%
      arrange(desc(performance_score)) %>%
      head(50) %>%
      select(symbol, close, ma_4d, ma_7d, ma_21d, ma_42d, performance_score) %>%
      pivot_longer(cols = c(close, ma_4d, ma_7d, ma_21d, ma_42d), names_to = "MA_Type", values_to = "Price")
    
    if(nrow(ma_data) == 0) return(plotly_empty())
    
    p <- ggplot(ma_data, aes(x = MA_Type, y = Price, color = symbol, group = symbol,
                             text = paste("Symbol:", symbol,
                                          "<br>Type:", MA_Type,
                                          "<br>Price:", round(Price, 2),
                                          "<br>Performance Score:", round(performance_score, 1)))) +
      geom_line(alpha = 0.6, size = 1) +
      geom_point(alpha = 0.8, size = 2) +
      labs(title = "Moving Average Alignment - Top 50 Stocks",
           x = "MA Type", y = "Price") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Volume performance  
  output$volume_performance <- renderPlotly({
    vol_perf_data <- trading_data %>%
      filter(!is.na(volume), !is.na(performance_score), !is.na(roc_ytd)) %>%
      arrange(desc(performance_score)) %>%
      head(500)
    
    if(nrow(vol_perf_data) == 0) return(plotly_empty())
    
    p <- ggplot(vol_perf_data, aes(x = log10(volume), y = roc_ytd, color = performance_score, 
                                   size = up_day_percentage,
                                   text = paste("Symbol:", symbol,
                                                "<br>Volume:", format(volume, big.mark = ","),
                                                "<br>YTD Return:", round(roc_ytd, 2), "%",
                                                "<br>Performance Score:", round(performance_score, 1),
                                                "<br>Up Day %:", round(up_day_percentage, 1)))) +
      geom_point(alpha = 0.6) +
      scale_color_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
      scale_size_continuous(range = c(1, 8), guide = "none") +
      labs(title = "Volume vs YTD Performance", x = "Log10(Volume)", y = "YTD Return (%)", 
           color = "Performance Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Consecutive days analysis
  output$consecutive_days <- renderPlotly({
    consec_data <- trading_data %>%
      filter(!is.na(consecutive_up_days), !is.na(max_up_streak)) %>%
      arrange(desc(performance_score)) %>%
      head(200)
    
    if(nrow(consec_data) == 0) return(plotly_empty())
    
    p <- ggplot(consec_data, aes(x = consecutive_up_days, y = max_up_streak, 
                                 color = performance_score, size = volume,
                                 text = paste("Symbol:", symbol,
                                              "<br>Consecutive Up Days:", consecutive_up_days,
                                              "<br>Max Up Streak:", max_up_streak,
                                              "<br>Performance Score:", round(performance_score, 1)))) +
      geom_point(alpha = 0.7) +
      scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 50) +
      scale_size_continuous(range = c(1, 8), guide = "none") +
      labs(title = "Consecutive Up Days vs Max Up Streak", 
           x = "Current Consecutive Up Days", y = "Max Up Streak", color = "Performance Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Risk-return efficient frontier
  output$efficient_frontier <- renderPlotly({
    risk_data <- trading_data %>%
      filter(!is.na(risk_adjusted_score), !is.na(roc_ytd), !is.na(volatility_proxy)) %>%
      head(500)
    
    if(nrow(risk_data) == 0) return(plotly_empty())
    
    p <- ggplot(risk_data, aes(x = volatility_proxy, y = roc_ytd, color = performance_score,
                               size = performance_score, text = paste("Symbol:", symbol,
                                                                      "<br>YTD Return:", round(roc_ytd, 2), "%",
                                                                      "<br>Volatility:", round(volatility_proxy, 3)))) +
      geom_point(alpha = 0.6) +
      scale_color_viridis_c(option = "plasma") +
      scale_size_continuous(range = c(1, 8), guide = "none") +
      labs(title = "Risk-Return Efficient Frontier",
           x = "Volatility Proxy", y = "YTD Return (%)", color = "Performance Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Volatility analysis
  output$volatility_analysis <- renderPlotly({
    vol_data <- trading_data %>%
      filter(!is.na(roc5), !is.na(roc10)) %>%
      arrange(desc(performance_score)) %>%
      head(500)
    
    if(nrow(vol_data) == 0) return(plotly_empty())
    
    vol_data <- vol_data %>%
      mutate(roc_diff = abs(roc5 - roc10))
    
    p <- ggplot(vol_data, aes(x = roc_diff, y = performance_score, color = performance_category,
                              text = paste("Symbol:", symbol,
                                           "<br>ROC Diff:", round(roc_diff, 2),
                                           "<br>Performance Score:", round(performance_score, 1)))) +
      geom_point(alpha = 0.7) +
      scale_color_brewer(palette = "Spectral") +
      labs(title = "Volatility (ROC Difference) vs Performance",
           x = "Absolute Difference between 5d and 10d ROC", y = "Performance Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Risk-adjusted table
  output$risk_adjusted_table <- DT::renderDataTable({
    risk_table_data <- trading_data %>%
      filter(!is.na(risk_adjusted_score)) %>%
      arrange(desc(risk_adjusted_score)) %>%
      head(100) %>%
      select(symbol, performance_score, risk_adjusted_score, roc_ytd, volatility_proxy, ma_alignment) %>%
      mutate(
        performance_score = round(performance_score, 1),
        risk_adjusted_score = round(risk_adjusted_score, 1),
        roc_ytd = round(roc_ytd, 2),
        volatility_proxy = round(volatility_proxy, 3)
      )
    
    if(nrow(risk_table_data) == 0) return(DT::datatable(data.frame(Message = "No data available")))
    
    DT::datatable(
      risk_table_data,
      options = list(pageLength = 25, scrollX = TRUE),
      colnames = c("Symbol", "Performance Score", "Risk-Adj Score", "YTD Return (%)", 
                   "Volatility Proxy", "MA Trend")
    ) %>%
      DT::formatStyle("risk_adjusted_score", backgroundColor = DT::styleInterval(c(40, 60, 80), c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
  # Sector performance
  output$sector_performance <- renderPlotly({
    sector_perf <- trading_data %>%
      group_by(sector) %>%
      summarise(
        avg_score = mean(performance_score, na.rm = TRUE),
        median_ytd = median(roc_ytd, na.rm = TRUE),
        n_stocks = n(),
        .groups = "drop"
      ) %>%
      filter(n_stocks > 10) %>%
      arrange(desc(avg_score))
    
    if(nrow(sector_perf) == 0) return(plotly_empty())
    
    p <- ggplot(sector_perf, aes(x = reorder(sector, avg_score), y = avg_score, fill = sector,
                                 text = paste("Sector:", sector,
                                              "<br>Avg Score:", round(avg_score, 1),
                                              "<br>Median YTD:", round(median_ytd, 2), "%",
                                              "<br>Stocks:", n_stocks))) +
      geom_col(alpha = 0.8) +
      labs(title = "Average Performance Score by Sector",
           x = "Sector", y = "Average Performance Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Sector risk-return
  output$sector_risk_return <- renderPlotly({
    sector_risk_return <- trading_data %>%
      group_by(sector) %>%
      summarise(
        avg_ytd = mean(roc_ytd, na.rm = TRUE),
        avg_volatility = mean(volatility_proxy, na.rm = TRUE),
        n_stocks = n(),
        .groups = "drop"
      ) %>%
      filter(n_stocks > 10)
    
    if(nrow(sector_risk_return) == 0) return(plotly_empty())
    
    p <- ggplot(sector_risk_return, aes(x = avg_volatility, y = avg_ytd, color = sector,
                                        size = n_stocks, text = paste("Sector:", sector,
                                                                      "<br>Avg YTD:", round(avg_ytd, 2), "%",
                                                                      "<br>Avg Volatility:", round(avg_volatility, 3),
                                                                      "<br>Stocks:", n_stocks))) +
      geom_point(alpha = 0.8) +
      labs(title = "Sector Risk-Return Profile",
           x = "Average Volatility Proxy", y = "Average YTD Return (%)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Sector best stocks
  output$sector_best <- DT::renderDataTable({
    best_by_sector <- trading_data %>%
      group_by(sector) %>%
      slice_max(performance_score, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(sector, symbol, performance_score, roc_ytd, close, volume) %>%
      arrange(desc(performance_score)) %>%
      mutate(
        performance_score = round(performance_score, 1),
        roc_ytd = round(roc_ytd, 2),
        close = round(close, 2),
        volume = format(volume, big.mark = ",")
      )
    
    if(nrow(best_by_sector) == 0) return(DT::datatable(data.frame(Message = "No data available")))
    
    DT::datatable(
      best_by_sector,
      options = list(pageLength = 25, scrollX = TRUE),
      colnames = c("Sector", "Symbol", "Performance Score", "YTD Return (%)", "Price ($)", "Volume")
    ) %>%
      DT::formatStyle("performance_score", backgroundColor = DT::styleInterval(c(50, 70, 80), c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
  # Advanced filtered table
  output$advanced_filtered_table <- DT::renderDataTable({
    if(input$apply_advanced_filters == 0) {
      return(DT::datatable(data.frame(Message = "Click 'Find Best Stocks' to see results")))
    }
    
    data <- advanced_filtered_data()
    if(nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No stocks match all criteria")))
    }
    
    data %>%
      select(symbol, performance_score, risk_adjusted_score, roc_ytd, roc5, 
             volume, close, ma_alignment, performance_category) %>%
      mutate(
        performance_score = round(performance_score, 1),
        risk_adjusted_score = round(risk_adjusted_score, 1),
        roc_ytd = round(roc_ytd, 2),
        roc5 = round(roc5, 3),
        close = round(close, 2),
        volume = format(volume, big.mark = ",")
      ) %>%
      DT::datatable(
        options = list(pageLength = 25, scrollX = TRUE, order = list(list(0, 'desc'))),
        colnames = c("Symbol", "Performance Score", "Risk-Adj Score", "YTD Return (%)", 
                     "5d ROC", "Volume", "Price ($)", "MA Trend", "Category")
      ) %>%
      DT::formatStyle("performance_score", backgroundColor = DT::styleInterval(c(60, 75, 85), c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
  # Filter summary
  output$filter_summary <- renderText({
    if(input$apply_advanced_filters == 0) {
      "Configure filters above and click 'Find Best Stocks' to see matching criteria summary."
    } else {
      data <- advanced_filtered_data()
      paste0("Found ", nrow(data), " stocks matching all criteria:\n",
             "• YTD Return ≥ ", input$min_ytd, "%\n",
             "• 5-day ROC ≥ ", input$min_momentum, "\n",
             "• Recent 21d Trend ≥ ", input$min_trend_21d, " days\n",
             "• Volume ≥ ", format(input$min_volume, big.mark = ","), "\n",
             "• Up Days (252d) ≥ ", input$min_up_days, "\n",
             "• Up Day % ≥ ", input$min_consistency, "%\n",
             "• Max Up Streak ≥ ", input$min_streak_days, " days\n",
             "• Strong Up Days ≥ ", input$min_strong_days, "\n",
             "• Trend Filter: ", input$trend_filter)
    }
  })
  
  # Correlation plot
  output$correlation_plot <- renderPlot({
    cor_data <- trading_data %>%
      select(performance_score, roc_ytd, roc5, roc10, volume, up_day_percentage, 
             max_up_streak, recent_up_trend_21d, recent_up_trend_63d) %>%
      mutate(volume = log10(volume)) %>%
      na.omit()
    
    if(nrow(cor_data) == 0) return(NULL)
    
    cor_matrix <- cor(cor_data, use = "complete.obs")
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             addCoef.col = "black", number.cex = 0.7)
  })
  
  # Data explorer table
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      trading_data %>% 
        select(symbol, date, close, volume, roc_ytd, performance_score, 
               ma_alignment, performance_category) %>%
        mutate(
          close = round(close, 2),
          roc_ytd = round(roc_ytd, 2),
          performance_score = round(performance_score, 1),
          volume = format(volume, big.mark = ",")
        ),
      options = list(pageLength = 25, scrollX = TRUE, order = list(list(5, 'desc'))),
      filter = 'top'
    )
  })
}

# Run the app

shinyApp(ui, server)
