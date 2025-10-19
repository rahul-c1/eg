# Enhanced Large Scale Trading Analysis Shiny App
# Version: 3.0 - Professional Edition
# Features: Date Range, Comparison Mode, Responsive Design, Export, Interactive Filtering

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
library(viridis)

# Load data with error handling
tryCatch({
  trading_data <- data.table::fread("latest_metrics.csv")
}, error = function(e) {
  set.seed(123)
  n_stocks <- 1000
  dates <- seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")
  trading_data <- data.table()
  
  for(i in 1:n_stocks) {
    stock_dates <- sample(dates, size = min(250, length(dates)), replace = FALSE)
    stock_dates <- sort(stock_dates)
    n_days <- length(stock_dates)
    
    temp_data <- data.table(
      symbol = paste0("STOCK", sprintf("%03d", i)),
      date = stock_dates,
      close = cumsum(rnorm(n_days, 0.5, 2)) + runif(1, 10, 200),
      volume = sample(100000:10000000, n_days, replace = TRUE),
      roc_ytd = rnorm(n_days, 10, 30),
      roc5 = runif(n_days, 0.95, 1.05),
      roc10 = runif(n_days, 0.90, 1.10),
      up_day_percentage = runif(n_days, 30, 70),
      max_up_streak = sample(1:30, n_days, replace = TRUE),
      recent_up_trend_21d = sample(5:21, n_days, replace = TRUE),
      recent_up_trend_63d = sample(15:63, n_days, replace = TRUE),
      ma_4d = NA_real_,
      ma_7d = NA_real_,
      ma_21d = NA_real_,
      ma_42d = NA_real_,
      total_up_days_252d = sample(80:200, n_days, replace = TRUE),
      consecutive_up_days = sample(0:10, n_days, replace = TRUE),
      strong_up_days = sample(10:100, n_days, replace = TRUE),
      first_date = rep(as.Date("2024-01-01"), n_days)
    )
    trading_data <- rbind(trading_data, temp_data)
  }
  cat("Using sample data with historical dates\n")
})

# Filter for valid stocks
symbol_vol_gt_100k <- trading_data %>% 
  group_by(symbol) %>% 
  filter(any(close > 5, na.rm = TRUE)) %>% 
  filter(any(volume > 100000, na.rm = TRUE)) %>% 
  pull(symbol) %>% 
  unique()

trading_data <- trading_data %>% 
  filter(symbol %in% symbol_vol_gt_100k) %>% 
  setDT()

# Convert date columns
if("date" %in% names(trading_data)) {
  trading_data$date <- as.Date(trading_data$date)
}
if("first_date" %in% names(trading_data)) {
  trading_data$first_date <- as.Date(trading_data$first_date)
}

# Calculate performance scores
calculate_performance_score <- function(data) {
  data %>%
    mutate(
      across(c(roc_ytd, roc5, roc10, close, volume, up_day_percentage, 
               max_up_streak, recent_up_trend_21d, recent_up_trend_63d,
               ma_4d, ma_7d, ma_21d, ma_42d), as.numeric),
      
      roc_ytd = coalesce(roc_ytd, 0),
      roc5 = coalesce(roc5, 1),
      roc10 = coalesce(roc10, 1),
      up_day_percentage = coalesce(up_day_percentage, 50),
      max_up_streak = coalesce(max_up_streak, 5),
      recent_up_trend_21d = coalesce(recent_up_trend_21d, 10),
      recent_up_trend_63d = coalesce(recent_up_trend_63d, 30),
      
      roc_ytd_score = pmax(0, pmin(100, (roc_ytd + 50) * 1)),
      momentum_score = pmax(0, pmin(100, ((roc5 - 0.95) * 1000 + (roc10 - 0.90) * 500) / 15)),
      trend_score = pmax(0, (recent_up_trend_21d / 21 + recent_up_trend_63d / 63) * 50),
      consistency_score = pmax(0, pmin(100, up_day_percentage)),
      volume_score = pmax(0, pmin(100, log10(pmax(1000, volume)) * 10)),
      streak_score = pmax(0, pmin(100, max_up_streak * 4)),
      
      performance_score = (
        roc_ytd_score * 0.3 +
          momentum_score * 0.2 +
          trend_score * 0.2 +
          consistency_score * 0.15 +
          volume_score * 0.1 +
          streak_score * 0.05
      ),
      
      volatility_proxy = abs(roc5 - 1) + abs(roc10 - 1),
      risk_adjusted_score = performance_score / (1 + volatility_proxy * 10),
      
      performance_category = case_when(
        performance_score >= 80 ~ "Excellent",
        performance_score >= 65 ~ "Good",
        performance_score >= 50 ~ "Average",
        performance_score >= 35 ~ "Below Average",
        TRUE ~ "Poor"
      ),
      
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

trading_data <- calculate_performance_score(trading_data)

# Add sectors if not present
if(!"sector" %in% names(trading_data)) {
  set.seed(123)
  symbols <- unique(trading_data$symbol)
  sector_map <- data.table(
    symbol = symbols,
    sector = sample(
      c("Technology", "Healthcare", "Finance", "Energy", "Consumer", "Industrial", "Materials", "Utilities"), 
      length(symbols), replace = TRUE, 
      prob = c(0.2, 0.15, 0.15, 0.1, 0.15, 0.1, 0.1, 0.05)
    )
  )
  trading_data <- merge(trading_data, sector_map, by = "symbol", all.x = TRUE)
}

# Add missing columns
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

# Get date range for the app
min_date <- min(trading_data$date, na.rm = TRUE)
max_date <- max(trading_data$date, na.rm = TRUE)

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = span(icon("chart-line"), "Trading Analytics Pro", 
                 style = "font-weight: 600; font-size: 18px;"),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), 
               badgeLabel = "Start", badgeColor = "olive"),
      menuItem("Screening", icon = icon("filter"), startExpanded = FALSE,
               menuSubItem("Stock Screener", tabName = "screener"),
               menuSubItem("Advanced Filters", tabName = "filters")
      ),
      menuItem("Analysis", icon = icon("chart-bar"), startExpanded = FALSE,
               menuSubItem("Top Performers", tabName = "topperformers"),
               menuSubItem("Performance", tabName = "performance"),
               menuSubItem("Technical", tabName = "technical")
      ),
      menuItem("Risk & Sectors", icon = icon("shield-alt"), startExpanded = FALSE,
               menuSubItem("Risk Analysis", tabName = "risk"),
               menuSubItem("Sector Analysis", tabName = "sector")
      ),
      menuItem("Stock Comparison", tabName = "comparison", icon = icon("balance-scale"),
               badgeLabel = "New", badgeColor = "light-blue"),
      menuItem("Data Explorer", tabName = "data", icon = icon("database")),
      
      # Global date range filter
      hr(),
      div(style = "padding: 15px;",
          h4("Global Filters", style = "color: #fff; margin-top: 0;"),
          dateRangeInput(
            "date_range",
            "Date Range:",
            start = max_date - 90,
            end = max_date,
            min = min_date,
            max = max_date,
            format = "yyyy-mm-dd"
          ),
          actionButton("apply_date_filter", "Apply Date Filter", 
                       class = "btn-primary btn-block", icon = icon("calendar-check")),
          br(),
          actionButton("reset_date_filter", "Reset to Latest", 
                       class = "btn-default btn-block btn-sm", icon = icon("undo"))
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Responsive and Modern Styling */
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
        
        body {
          font-family: 'Inter', 'Segoe UI', sans-serif;
          font-size: 14px;
        }
        
        /* Responsive adjustments */
        @media (max-width: 768px) {
          .content-wrapper {
            padding: 10px !important;
          }
          .box {
            margin-bottom: 10px;
          }
          .small-box {
            margin-bottom: 10px;
          }
          .sidebar-menu {
            font-size: 13px;
          }
          h3, h4 {
            font-size: 18px;
          }
        }
        
        @media (max-width: 576px) {
          .main-header .logo {
            width: 200px;
          }
          .main-header .navbar {
            margin-left: 200px;
          }
        }
        
        /* Modern color scheme */
        :root {
          --primary-color: #2C3E50;
          --success-color: #27AE60;
          --danger-color: #E74C3C;
          --warning-color: #F39C12;
          --info-color: #3498DB;
        }
        
        .content-wrapper, .right-side { 
          background-color: #f4f6f9; 
        }
        
        /* Value boxes with hover effects */
        .small-box {
          margin-bottom: 15px;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          transition: all 0.3s ease;
        }
        
        .small-box:hover {
          transform: translateY(-4px);
          box-shadow: 0 6px 20px rgba(0,0,0,0.12);
        }
        
        /* Better box styling */
        .box {
          border-radius: 8px;
          box-shadow: 0 1px 4px rgba(0,0,0,0.08);
          border-top: 3px solid #3c8dbc;
        }
        
        .box.box-primary { border-top-color: #3c8dbc; }
        .box.box-success { border-top-color: #00a65a; }
        .box.box-warning { border-top-color: #f39c12; }
        .box.box-danger { border-top-color: #dd4b39; }
        .box.box-info { border-top-color: #00c0ef; }
        
        /* Enhanced buttons */
        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border: none;
          border-radius: 6px;
          padding: 10px 24px;
          font-weight: 600;
          transition: all 0.3s ease;
        }
        
        .btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
        }
        
        .btn-success {
          background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);
          border: none;
          border-radius: 6px;
        }
        
        /* Data table enhancements */
        .dataTables_wrapper {
          padding: 15px;
        }
        
        table.dataTable tbody tr:hover {
          background-color: #f1f5f9 !important;
        }
        
        table.dataTable thead th {
          background: linear-gradient(to bottom, #f8f9fa 0%, #e9ecef 100%);
          font-weight: 600;
          border-bottom: 2px solid #dee2e6;
        }
        
        /* Comparison cards */
        .comparison-card {
          background: white;
          border-radius: 8px;
          padding: 20px;
          margin: 10px 0;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          border-left: 4px solid #3c8dbc;
          transition: all 0.3s ease;
        }
        
        .comparison-card:hover {
          box-shadow: 0 4px 16px rgba(0,0,0,0.12);
          transform: translateX(4px);
        }
        
        .comparison-metric {
          display: flex;
          justify-content: space-between;
          padding: 8px 0;
          border-bottom: 1px solid #f0f0f0;
        }
        
        .metric-label {
          font-weight: 600;
          color: #555;
        }
        
        .metric-value {
          font-weight: 700;
          color: #2c3e50;
        }
        
        /* Stock search enhancement */
        .selectize-input {
          border-radius: 6px;
          border: 2px solid #e0e0e0;
          transition: border-color 0.3s ease;
        }
        
        .selectize-input:focus {
          border-color: #667eea;
          box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
        }
        
        /* Loading spinner */
        .shiny-spinner-output-container {
          display: flex;
          justify-content: center;
          align-items: center;
          min-height: 200px;
        }
        
        /* Export buttons styling */
        .dt-buttons {
          margin-bottom: 10px;
        }
        
        .dt-button {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
          color: white !important;
          border: none !important;
          border-radius: 4px !important;
          padding: 6px 12px !important;
          margin-right: 5px !important;
          font-weight: 600 !important;
        }
        
        .dt-button:hover {
          opacity: 0.9;
          transform: translateY(-1px);
        }
        
        /* Info badge */
        .info-badge {
          background: #e8f4fd;
          border-left: 4px solid #3498DB;
          padding: 12px;
          border-radius: 4px;
          margin: 10px 0;
        }
        
        /* Responsive table container */
        .table-responsive {
          overflow-x: auto;
          -webkit-overflow-scrolling: touch;
        }
      "))
    ),
    
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_stocks_dash", width = 3),
                valueBoxOutput("avg_performance_dash", width = 3),
                valueBoxOutput("top_sector_dash", width = 3),
                valueBoxOutput("date_range_info", width = 3)
              ),
              fluidRow(
                box(
                  title = "Performance Distribution", status = "primary", 
                  solidHeader = TRUE, width = 8, collapsible = TRUE,
                  plotlyOutput("dash_performance_dist", height = "300px") %>% withSpinner()
                ),
                box(
                  title = "Quick Stats", status = "info",
                  solidHeader = TRUE, width = 4, collapsible = TRUE,
                  tableOutput("dash_quick_stats")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Performers", status = "success",
                  solidHeader = TRUE, width = 6, collapsible = TRUE,
                  DT::dataTableOutput("dash_top10") %>% withSpinner()
                ),
                box(
                  title = "Sector Heatmap", status = "warning",
                  solidHeader = TRUE, width = 6, collapsible = TRUE,
                  plotlyOutput("dash_sector_heatmap", height = "300px") %>% withSpinner()
                )
              )
      ),
      
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
                    column(3,
                           numericInput("min_up_percentage", "Min Up Day %", value = 45, min = 0, max = 100)
                    ),
                    column(3,
                           numericInput("min_streak", "Min Up Streak", value = 5, min = 0, max = 50)
                    ),
                    column(3,
                           numericInput("top_n", "Show Top N Stocks", value = 50, min = 10, max = 500)
                    ),
                    column(3, br(),
                           actionButton("apply_filters", "Apply Filters", 
                                        class = "btn-primary", icon = icon("search"), 
                                        style = "margin-right: 5px;"),
                           actionButton("reset_filters", "Reset", 
                                        class = "btn-default", icon = icon("undo"))
                    )
                  )
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
                  width = 12, collapsible = TRUE,
                  div(class = "info-badge",
                      icon("info-circle"), " Click on sector names in the chart to filter the table. ",
                      "Use export buttons to download data."
                  ),
                  DT::dataTableOutput("screener_table") %>% withSpinner()
                )
              )
      ),
      
      # Stock Comparison Tab (NEW)
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Stock Comparison Tool", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  fluidRow(
                    column(8,
                           selectizeInput(
                             "compare_stocks",
                             "Select stocks to compare (up to 5):",
                             choices = NULL,
                             multiple = TRUE,
                             options = list(
                               maxItems = 5,
                               placeholder = 'Type to search stocks...',
                               plugins = list('remove_button')
                             )
                           )
                    ),
                    column(4, br(),
                           actionButton("run_comparison", "Compare Stocks", 
                                        class = "btn-success btn-block", 
                                        icon = icon("balance-scale"))
                    )
                  )
                )
              ),
              fluidRow(
                uiOutput("comparison_cards")
              ),
              fluidRow(
                box(
                  title = "Performance Comparison Chart", status = "primary",
                  solidHeader = TRUE, width = 6, collapsible = TRUE,
                  plotlyOutput("comparison_chart") %>% withSpinner()
                ),
                box(
                  title = "Metrics Radar Chart", status = "info",
                  solidHeader = TRUE, width = 6, collapsible = TRUE,
                  plotlyOutput("comparison_radar") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Historical Price Comparison", status = "success",
                  solidHeader = TRUE, width = 12, collapsible = TRUE,
                  plotlyOutput("comparison_historical", height = "400px") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Detailed Comparison Table", status = "warning",
                  solidHeader = TRUE, width = 12, collapsible = TRUE,
                  DT::dataTableOutput("comparison_table") %>% withSpinner()
                )
              )
      ),
      
      # Top Performers Tab
      tabItem(tabName = "topperformers",
              fluidRow(
                box(
                  title = "Top Performers Filters", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
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
                    column(6, br(),
                           actionButton("apply_top_filters", "Apply Filters", 
                                        class = "btn-primary", icon = icon("filter"))
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
                  width = 6, collapsible = TRUE,
                  plotlyOutput("top_performers_chart") %>% withSpinner()
                ),
                box(
                  title = "Filtered Performance Score Distribution", status = "success", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("score_distribution") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Price vs Performance (Filtered)", status = "warning", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("price_performance_scatter") %>% withSpinner()
                ),
                box(
                  title = "Volume vs Performance (Filtered)", status = "info", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("volume_performance_scatter") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Filtered Top Performers Table", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  DT::dataTableOutput("top_performers_table") %>% withSpinner()
                )
              )
      ),
      
      # Performance Analysis Tab
      tabItem(tabName = "performance",
              fluidRow(
                box(
                  title = "YTD Returns Heatmap (Top 100)", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  plotlyOutput("returns_heatmap", height = "500px") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Momentum Analysis (Top 200)", status = "success", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("momentum_analysis") %>% withSpinner()
                ),
                box(
                  title = "Trend Strength Analysis (Top 100)", status = "warning", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("trend_analysis") %>% withSpinner()
                )
              )
      ),
      
      # Technical Analysis Tab
      tabItem(tabName = "technical",
              fluidRow(
                box(
                  title = "Moving Average Alignment (Top 50)", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  plotlyOutput("ma_alignment_chart", height = "500px") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Volume vs YTD Performance (Top 500)", status = "success", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("volume_performance") %>% withSpinner()
                ),
                box(
                  title = "Consecutive Up Days Analysis (Top 200)", status = "info", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("consecutive_days") %>% withSpinner()
                )
              )
      ),
      
      # Risk Analysis Tab
      tabItem(tabName = "risk",
              fluidRow(
                box(
                  title = "Risk-Return Efficient Frontier (Top 500)", status = "primary", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("efficient_frontier", height = "500px") %>% withSpinner()
                ),
                box(
                  title = "Risk-Adjusted Performance Rankings", status = "warning", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("volatility_analysis", height = "500px") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Risk-Adjusted Top Performers", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  DT::dataTableOutput("risk_adjusted_table") %>% withSpinner()
                )
              )
      ),
      
      # Sector Analysis Tab
      tabItem(tabName = "sector",
              fluidRow(
                box(
                  title = "Sector Performance Comparison", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  plotlyOutput("sector_performance", height = "500px") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Sector Risk-Return Profile", status = "success", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  plotlyOutput("sector_risk_return") %>% withSpinner()
                ),
                box(
                  title = "Best Stock by Sector", status = "info", solidHeader = TRUE,
                  width = 6, collapsible = TRUE,
                  DT::dataTableOutput("sector_best") %>% withSpinner()
                )
              )
      ),
      
      # Advanced Filters Tab
      tabItem(tabName = "filters",
              fluidRow(
                box(
                  title = "Advanced Multi-Criteria Stock Finder", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
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
                                   icon = icon("search"),
                                   style = "padding: 12px 40px; font-size: 16px;")
                  ),
                  verbatimTextOutput("filter_summary", placeholder = TRUE)
                )
              ),
              fluidRow(
                box(
                  title = "Advanced Filtered Results - Elite Performing Stocks", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  DT::dataTableOutput("advanced_filtered_table") %>% withSpinner()
                )
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Correlation Matrix - Key Performance Metrics", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  plotOutput("correlation_plot", height = "600px") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Complete Trading Data Explorer", status = "info", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  DT::dataTableOutput("data_table") %>% withSpinner()
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values for storing state
  rv <- reactiveValues(
    filtered_data = NULL,
    clicked_sector = NULL,
    date_filtered_data = trading_data
  )
  
  # Update stock choices for comparison
  updateSelectizeInput(session, "compare_stocks", 
                       choices = sort(unique(trading_data$symbol)), 
                       server = TRUE)
  
  # Helper function for empty plotly plots
  plotly_empty <- function(message = "No data available") {
    plot_ly() %>% layout(
      title = list(text = message, x = 0.5, y = 0.5),
      xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      annotations = list(
        text = message,
        xref = "paper",
        yref = "paper",
        x = 0.5,
        y = 0.5,
        showarrow = FALSE,
        font = list(size = 20, color = "#999")
      )
    )
  }
  
  # Plotly theme function
  theme_trading_plotly <- function(p) {
    p %>% layout(
      font = list(family = "Inter, Segoe UI, sans-serif", size = 12),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#F8F9FA",
      hovermode = "closest",
      hoverlabel = list(
        bgcolor = "#2C3E50",
        font = list(color = "white", size = 13)
      ),
      margin = list(l = 60, r = 30, t = 50, b = 60)
    ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"))
  }
  
  # Date filter logic
  observeEvent(input$apply_date_filter, {
    rv$date_filtered_data <- trading_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    showNotification(
      paste("Data filtered to", nrow(rv$date_filtered_data), "records between",
            input$date_range[1], "and", input$date_range[2]),
      type = "message",
      duration = 3
    )
  })
  
  observeEvent(input$reset_date_filter, {
    updateDateRangeInput(session, "date_range",
                         start = max_date - 90,
                         end = max_date)
    rv$date_filtered_data <- trading_data
    showNotification("Date filter reset to latest 90 days", type = "message", duration = 3)
  })
  
  # Get current data based on date filter
  current_data <- reactive({
    rv$date_filtered_data %>%
      group_by(symbol) %>%
      filter(date == max(date)) %>%
      ungroup()
  })
  
  # Dashboard value boxes
  output$total_stocks_dash <- renderValueBox({
    valueBox(
      value = format(length(unique(current_data()$symbol)), big.mark = ","),
      subtitle = "Total Stocks",
      icon = icon("list"),
      color = "primary"
    )
  })
  
  output$avg_performance_dash <- renderValueBox({
    avg_perf <- round(mean(current_data()$performance_score, na.rm = TRUE), 1)
    valueBox(
      value = avg_perf,
      subtitle = "Avg Performance Score",
      icon = icon("chart-line"),
      color = if(avg_perf >= 60) "success" else if(avg_perf >= 40) "warning" else "danger"
    )
  })
  
  output$top_sector_dash <- renderValueBox({
    top_sector <- current_data() %>%
      group_by(sector) %>%
      summarise(avg_score = mean(performance_score, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_score)) %>%
      slice(1) %>%
      pull(sector)
    
    valueBox(
      value = top_sector,
      subtitle = "Top Sector",
      icon = icon("trophy"),
      color = "purple"
    )
  })
  
  output$date_range_info <- renderValueBox({
    valueBox(
      value = format(input$date_range[2], "%b %d"),
      subtitle = paste("As of", format(input$date_range[1], "%b %d")),
      icon = icon("calendar"),
      color = "teal"
    )
  })
  
  # Dashboard charts
  output$dash_performance_dist <- renderPlotly({
    data <- current_data()
    if(nrow(data) == 0) return(plotly_empty())
    
    p <- plot_ly(data, x = ~performance_score, type = "histogram",
                 marker = list(color = '#667eea',
                               line = list(color = '#764ba2', width = 1))) %>%
      layout(title = "",
             xaxis = list(title = "Performance Score"),
             yaxis = list(title = "Count"))
    
    theme_trading_plotly(p)
  })
  
  output$dash_quick_stats <- renderTable({
    data <- current_data()
    data.frame(
      Metric = c("Excellent Stocks", "Good Stocks", "Avg YTD Return", "Max Performance Score"),
      Value = c(
        sum(data$performance_category == "Excellent", na.rm = TRUE),
        sum(data$performance_category == "Good", na.rm = TRUE),
        paste0(round(mean(data$roc_ytd, na.rm = TRUE), 1), "%"),
        round(max(data$performance_score, na.rm = TRUE), 1)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$dash_top10 <- DT::renderDataTable({
    data <- current_data() %>%
      arrange(desc(performance_score)) %>%
      head(10) %>%
      select(symbol, performance_score, roc_ytd, close, sector) %>%
      mutate(
        performance_score = round(performance_score, 1),
        roc_ytd = round(roc_ytd, 2),
        close = round(close, 2)
      )
    
    DT::datatable(data,
                  options = list(dom = 't', pageLength = 10),
                  rownames = FALSE,
                  colnames = c("Symbol", "Score", "YTD %", "Price", "Sector"))
  })
  
  output$dash_sector_heatmap <- renderPlotly({
    sector_data <- current_data() %>%
      group_by(sector) %>%
      summarise(avg_score = mean(performance_score, na.rm = TRUE),
                n_stocks = n(), .groups = "drop")
    
    if(nrow(sector_data) == 0) return(plotly_empty())
    
    p <- plot_ly(
      sector_data,
      x = ~sector,
      y = ~n_stocks,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = ~n_stocks,
        color = ~avg_score,
        colorscale = "Viridis",
        showscale = TRUE,
        sizemode = "diameter",
        sizeref = 2,
        colorbar = list(title = "Avg Score")
      ),
      text = ~paste("Sector:", sector, "<br>Avg Score:", round(avg_score, 1),
                    "<br>Stocks:", n_stocks),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Sector"),
        yaxis = list(title = "Number of Stocks")
      )
    
    theme_trading_plotly(p)
  })
  
  # Screener reactive data
  filtered_data <- eventReactive(input$apply_filters, {
    req(nrow(current_data()) > 0)
    
    data <- current_data()
    
    if (input$ma_trend != "All") {
      data <- data %>% filter(ma_alignment == input$ma_trend)
    }
    
    # Apply sector filter if clicked
    if (!is.null(rv$clicked_sector)) {
      data <- data %>% filter(sector == rv$clicked_sector)
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
    
    rv$filtered_data <- data
    return(data)
  }, ignoreNULL = FALSE)
  
  filtered_data_init <- reactive({
    current_data() %>% 
      arrange(desc(performance_score)) %>% 
      head(50)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateNumericRangeInput(session, "ytd_range", value = c(-50, 100))
    updateNumericRangeInput(session, "volume_range", value = c(10000, 10000000))
    updateNumericRangeInput(session, "score_range", value = c(0, 100))
    updateSelectInput(session, "ma_trend", selected = "All")
    updateNumericInput(session, "min_up_percentage", value = 45)
    updateNumericInput(session, "min_streak", value = 5)
    updateNumericInput(session, "top_n", value = 50)
    rv$clicked_sector <- NULL
    
    showNotification("Filters reset to defaults", type = "message", duration = 2)
  })
  
  # Interactive sector filtering from charts
  observeEvent(event_data("plotly_click", source = "sector_chart"), {
    clicked <- event_data("plotly_click", source = "sector_chart")
    if(!is.null(clicked)) {
      rv$clicked_sector <- clicked$x
      showNotification(
        paste("Filtering by sector:", rv$clicked_sector, "- Click 'Apply Filters' to update"),
        type = "message",
        duration = 4
      )
    }
  })
  
  # Value boxes for screener
  output$filtered_count <- renderValueBox({
    count <- if(input$apply_filters == 0) {
      nrow(filtered_data_init())
    } else {
      nrow(filtered_data())
    }
    
    subtitle_text <- if(!is.null(rv$clicked_sector)) {
      paste("Filtered Stocks -", rv$clicked_sector)
    } else {
      "Filtered Stocks"
    }
    
    valueBox(value = count, subtitle = subtitle_text, icon = icon("filter"), color = "primary")
  })
  
  output$avg_performance <- renderValueBox({
    avg_perf <- if(input$apply_filters == 0) {
      data <- filtered_data_init()
      round(mean(data$performance_score, na.rm = TRUE), 1)
    } else {
      data <- filtered_data()
      if(nrow(data) > 0) round(mean(data$performance_score, na.rm = TRUE), 1) else 0
    }
    
    overall_avg <- round(mean(current_data()$performance_score, na.rm = TRUE), 1)
    diff <- avg_perf - overall_avg
    
    valueBox(
      value = tags$div(
        style = "font-size: 28px; font-weight: bold;",
        avg_perf
      ),
      subtitle = tags$div(
        "Avg Performance Score",
        tags$br(),
        tags$span(
          icon(if(diff > 0) "arrow-up" else "arrow-down"),
          paste0(abs(round(diff, 1)), " vs overall"),
          style = paste0("color: ", if(diff > 0) "#27AE60" else "#E74C3C", "; font-size: 12px;")
        )
      ),
      icon = icon("chart-line"),
      color = if(avg_perf >= 70) "success" else if(avg_perf >= 50) "warning" else "danger"
    )
  })
  
  output$top_performer <- renderValueBox({
    top_stock <- if(input$apply_filters == 0) {
      data <- filtered_data_init()
      if(nrow(data) > 0) data$symbol[1] else "None"
    } else {
      data <- filtered_data()
      if(nrow(data) > 0) data$symbol[1] else "None"
    }
    valueBox(value = top_stock, subtitle = "Top Performer", icon = icon("trophy"), color = "warning")
  })
  
  # Screener table with export buttons
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
             max_up_streak, ma_alignment, performance_category, sector) %>%
      mutate(
        performance_score = round(performance_score, 1),
        roc_ytd = round(roc_ytd, 2),
        close = round(close, 2),
        up_day_percentage = round(up_day_percentage, 1)
      ) %>%
      DT::datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'csv', filename = paste0('screener_', Sys.Date())),
            list(extend = 'excel', filename = paste0('screener_', Sys.Date())),
            'copy', 'print'
          ),
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(1, 'desc'))
        ),
        colnames = c("Symbol", "Performance Score", "YTD Return (%)", "Price ($)", "Volume", 
                     "Up Day %", "Max Up Streak", "MA Trend", "Category", "Sector")
      ) %>%
      DT::formatStyle("performance_score", 
                      backgroundColor = DT::styleInterval(c(50, 70, 80), 
                                                          c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
  # Stock Comparison Feature
  comparison_data <- eventReactive(input$run_comparison, {
    req(length(input$compare_stocks) > 0)
    
    rv$date_filtered_data %>%
      filter(symbol %in% input$compare_stocks) %>%
      group_by(symbol) %>%
      arrange(date) %>%
      ungroup()
  })
  
  output$comparison_cards <- renderUI({
    req(input$run_comparison > 0)
    req(length(input$compare_stocks) > 0)
    
    latest_data <- comparison_data() %>%
      group_by(symbol) %>%
      filter(date == max(date)) %>%
      ungroup()
    
    if(nrow(latest_data) == 0) {
      return(div(class = "info-badge",
                 icon("info-circle"), " No data available for selected stocks in this date range."))
    }
    
    cards <- lapply(1:nrow(latest_data), function(i) {
      stock <- latest_data[i, ]
      
      box(
        width = 12 / min(nrow(latest_data), 3),
        div(class = "comparison-card",
            h3(stock$symbol, style = "margin-top: 0; color: #2c3e50;"),
            div(class = "comparison-metric",
                span(class = "metric-label", "Performance Score:"),
                span(class = "metric-value", style = paste0("color: ", 
                                                            if(stock$performance_score >= 70) "#27AE60" else if(stock$performance_score >= 50) "#F39C12" else "#E74C3C"),
                     round(stock$performance_score, 1))
            ),
            div(class = "comparison-metric",
                span(class = "metric-label", "YTD Return:"),
                span(class = "metric-value", paste0(round(stock$roc_ytd, 2), "%"))
            ),
            div(class = "comparison-metric",
                span(class = "metric-label", "Price:"),
                span(class = "metric-value", paste0("$", round(stock$close, 2)))
            ),
            div(class = "comparison-metric",
                span(class = "metric-label", "Volume:"),
                span(class = "metric-value", format(stock$volume, big.mark = ","))
            ),
            div(class = "comparison-metric",
                span(class = "metric-label", "MA Trend:"),
                span(class = "metric-value", stock$ma_alignment)
            ),
            div(class = "comparison-metric",
                span(class = "metric-label", "Category:"),
                span(class = "metric-value", stock$performance_category)
            ),
            div(class = "comparison-metric",
                span(class = "metric-label", "Sector:"),
                span(class = "metric-value", stock$sector)
            )
        )
      )
    })
    
    fluidRow(cards)
  })
  
  output$comparison_chart <- renderPlotly({
    req(input$run_comparison > 0)
    req(length(input$compare_stocks) > 0)
    
    latest_data <- comparison_data() %>%
      group_by(symbol) %>%
      filter(date == max(date)) %>%
      ungroup()
    
    if(nrow(latest_data) == 0) return(plotly_empty("No data for comparison"))
    
    metrics <- c("performance_score", "roc_ytd", "volume_score", "consistency_score")
    plot_data <- latest_data %>%
      select(symbol, all_of(metrics)) %>%
      pivot_longer(cols = all_of(metrics), names_to = "Metric", values_to = "Value")
    
    p <- plot_ly(plot_data, x = ~Metric, y = ~Value, color = ~symbol, type = "bar") %>%
      layout(
        title = "Key Metrics Comparison",
        xaxis = list(title = ""),
        yaxis = list(title = "Value"),
        barmode = "group"
      )
    
    theme_trading_plotly(p)
  })
  
  output$comparison_radar <- renderPlotly({
    req(input$run_comparison > 0)
    req(length(input$compare_stocks) > 0)
    
    latest_data <- comparison_data() %>%
      group_by(symbol) %>%
      filter(date == max(date)) %>%
      ungroup()
    
    if(nrow(latest_data) == 0) return(plotly_empty("No data for radar chart"))
    
    # Normalize scores to 0-100 scale for radar chart
    radar_data <- latest_data %>%
      mutate(
        Performance = pmin(100, performance_score),
        Momentum = pmin(100, momentum_score),
        Trend = pmin(100, trend_score),
        Consistency = pmin(100, consistency_score),
        Volume = pmin(100, volume_score)
      ) %>%
      select(symbol, Performance, Momentum, Trend, Consistency, Volume)
    
    p <- plot_ly(type = 'scatterpolar', fill = 'toself', mode = 'lines+markers')
    
    for(i in 1:nrow(radar_data)) {
      p <- p %>% add_trace(
        r = c(radar_data$Performance[i], radar_data$Momentum[i], radar_data$Trend[i],
              radar_data$Consistency[i], radar_data$Volume[i], radar_data$Performance[i]),
        theta = c('Performance', 'Momentum', 'Trend', 'Consistency', 'Volume', 'Performance'),
        name = radar_data$symbol[i]
      )
    }
    
    p <- p %>% layout(
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 100))
      )
    )
    
    theme_trading_plotly(p)
  })
  
  output$comparison_historical <- renderPlotly({
    req(input$run_comparison > 0)
    req(length(input$compare_stocks) > 0)
    
    hist_data <- comparison_data()
    
    if(nrow(hist_data) == 0) return(plotly_empty("No historical data available"))
    
    p <- plot_ly(hist_data, x = ~date, y = ~close, color = ~symbol, type = 'scatter', mode = 'lines') %>%
      layout(
        title = "Historical Price Comparison",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price ($)"),
        hovermode = "x unified"
      )
    
    theme_trading_plotly(p)
  })
  
  output$comparison_table <- DT::renderDataTable({
    req(input$run_comparison > 0)
    req(length(input$compare_stocks) > 0)
    
    latest_data <- comparison_data() %>%
      group_by(symbol) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      select(symbol, performance_score, risk_adjusted_score, roc_ytd, roc5, roc10,
             close, volume, ma_alignment, performance_category, sector) %>%
      mutate(
        performance_score = round(performance_score, 1),
        risk_adjusted_score = round(risk_adjusted_score, 1),
        roc_ytd = round(roc_ytd, 2),
        roc5 = round(roc5, 3),
        roc10 = round(roc10, 3),
        close = round(close, 2)
      )
    
    DT::datatable(
      latest_data,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', filename = paste0('comparison_', Sys.Date())),
          list(extend = 'excel', filename = paste0('comparison_', Sys.Date())),
          'copy', 'print'
        ),
        pageLength = 10,
        scrollX = TRUE
      ),
      colnames = c("Symbol", "Performance", "Risk-Adj", "YTD %", "5d ROC", "10d ROC",
                   "Price", "Volume", "MA Trend", "Category", "Sector")
    )
  })
  
  # Top performers section
  top_performers_filtered <- eventReactive(input$apply_top_filters, {
    req(nrow(current_data()) > 0)
    
    data <- current_data()
    
    data <- data %>%
      filter(
        close >= input$price_range_top[1] & close <= input$price_range_top[2],
        volume >= input$volume_range_top[1] & volume <= input$volume_range_top[2],
        performance_score >= input$min_perf_score_top
      )
    
    if (input$perf_category_filter != "All") {
      data <- data %>% filter(performance_category == input$perf_category_filter)
    }
    
    data %>%
      arrange(desc(performance_score)) %>%
      head(input$top_n_performers)
  }, ignoreNULL = FALSE)
  
  top_performers_init <- reactive({
    current_data() %>% 
      arrange(desc(performance_score)) %>% 
      head(50)
  })
  
  output$total_stocks <- renderValueBox({
    valueBox(value = format(length(unique(current_data()$symbol)), big.mark = ","), 
             subtitle = "Total Stocks", icon = icon("list"), color = "primary")
  })
  
  output$filtered_top_count <- renderValueBox({
    count <- if(input$apply_top_filters == 0) {
      nrow(top_performers_init())
    } else {
      nrow(top_performers_filtered())
    }
    valueBox(value = count, subtitle = "Filtered Top Stocks", icon = icon("filter"), color = "success")
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
  
  output$top_performers_chart <- renderPlotly({
    data_to_plot <- if(input$apply_top_filters == 0) {
      top_performers_init()
    } else {
      top_performers_filtered()
    }
    
    if(nrow(data_to_plot) == 0) return(plotly_empty())
    
    # Use manual colors if fewer than 3 categories
    n_categories <- length(unique(data_to_plot$performance_category))
    
    p <- ggplot(data_to_plot, aes(x = reorder(symbol, performance_score), y = performance_score, fill = performance_category)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      labs(title = "", x = "Symbol", y = "Performance Score") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 6))
    
    if(n_categories >= 3) {
      p <- p + scale_fill_brewer(palette = "Spectral")
    } else {
      p <- p + scale_fill_manual(values = c("Excellent" = "#2ecc71", "Good" = "#3498db", 
                                            "Average" = "#f39c12", "Below Average" = "#e67e22", 
                                            "Poor" = "#e74c3c"))
    }
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>% theme_trading_plotly()
  })
  
  output$score_distribution <- renderPlotly({
    data_to_plot <- if(input$apply_top_filters == 0) {
      current_data() %>% arrange(desc(performance_score)) %>% head(1000)
    } else {
      data <- current_data() %>%
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
    
    if(nrow(data_to_plot) == 0) return(plotly_empty())
    
    p <- ggplot(data_to_plot, aes(x = performance_score)) +
      geom_histogram(bins = 30, fill = "#667eea", alpha = 0.7, color = "white") +
      geom_vline(xintercept = mean(data_to_plot$performance_score, na.rm = TRUE), 
                 linetype = "dashed", color = "#E74C3C", linewidth = 1) +
      labs(title = "", x = "Performance Score", y = "Count") +
      theme_minimal()
    
    ggplotly(p) %>% theme_trading_plotly()
  })
  
  output$price_performance_scatter <- renderPlotly({
    data_to_plot <- if(input$apply_top_filters == 0) {
      current_data() %>% arrange(desc(performance_score)) %>% head(500)
    } else {
      data <- current_data() %>%
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
    
    p <- plot_ly(data_to_plot, x = ~close, y = ~performance_score, color = ~performance_category,
                 colors = c("Excellent" = "#2ecc71", "Good" = "#3498db", 
                            "Average" = "#f39c12", "Below Average" = "#e67e22", 
                            "Poor" = "#e74c3c"),
                 size = ~volume, sizes = c(5, 50),
                 text = ~paste("Symbol:", symbol,
                               "<br>Price:", round(close, 2),
                               "<br>Performance:", round(performance_score, 1)),
                 hoverinfo = "text") %>%
      add_markers(alpha = 0.6) %>%
      layout(title = "", xaxis = list(title = "Price ($)"), 
             yaxis = list(title = "Performance Score"))
    
    theme_trading_plotly(p)
  })
  
  output$volume_performance_scatter <- renderPlotly({
    data_to_plot <- if(input$apply_top_filters == 0) {
      current_data() %>% arrange(desc(performance_score)) #%>% head(500)
    } else {
      data <- current_data() %>%
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
    
    p <- plot_ly(data_to_plot, x = ~log10(volume), y = ~performance_score, 
                 color = ~close, colors = viridis(100),
                 size = ~abs(roc_ytd), sizes = c(5, 50),
                 text = ~paste("Symbol:", symbol,
                               "<br>Volume:", format(volume, big.mark = ","),
                               "<br>Performance:", round(performance_score, 1)),
                 hoverinfo = "text") %>%
      add_markers(alpha = 0.6) %>%
      layout(title = "", xaxis = list(title = "Log10(Volume)"), 
             yaxis = list(title = "Performance Score"))
    
    theme_trading_plotly(p)
  })
  
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
          close = round(close, 2)
        ),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', filename = paste0('top_performers_', Sys.Date())),
          list(extend = 'excel', filename = paste0('top_performers_', Sys.Date())),
          'copy'
        ),
        pageLength = 25,
        scrollX = TRUE
      ),
      colnames = c("Symbol", "Performance Score", "Risk-Adj Score", "YTD Return (%)", 
                   "5d ROC", "10d ROC", "Volume", "Price ($)", "MA Trend")
    )
  })
  
  # Performance Analysis Charts
  output$returns_heatmap <- renderPlotly({
    top_100 <- current_data() %>% 
      filter(!is.na(performance_score), !is.na(roc_ytd)) %>%
      arrange(desc(performance_score)) %>% 
      head(100)
    
    if(nrow(top_100) == 0) return(plotly_empty())
    
    heatmap_matrix <- matrix(top_100$roc_ytd, nrow = 10, byrow = TRUE)
    heatmap_text <- matrix(paste(top_100$symbol, "<br>YTD:", round(top_100$roc_ytd, 2), "%"), 
                           nrow = 10, byrow = TRUE)
    
    p <- plot_ly(x = 1:10, y = 1:10, z = heatmap_matrix, type = "heatmap",
                 text = heatmap_text, hoverinfo = "text",
                 colorscale = list(c(0, "red"), c(0.5, "yellow"), c(1, "green"))) %>%
      layout(title = "",
             xaxis = list(title = "", showticklabels = FALSE), 
             yaxis = list(title = "", showticklabels = FALSE))
    
    theme_trading_plotly(p)
  })
  
  output$momentum_analysis <- renderPlotly({
    momentum_data <- current_data() %>%
      filter(!is.na(roc5), !is.na(roc10), !is.na(roc_ytd)) %>%
      arrange(desc(performance_score)) %>%
      head(200) %>%
      select(symbol, roc5, roc10, roc_ytd) %>%
      pivot_longer(cols = starts_with("roc"), names_to = "ROC_Period", values_to = "ROC_Value")
    
    if(nrow(momentum_data) == 0) return(plotly_empty())
    
    # Use manual colors
    p <- ggplot(momentum_data, aes(x = symbol, y = ROC_Value, fill = ROC_Period)) +
      geom_col(position = "dodge", alpha = 0.8) +
      labs(title = "", x = "Symbol", y = "ROC Value") +
      theme_minimal() +
      scale_fill_manual(values = c("roc5" = "#66c2a5", "roc10" = "#fc8d62", "roc_ytd" = "#8da0cb")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
    
    ggplotly(p) %>% theme_trading_plotly()
  })
  
  output$trend_analysis <- renderPlotly({
    trend_data <- current_data() %>%
      filter(!is.na(recent_up_trend_21d), !is.na(recent_up_trend_63d)) %>%
      arrange(desc(performance_score)) %>%
      head(100)
    
    if(nrow(trend_data) == 0) return(plotly_empty())
    
    p <- plot_ly(trend_data, x = ~recent_up_trend_21d, y = ~recent_up_trend_63d, 
                 color = ~performance_score, colors = viridis(100),
                 size = ~volume, sizes = c(5, 30),
                 text = ~paste("Symbol:", symbol,
                               "<br>21d Trend:", recent_up_trend_21d,
                               "<br>63d Trend:", recent_up_trend_63d),
                 hoverinfo = "text") %>%
      add_markers(alpha = 0.7) %>%
      layout(title = "", 
             xaxis = list(title = "Recent Up Trend 21d"), 
             yaxis = list(title = "Recent Up Trend 63d"))
    
    theme_trading_plotly(p)
  })
  
  # Technical Analysis
  output$ma_alignment_chart <- renderPlotly({
    ma_data <- current_data() %>%
      filter(!is.na(close), !is.na(ma_4d), !is.na(ma_7d), !is.na(ma_21d), !is.na(ma_42d)) %>%
      arrange(desc(performance_score)) %>%
      head(50) %>%
      select(symbol, close, ma_4d, ma_7d, ma_21d, ma_42d) %>%
      pivot_longer(cols = c(close, ma_4d, ma_7d, ma_21d, ma_42d), 
                   names_to = "MA_Type", values_to = "Price")
    
    if(nrow(ma_data) == 0) return(plotly_empty())
    
    p <- plot_ly(ma_data, x = ~MA_Type, y = ~Price, color = ~symbol, type = 'scatter',
                 mode = 'lines+markers') %>%
      layout(title = "", xaxis = list(title = "MA Type"), 
             yaxis = list(title = "Price"),
             showlegend = FALSE)
    
    theme_trading_plotly(p)
  })
  
  output$volume_performance <- renderPlotly({
    vol_perf_data <- current_data() %>%
      filter(!is.na(volume), !is.na(performance_score), !is.na(roc_ytd)) %>%
      arrange(desc(performance_score)) %>%
      head(500)
    
    if(nrow(vol_perf_data) == 0) return(plotly_empty())
    
    p <- plot_ly(vol_perf_data, x = ~log10(volume), y = ~roc_ytd, 
                 color = ~performance_score, colors = viridis(100),
                 size = ~up_day_percentage, sizes = c(5, 30),
                 text = ~paste("Symbol:", symbol,
                               "<br>Volume:", format(volume, big.mark = ","),
                               "<br>YTD:", round(roc_ytd, 2), "%"),
                 hoverinfo = "text") %>%
      add_markers(alpha = 0.6) %>%
      layout(title = "", xaxis = list(title = "Log10(Volume)"), 
             yaxis = list(title = "YTD Return (%)"))
    
    theme_trading_plotly(p)
  })
  
  output$consecutive_days <- renderPlotly({
    consec_data <- current_data() %>%
      filter(!is.na(consecutive_up_days), !is.na(max_up_streak)) %>%
      arrange(desc(performance_score)) %>%
      head(200)
    
    if(nrow(consec_data) == 0) return(plotly_empty())
    
    p <- plot_ly(consec_data, x = ~consecutive_up_days, y = ~max_up_streak, 
                 color = ~performance_score, colors = viridis(100),
                 size = ~volume, sizes = c(5, 30),
                 text = ~paste("Symbol:", symbol,
                               "<br>Consecutive:", consecutive_up_days,
                               "<br>Max Streak:", max_up_streak),
                 hoverinfo = "text") %>%
      add_markers(alpha = 0.7) %>%
      layout(title = "", 
             xaxis = list(title = "Current Consecutive Up Days"), 
             yaxis = list(title = "Max Up Streak"))
    
    theme_trading_plotly(p)
  })
  
  # Risk Analysis
  output$efficient_frontier <- renderPlotly({
    risk_data <- current_data() %>%
      filter(!is.na(risk_adjusted_score), !is.na(roc_ytd), !is.na(volatility_proxy)) %>%
      head(500)
    
    if(nrow(risk_data) == 0) return(plotly_empty())
    
    p <- plot_ly(risk_data, x = ~volatility_proxy, y = ~roc_ytd, 
                 color = ~performance_score, colors = viridis(100, option = "plasma"),
                 size = ~performance_score, sizes = c(5, 30),
                 text = ~paste("Symbol:", symbol,
                               "<br>YTD:", round(roc_ytd, 2), "%",
                               "<br>Volatility:", round(volatility_proxy, 3)),
                 hoverinfo = "text") %>%
      add_markers(alpha = 0.6) %>%
      layout(title = "",
             xaxis = list(title = "Volatility Proxy"), 
             yaxis = list(title = "YTD Return (%)"))
    
    theme_trading_plotly(p)
  })
  
  output$volatility_analysis <- renderPlotly({
    vol_data <- current_data() %>%
      filter(!is.na(roc5), !is.na(roc10)) %>%
      arrange(desc(performance_score)) %>%
      head(500) %>%
      mutate(roc_diff = abs(roc5 - roc10))
    
    if(nrow(vol_data) == 0) return(plotly_empty())
    
    p <- plot_ly(vol_data, x = ~roc_diff, y = ~performance_score, 
                 color = ~performance_category,
                 colors = c("Excellent" = "#2ecc71", "Good" = "#3498db", 
                            "Average" = "#f39c12", "Below Average" = "#e67e22", 
                            "Poor" = "#e74c3c"),
                 text = ~paste("Symbol:", symbol,
                               "<br>ROC Diff:", round(roc_diff, 2),
                               "<br>Performance:", round(performance_score, 1)),
                 hoverinfo = "text") %>%
      add_markers(alpha = 0.7) %>%
      layout(title = "",
             xaxis = list(title = "Absolute Difference (5d vs 10d ROC)"), 
             yaxis = list(title = "Performance Score"))
    
    theme_trading_plotly(p)
  })
  
  output$risk_adjusted_table <- DT::renderDataTable({
    risk_table_data <- current_data() %>%
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
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', filename = paste0('risk_adjusted_', Sys.Date())),
          'copy'
        ),
        pageLength = 25,
        scrollX = TRUE
      ),
      colnames = c("Symbol", "Performance Score", "Risk-Adj Score", "YTD Return (%)", 
                   "Volatility Proxy", "MA Trend")
    ) %>%
      DT::formatStyle("risk_adjusted_score", 
                      backgroundColor = DT::styleInterval(c(40, 60, 80), 
                                                          c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
  # Sector Analysis
  output$sector_performance <- renderPlotly({
    sector_perf <- current_data() %>%
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
    
    p <- plot_ly(sector_perf, 
                 x = ~reorder(sector, avg_score), 
                 y = ~avg_score, 
                 type = 'bar',
                 marker = list(color = ~avg_score, colorscale = "Viridis"),
                 text = ~paste("Sector:", sector,
                               "<br>Avg Score:", round(avg_score, 1),
                               "<br>Median YTD:", round(median_ytd, 2), "%",
                               "<br>Stocks:", n_stocks),
                 hoverinfo = "text",
                 source = "sector_chart") %>%
      layout(title = "",
             xaxis = list(title = "Sector"), 
             yaxis = list(title = "Average Performance Score"))
    
    p <- theme_trading_plotly(p)
    event_register(p, 'plotly_click')
    p
  })
  
  output$sector_risk_return <- renderPlotly({
    sector_risk_return <- current_data() %>%
      group_by(sector) %>%
      summarise(
        avg_ytd = mean(roc_ytd, na.rm = TRUE),
        avg_volatility = mean(volatility_proxy, na.rm = TRUE),
        n_stocks = n(),
        .groups = "drop"
      ) %>%
      filter(n_stocks > 10)
    
    if(nrow(sector_risk_return) == 0) return(plotly_empty())
    
    p <- plot_ly(sector_risk_return, x = ~avg_volatility, y = ~avg_ytd, 
                 color = ~sector, size = ~n_stocks, sizes = c(20, 100),
                 text = ~paste("Sector:", sector,
                               "<br>Avg YTD:", round(avg_ytd, 2), "%",
                               "<br>Avg Volatility:", round(avg_volatility, 3),
                               "<br>Stocks:", n_stocks),
                 hoverinfo = "text") %>%
      add_markers(alpha = 0.8) %>%
      layout(title = "",
             xaxis = list(title = "Average Volatility Proxy"), 
             yaxis = list(title = "Average YTD Return (%)"))
    
    theme_trading_plotly(p)
  })
  
  output$sector_best <- DT::renderDataTable({
    best_by_sector <- current_data() %>%
      group_by(sector) %>%
      slice_max(performance_score, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(sector, symbol, performance_score, roc_ytd, close, volume) %>%
      arrange(desc(performance_score)) %>%
      mutate(
        performance_score = round(performance_score, 1),
        roc_ytd = round(roc_ytd, 2),
        close = round(close, 2)
      )
    
    if(nrow(best_by_sector) == 0) return(DT::datatable(data.frame(Message = "No data available")))
    
    DT::datatable(
      best_by_sector,
      options = list(pageLength = 25, scrollX = TRUE, dom = 't'),
      colnames = c("Sector", "Symbol", "Performance Score", "YTD Return (%)", "Price ($)", "Volume")
    ) %>%
      DT::formatStyle("performance_score", 
                      backgroundColor = DT::styleInterval(c(50, 70, 80), 
                                                          c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
  # Advanced Filters
  advanced_filtered_data <- eventReactive(input$apply_advanced_filters, {
    req(nrow(current_data()) > 0)
    
    data <- current_data() %>%
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
  
  output$advanced_filtered_table <- DT::renderDataTable({
    if(input$apply_advanced_filters == 0) {
      return(DT::datatable(data.frame(Message = "Click 'Find Best Stocks' to see results")))
    }
    
    data <- advanced_filtered_data()
    if(nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No stocks match all criteria")))
    }
    
    DT::datatable(
      data %>%
        select(symbol, performance_score, risk_adjusted_score, roc_ytd, roc5, 
               volume, close, ma_alignment, performance_category) %>%
        mutate(
          performance_score = round(performance_score, 1),
          risk_adjusted_score = round(risk_adjusted_score, 1),
          roc_ytd = round(roc_ytd, 2),
          roc5 = round(roc5, 3),
          close = round(close, 2)
        ),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', filename = paste0('advanced_filter_', Sys.Date())),
          list(extend = 'excel', filename = paste0('advanced_filter_', Sys.Date())),
          'copy', 'print'
        ),
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, 'desc'))
      ),
      colnames = c("Symbol", "Performance Score", "Risk-Adj Score", "YTD Return (%)", 
                   "5d ROC", "Volume", "Price ($)", "MA Trend", "Category")
    ) %>%
      DT::formatStyle("performance_score", 
                      backgroundColor = DT::styleInterval(c(60, 75, 85), 
                                                          c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
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
  
  # Data Explorer
  output$correlation_plot <- renderPlot({
    cor_data <- current_data() %>%
      select(performance_score, roc_ytd, roc5, roc10, volume, up_day_percentage, 
             max_up_streak, recent_up_trend_21d, recent_up_trend_63d) %>%
      mutate(volume = log10(volume)) %>%
      na.omit()
    
    if(nrow(cor_data) == 0) return(NULL)
    
    cor_matrix <- cor(cor_data, use = "complete.obs")
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             addCoef.col = "black", number.cex = 0.7,
             title = "Correlation Matrix of Key Metrics",
             mar = c(0, 0, 2, 0))
  })
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      current_data() %>% 
        select(symbol, date, close, volume, roc_ytd, performance_score, 
               ma_alignment, performance_category, sector) %>%
        mutate(
          close = round(close, 2),
          roc_ytd = round(roc_ytd, 2),
          performance_score = round(performance_score, 1)
        ),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', filename = paste0('full_data_', Sys.Date())),
          list(extend = 'excel', filename = paste0('full_data_', Sys.Date())),
          'copy'
        ),
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(5, 'desc'))
      ),
      filter = 'top'
    )
  })
}

# Run the app
shinyApp(ui, server)

