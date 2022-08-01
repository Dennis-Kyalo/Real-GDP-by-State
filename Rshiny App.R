# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

library(plotly)
library(tidyquant)
library(tidyverse)


source(file = "00_functions/all_functions.R")

metrics_data <- read_rds(file = "00_data/metrics_data.rds")
stategdp_wrangled_tbl <- read_rds(file = "00_data/stategdp_wrangled.rds")



# UI ---------------------------------------------------------------------------------------------------------

ui <- fluidPage(
    title = "REAL GROSS DOMESTIC PRODUCT BY STATE",
    inverse = FALSE,
    collapsible = TRUE,
    theme = shinytheme("cyborg"),
    
    # CSS 
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    #  JS 
    shinyjs::useShinyjs(),
    
    
    # APP Header 
    div(
        class = "container-fluid",
        id    = "header1",
        h2("REAL GROSS DOMESTIC PRODUCT BY STATE")
    ),
    
    
    
    # * Accuracy Metrics --------------------------------------------------------------------
    div(
        class = "container-fluid",
        id    = "metrics_container",
        
        div( 
            id = "metrics_header",
            column(
                width = 12,
                h3("Metrics")
            )
        ),
        div(
            id    = "favorite_cards",
            
            fluidRow(
                
                column(
                    width = 3,
                    info_card(title     = "State GDP", main_icon = "usd",
                              value     =  verbatimTextOutput(outputId = "state_gdp")
                    )
                ),
                column(
                    width = 3,
                    info_card(title     = "Contribution to National GDP", 
                              value     = verbatimTextOutput(outputId = "contribution")
                    )
                ),
                column(
                    width = 3,
                    info_card(title     = "Rank in National GDP Contribution", main_icon = "line-chart", 
                              value     = verbatimTextOutput(outputId = "rank")
                    )
                ),
                column(
                    width = 3,
                    info_card(title     = "Quarterly Growth Rate", main_icon = "percent",
                              value     = verbatimTextOutput(outputId = "growth_rate") 
                    )
                )
            )
            
        )
    ), 
    
    
    # SIDE PANEL  --------------------------------------------------------------
    
    div(
        class = "container-fluid",
        id    = "application",
        
        
        fluidRow( 
            
            column(width = 3,
                   
                   # * Date Range ----
                   wellPanel(
                       id = "date_range",
                       
                       dateRangeInput(
                           inputId = "date_range", 
                           label   = h4("Date Range"),
                           start   = min(metrics_data$format_date), 
                           end     = max(metrics_data$format_date), 
                           min     = min(metrics_data$format_date), 
                           max     = max(metrics_data$format_date), 
                           startview = "month"
                       )
                       
                   ),
                   
                   # * State Picker Inputs -----
                   wellPanel(
                       id    = "pickers",
                       
                       shinyWidgets::pickerInput(
                           inputId  = "state_picker",
                           label    = h4("State"),
                           choices  = unique(metrics_data$state) %>% sort(),
                           selected = "United States",
                           multiple = FALSE, 
                           options = pickerOptions(actionsBox = FALSE, 
                                                   liveSearch = TRUE, 
                                                   size = 10
                                                   
                           )
                           
                       ),
                       
                       fluidRow(
                           
                           # * Year Picker Inputs -----
                           column(width = 6, 
                                  shinyWidgets::pickerInput(
                                      inputId  = "year_picker",
                                      label    = h4("Year"),
                                      choices  = unique(metrics_data$year) %>% sort(decreasing = TRUE),
                                      selected = max(metrics_data$year),
                                      multiple = FALSE,
                                      options  = list(
                                          `actions-box` = TRUE,
                                          size = 10,
                                          `selected-text-format` = "count > 3"
                                      )
                                  ),
                                  br(),
                                  actionButton(inputId = "apply", label = "Apply", icon = icon("play"))
                                  
                           ),
                           
                           # * Quarter Picker Inputs -----
                           column(width = 6,
                                  shinyWidgets::pickerInput(
                                      inputId = "quarter_picker",
                                      label = h4("Quarter"),
                                      choices = unique(metrics_data$quarter) %>% sort(),
                                      selected = "Q3",
                                      multiple = FALSE
                                      
                                  ),
                                  br(),
                                  actionButton(inputId = "reset", label = "Reset", icon = icon("sync"),
                                               class = "btn pull-right"
                                  )  
                                  
                           )
                           
                           
                           
                       )
                       
                   ),
                   
                   # * Data Description ----------
                   wellPanel(
                       id = "about_data",
                       h4("About the data"),
                       hr(),
                       p("The data was acquired from the Bureau of Economic Analysis
                              U.S. Department of Commerce.",
                         br(),
                         br(),
                         a(href = "https://www.bea.gov/", class = "btn btn-primary", "Learn More"))
                   )
                   
                   
            ),
            
            # MAIN PANEL --------------------------------------------------------------------------
            
            column(width = 9,
                   
                   # * States Map Plot ---------
                   
                   div(
                       class = "well",
                       plotlyOutput(outputId = "plotly_1")
                   ),
                   
                   
                   
                   
                   
                   fluidRow(
                       
                       column(width = 6,
                              
                              div(
                                  # * State GDP Time Series Plot -----------
                                  class = "well",
                                  plotlyOutput(outputId = "plotly_2")  
                                  
                              )
                              
                              
                       ),
                       
                       column(width = 6,
                              
                              
                              div(
                                  # * State Quarterly GDP Growth Rate Plot -------------
                                  class = "well",
                                  plotlyOutput(outputId = "plotly_3")
                                  
                              )
                              
                              
                              
                       )
                       
                   )
                   
                   
                   
            )
            
            
        )
        
        
        
    )
    
)


    
     
    
# SERVER -----------------------------------------------------------------------------------

server <- function(input, output, session) {
   
    # * Updating Information ----------------------
    observeEvent(eventExpr = input$reset, handlerExpr = {
        
        updatePickerInput(
            session = session,
            inputId = "state_picker",
            selected = "United States"
        )
        
        updateDateRangeInput(
            session = session,
            inputId = "date_range",
            start = min(metrics_data$format_date),
            end = max(metrics_data$format_date)
        )
        
        updatePickerInput(
            session = session,
            inputId = "year_picker",
            selected = max(metrics_data$year)
        )
        
        updatePickerInput(session = session,
                          inputId = "quarter_picker",
                          selected = "Q4")
        
        shinyjs::delay(ms = 300, expr = {
            shinyjs::click(id = "apply")
        })
        
    })
    
    
    # * Filtered Data -------------------------------
    
    date_chosen <- eventReactive(input$apply, {
        input$date_range
    },
    ignoreNULL = FALSE)
    
    
    state_chosen <- eventReactive(input$apply, {
        input$state_picker
    },
    ignoreNULL = FALSE)
    
    
    year_chosen <- eventReactive(input$apply, {
        input$year_picker
    },
    ignoreNULL = FALSE)
    
    
   quarter_chosen <- eventReactive(input$apply, {
       input$quarter_picker
    },
    ignoreNULL = FALSE)
    
   
    
    filtered_data1 <- reactive({
        
        time_picker_state(stategdp_wrangled_tbl, time_unit = "quarter") %>%
            filter(format_date %>% between(left  = date_chosen()[1],
                                           right = date_chosen()[2]))
        
    })
    
    
    
    # * Metrics ----------------------------------------
    
    filtered_metrics_tbl <- reactive({
        
        metrics_data %>%
            filter(year %in% year_chosen() & quarter %in% quarter_chosen()) %>%
            filter(state %in% state_chosen())
        
    })
    
    
    # ** Metrics Reactivity ------
    output$state_gdp <-
        renderText(
            filtered_metrics_tbl() %>% pull(gdp) %>%
                scales::number(
                    big.mark = ",",
                    suffix = " Trillion",
                    prefix = "$",
                    scale = 1 / 1000000, 
                    accuracy = 0.01
                )
        )
    
    output$contribution <- renderText(
        filtered_metrics_tbl() %>% 
            pull(state_cont_percent)
    )
    
    output$rank <- renderText(
        filtered_metrics_tbl() %>% 
            pull(rank)
    )
    
    
    rate_tbl <- metrics_data %>%
        select(state, gdp, format_date, quarter) %>%
        group_by(state) %>%
        mutate(lag_1 = lag(gdp, n = 1)) %>%
        mutate(lag_1 = case_when(is.na(lag_1) ~ gdp,
                                 TRUE ~ lag_1)) %>%
        summarise(
            rate = (gdp - lag_1) / lag_1,
            row                   = row_number(),
            format_date           = format_date[row],
            quarter               = quarter[row],
        ) %>%
        ungroup() %>%
        mutate(rate_text = scales::percent(rate, accuracy = 0.1)) %>% 
        mutate(year = year(format_date))
    
    
    
    rate_metrics_tbl <- reactive({ 
        
        rate_tbl %>%
            filter(year %in% year_chosen() & quarter %in% quarter_chosen()) %>%
            filter(state %in% state_chosen())
        
    })
    
    

    output$growth_rate <- renderText(
        rate_metrics_tbl() %>% pull(rate_text)
    )
    
    

    # * States Map Plot ------------------
    
    pick_state_map <- eventReactive(
        eventExpr = input$apply,
        
        valueExpr = {
            
            time_picker_state(stategdp_wrangled_tbl, time_unit = "quarter") %>%
                filter(format_date %>% between(left  = input$date_range[1],
                                               right = input$date_range[2])) %>% 
                filter(year == input$year_picker & quarter == input$quarter_picker) %>% 
                pick_state(state_name = input$state_picker)
            
        },
        
        ignoreNULL = FALSE
    )
    
    
    output$plotly_1 <- renderPlotly({
        
        pick_state_map()
        
    })
    
    
    
    # * States GDP Time series Plot ---------------
    
    output$plotly_2 <- renderPlotly({
        
        filtered_data1() %>% 
            filter(state %in% state_chosen()) %>% 
            gdp_timeseries_plot(state_chosen())
        
    })
    
    
    # State Quarterly GDP Growth Rate Plot ----------------
    
     output$plotly_3 <- renderPlotly({
        
       filtered_data1() %>% 
             filter(state %in% state_chosen()) %>% 
           gdp_growth_rate_plot(state_chosen())
            
    })
    
}

# SHINYAPP ------------------------------------------------------------------

shinyApp(ui = ui, server = server)
    

