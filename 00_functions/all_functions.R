gdp_growth_rate_plot <-
function(data, name){
    
  g2 <- data %>% 
        select(state, gdp, format_date, year, quarter) %>%
        group_by(state) %>%
        mutate(lag_1 = lag(gdp, n = 1)) %>%
        mutate(lag_1 = case_when(is.na(lag_1) ~ gdp,
                                 TRUE ~ lag_1)) %>%
        summarise(
            rate = (gdp - lag_1) / lag_1,
            row                   = row_number(),
            format_date           = format_date[row],
            year                  = year[row],
            quarter               = quarter[row]
            
        ) %>%
        ungroup() %>%
        mutate(rate_text = scales::percent(rate, accuracy = 0.1)) %>%
        mutate(ind_cont_perc_text = str_glue("State : {state}
                                          Growth Rate : {rate_text}
                                         Duration : {year}-{quarter}")) %>%
        mutate(pos = rate >= 0) %>%
        
        ggplot(aes(x = format_date, y = rate, fill = pos)) +
        geom_col() +
        # geom_line() +
        geom_point(aes(text = ind_cont_perc_text),
                   color = "#2c3e50",
                   size = 0.01) +
        # geom_smooth(method = "loess", span = 0.2) +
        theme_tq() +
        scale_fill_tq() +
        expand_limits(y = 0) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(x = "",
             y = "Growth rate",
             title = str_glue("{name} Quarterly GDP Growth rate")) +
        theme(legend.position = "none")
    
    
    ggplotly(g2, tooltip = "text")
    
    
}
gdp_timeseries_plot <-
function(data, name){
    
    g1 <- data %>%
        ggplot(aes(x = format_date, y = gdp)) +
        geom_line(color     = "#2c3e50") +
        geom_point(aes(text = state_cont_perc_text),
                   color    = "#2c3e50",
                   size     = 0.01) +
        geom_smooth(method  = "loess", span = 0.2) +
        
        theme_tq() +
        scale_color_tq() +
        expand_limits(y = 0) +
        scale_y_log10(labels = scales::dollar_format(scale = 1/1000000)) +
        labs(x = "", 
             y = "GDP (Log Scale) Trillion",
             title = str_glue("{name} GDP Plot"))
    
    
    ggplotly(g1, tooltip = "text")
    
}
time_picker_state <-
function(data, time_unit = "quarter"){
    
    if(time_unit %in% "quarter"){
        quarterly_tbl <- data %>% 
            mutate(format_date = floor_date(gdp_date, unit = time_unit)) %>% 
            group_by(format_date) %>% 
            mutate(first_gdp = first(gdp)) %>% 
            rename(state = GeoName) %>% 
        
        summarise(
            row = row_number(),
            state_contribution = gdp/first_gdp,
            state = state[row],
            gdp = gdp[row],
            quarter = quarter[row],
            abb = abb[row]
        ) %>% 
            
            ungroup() %>% 
            
            dplyr::select(state, gdp, everything(), -row) %>%
            mutate(
                state_contribution = dplyr::case_when(
                    state_contribution == 1 ~ NA_real_,
                    TRUE ~ as.numeric(state_contribution)
                )
            ) %>% 
            
            group_by(format_date) %>%
            mutate(rank = dense_rank(desc(state_contribution))) %>%
            ungroup() %>%
            mutate(year     = year(format_date)) %>%
            mutate(gdp_text = scales::dollar(gdp, scale = 1/1000000)) %>%
            mutate(state_cont_percent = state_contribution %>% scales::percent(accuracy = 0.1)) %>%
            mutate(
                state_cont_perc_text = str_glue(
                                        "State: {state}
                                         Rank: {rank}
                                         State GDP : {gdp_text} Trillion
                                         Contribution to National GDP : {state_cont_percent}
                                         Duration: {year}-{quarter}"
                )
            ) 
        
       return(quarterly_tbl) 
    }
    
    else if(time_unit %in% "year"){
        
        yearly_tbl <- data %>% 
            rename(state = GeoName) %>% 
            mutate(format_date = floor_date(gdp_date, unit = time_unit)) %>% 
            
            group_by(state, format_date) %>% 
            summarise(yearly_gdp = mean(gdp)) %>%  
            ungroup() %>% 
            
            # make "United states" First
            pivot_wider(names_from = state, values_from = yearly_gdp) %>% 
            dplyr::select(format_date, `United States`, everything()) %>%
            pivot_longer(
                cols      = -format_date,
                names_to  = "state",
                values_to = "yearly_gdp"
            ) %>%
            dplyr::select(state, everything()) %>%    
            
            # group and fstate the percent contribution yearly
            group_by(format_date) %>%
            mutate(first_gdp = first(x = yearly_gdp)) %>% 
        
        # to retain the state column
        summarise(
            row                   = row_number(),
            state_contribution    = yearly_gdp / first_gdp,
            state                 = state[row],
            yearly_gdp            = yearly_gdp[row],
            abb       = abb[row]
            
        ) %>%
            ungroup() %>%
            
            dplyr::select(-row) %>%
            rename(gdp = yearly_gdp) %>%
            dplyr::select(state, gdp, everything()) %>%
            mutate(
                state_contribution     = dplyr::case_when(
                    state_contribution == 1 ~ NA_real_,
                    TRUE ~ as.numeric(state_contribution)
                )
            ) %>%
            
            group_by(format_date) %>%
            mutate(rank = dense_rank(desc(state_contribution))) %>%
            ungroup() %>%
            mutate(year = year(format_date)) %>%
            mutate(gdp_text         = scales::dollar(gdp, scale = 1/1000000)) %>%
            mutate(state_cont_percent = state_contribution %>% scales::percent(accuracy = 0.1)) %>%
            mutate(
                state_cont_perc_text  = str_glue(
                                        "State: {state}
                                         Rank : {rank}
                                         State GDP : {scales::dollar(gdp)} Trillion
                                         Contribution to National GDP : {state_cont_percent}
                                         Duration: {year}"
                )
            )
        
        return(yearly_tbl)
    }
    
}
pick_state <-
function(data, state_name = "United States") {
    if (state_name == "United States") {
        
        option1 <- data %>%
            plot_geo(locationmode = "USA-states") %>%
            add_trace(
                z         = ~ gdp,
                locations = ~ abb,
                color     = ~ gdp,
                text      = ~ state_cont_perc_text,
                colors    = "Greens"
            ) %>%
            layout(geo = list(
                scope = "usa",
                projection = list(type = "albers usa"),
                showlakes  = TRUE,
                lakecolor  = toRGB("white")
            ))
        
        return(option1)        
        
        
        
    } else if (state_name != "United States") {
        
        
        option2 <- data %>%
            filter(state %in% state_name) %>%
            plot_geo(locationmode = "USA-states") %>%
            add_trace( z         = ~ gdp,
                       locations = ~ abb,
                       color     = ~ gdp,
                       text      = ~ state_cont_perc_text,
                       colors    = "Greens"
            ) %>%
            layout(geo = list(
                scope = "usa",
                projection = list(type = "albers usa"),
                showlakes  = TRUE,
                lakecolor  = toRGB("white")
                
            ))
        
        return(option2)    
    }
    
}
info_card <-
function(title, value,
             main_icon = "chart-line",
             bg_color = "default", text_color = "default") {
        
        div(
            class = "panel panel-default",
            style = "padding: 0px;",
            div(
                class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
                p(class = "pull-right", icon(class = "fa-2x", main_icon)),
                h4(title),
                h5(value),
                
            )
        )
        
    }
