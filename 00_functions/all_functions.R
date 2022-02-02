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
            mutate(gdp_text = scales::dollar(gdp)) %>%
            mutate(state_cont_percent = state_contribution %>% scales::percent(accuracy = 0.1)) %>%
            mutate(
                state_cont_perc_text = str_glue(
                                        "State: {state}
                                         Rank: {rank}
                                         State GDP : {gdp_text} Million
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
            state_contribution = yearly_gdp / first_gdp,
            state              = state[row],
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
            mutate(gdp_text         = scales::dollar(gdp)) %>%
            mutate(state_cont_percent = state_contribution %>% scales::percent(accuracy = 0.1)) %>%
            mutate(
                state_cont_perc_text  = str_glue(
                                        "State: {state}
                                         Rank : {rank}
                                         State GDP : {scales::dollar(gdp)} Million
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
