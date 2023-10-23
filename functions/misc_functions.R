#Misc functions
library(tidyverse)

#Compute series of slopes within groups safely, returning 0 if can't calculate
compute_slope_or_zero <- function(data, ..., y, x) {
  
  groups <- quos(...) 
  y <- enquo(y)
  x <- enquo(x)

  #Function to compute slope
  get_slope <- function(data) {
    # model <- lm(data = data, formula = as.formula(paste0(!!y, " ~ ", !!x)))
    model <- lm(data = data, formula = as.formula(paste0(quo_name(y), " ~ ", quo_name(x))))
    coef(model)[2]
  }

  #Make it a safe function using purrr::possibly
  safe_get_slope <- possibly(get_slope, otherwise = 0)

  #Group and summarize
  data %>%
    group_by(!!!groups) %>%
    nest() %>%
    mutate(slope = map_dbl(data, safe_get_slope)) %>%
    select(-data)
  
}




#Create location quotients (and the regional and larger scale proportions needed to calculate it) and return attached to original dataframe
add_location_quotient_and_proportions <- function(df, regionvar, lq_var, valuevar){
  
  regionvar <- enquo(regionvar)
  lq_var <- enquo(lq_var)
  valuevar <- enquo(valuevar)
  
  df <- df %>%
    group_by(!!regionvar) %>% 
    mutate(
      region_totalsize = sum(!!valuevar, na.rm = T),#a. Current price per region per year, for regional denominator
      sector_regional_proportion = !!valuevar / region_totalsize#b. regional sector proportion (noting that a single row in this group is a single sector)
    ) %>% 
    group_by(!!lq_var) %>% 
    mutate(
      total_sectorsize = sum(!!valuevar, na.rm = T),#c. Summed current prices for EACH SECTOR, UK-wide
    ) %>% 
    ungroup() %>% 
    mutate(
      totalsize = sum(!!valuevar, na.rm = T),#d. Summed current prices for WHOLE UK per year, for UK denominator
      sector_total_proportion = total_sectorsize / totalsize#e. UK-level sector proportion
    ) %>% 
    mutate(
      LQ = sector_regional_proportion / sector_total_proportion#f. Location quotient!
    ) %>% 
    mutate(LQ_log = log(LQ)) 
  
  return(df)
  
}





#Make base plot for LQ plot, with option of setting alpha to zero if we don't want to see all other places
LQ_baseplot <- function(df, alpha = 0.1, sector_name, LQ_column, change_over_time){
  
  sector_name <- enquo(sector_name)
  LQ_column <- enquo(LQ_column)
  change_over_time <- enquo(change_over_time)
  
  p <- ggplot() +
  geom_point(
    data = df %>% filter(!!change_over_time > 0), 
    aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time),
    alpha = alpha,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = df %>% filter(!!change_over_time < 0), 
    aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * -1),
    alpha = alpha,
    shape = 16,
    colour = 'red'
  )  +
  scale_size_continuous(range = c(1,17)) +
  scale_x_continuous(trans = 'log10') +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F) +
  ylab("")
  
  return(p)

}
  

#For LQ change plots, overlay another place on the base plot
#It expects the following:
#dataframe containing a region and sector column, where the sector column is an ordered factor, ordered before it gets here
#a column with the LQ value
#a column with values of change over time showing growth or shrinkage
#Optional columns to include:
#a column with the raw value the LQ is based on, and a column containing the sector regional proportion from the LQ calculation (both appear in text if included)
#a column with min and max values to overlay as bars to indicate full range of the data
addplacename_to_LQplot <- function(df, plot_to_addto, placename, shapenumber=16, backgroundcolour='black', add_gva = F, setalpha = 1,
                                   region_name, sector_name,change_over_time, value_column, LQ_column, sector_regional_proportion,
                                   min_LQ_all_time,max_LQ_all_time){
  
  region_name <- enquo(region_name) 
  sector_name <- enquo(sector_name)
  change_over_time <- enquo(change_over_time)
  LQ_column <- enquo(LQ_column)
  
  plot_to_addto <- plot_to_addto +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time > 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time *1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time < 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time *-1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time > 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time),
      shape = shapenumber,
      colour = 'green',
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time < 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * -1),
      shape = shapenumber,
      colour = 'red',
      alpha = setalpha
    ) 
  
  #Test for one of these missing, don't display if so
  if(!(missing(value_column)|missing(sector_regional_proportion))){
    value_column <- enquo(value_column)
    sector_regional_proportion <- enquo(sector_regional_proportion)
    
    plot_to_addto <- plot_to_addto +  
      geom_text(
        data = df %>% filter(!!region_name == placename), 
        aes(y = !!sector_name, x = 20, label = paste0('£',value,'M, ',round(!!sector_regional_proportion * 100, 2),'%')),
        nudge_x = 0.3, hjust = 1, alpha = 0.7, size = 3
      )
    
    #Test for one of these missing, don't display if so
    if(!(missing(min_LQ_all_time)|missing(max_LQ_all_time)) ){
      
      min_LQ_all_time <- enquo(min_LQ_all_time)
      max_LQ_all_time <- enquo(max_LQ_all_time)
      
      plot_to_addto <- plot_to_addto +
        geom_errorbar(
          data = df %>% filter(!!region_name == placename),
          aes(y = !!sector_name, xmin = !!min_LQ_all_time, xmax = !!max_LQ_all_time),
          width = 0.1
        )
      
    }
    
    
  }
  
  return(plot_to_addto)
  
}






