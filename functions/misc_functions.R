#Misc functions
library(dplyr)
library(purrr)
library(tidyr)

#Compute series of slopes within groups safely, returning 0 if can't calculate
compute_slope_or_zero <- function(data, ..., y, x) {
  
  groups <- quos(...)

  # Define the function to compute slope
  get_slope <- function(data) {
    model <- lm(data = data, formula = as.formula(paste0(y, " ~ ", x)))
    coef(model)[2]
  }

  # Make it a safe function using purrr::possibly
  safe_get_slope <- possibly(get_slope, otherwise = 0)

  # Use dplyr to group and summarize
  data %>%
    group_by(!!!groups) %>%
    nest() %>%
    mutate(slope = map_dbl(data, safe_get_slope)) %>%
    select(-data)
  
}