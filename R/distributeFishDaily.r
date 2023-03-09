#' Distribute fish approaching a dam in a given month into daily approaching
#' subpopulations.
#' @aliases distributeFish_daily
#' @param ressim_data A dataframe that includes at least Date (in datetime
#' format) and Outflow (cfs)
#' @param resv_data Named list of objects that include reservoir-specific data.
#' See the load_* functions for more details on what
#           parameters are expected. Here, only approaching data are needed.
#' @param quickset_data
#' @param method Distribution method; one of "flat" or "flow". In each month,
#' are the fish per day distributed uniformly ("flat", such that the same
#' proportion of the population approaches each day that month) or proportional
#' to flow ("flow")
#' @return Dataframe matching ressim_data but with new columns appended:
#' "fry_DailyDistribution", "sub_DailyDistribution", "year_DailyDistribution"
#' containing the calculated proportion of fry, sub-yearlings, and yearlngs
#' approaching the dam on each day
#' 
#' @import dplyr
#' @import lubridate
#' @export

distributeFishDaily <- function(ressim_data, param_list) {
  ressim_data_tmp <- ressim_data %>%
  # First, calculate days in month as DiM column
    mutate(DiM = lubridate::days_in_month(Date),
      # Extract month from flow data
      YrMo = paste0(lubridate::year(Date), "-", lubridate::month(Date)),
      Month = lubridate::month(Date, label = TRUE, abbr = TRUE)
    ) %>%
    # Then calculate year-month statistics (mean and total outflow)
    group_by(YrMo) %>%
    mutate(
      MonthlyQ_mean = mean(outflow_flow),
      MonthlyQ_total = sum(outflow_flow)
    ) %>%
    ungroup()
  # Assess which timing should be used - baseline or FPS-modified?
  if (param_list$alt_desc[["fps_alternative"]] == "Y") {
    fish_approaching <- data.frame(
      # Convert the date column into month only (instead of dyt format)
      Month = lubridate::month(param_list$monthly_runtiming$Date, 
        label = TRUE, abbr = TRUE),
      # Use the "alternative" approaching run timing
      approaching_month = param_list$monthly_runtiming$approaching_alternative)
  } else {
    fish_approaching <- data.frame(
      # Convert the date column into month only (instead of dyt format)
      Month = lubridate::month(param_list$monthly_runtiming$Date, 
        label = TRUE, abbr = TRUE),
      # Use the "baseline" approaching run timing
      approaching_month = param_list$monthly_runtiming$approaching_baseline)
  }
  # Join the run timing dataframe and above DF by the "Month" column
  ressim_data_runTiming <- left_join(ressim_data_tmp,
    fish_approaching, by = "Month")
  # Determine how the distribution will be done: flat or with flow
  dist_type <- ifelse(param_list$alt_desc[["fish_with_flow"]] == "Y", "flow", 
    "flat")
  if (dist_type == "flat") {
    # If flat, equal proportion across days in each month
    outDF <- ressim_data_runTiming %>%
      # Add new columns, prop_monthlyflow and approaching_daily
      mutate(
        prop_month = 1 / DiM)
  } else if (dist_type == "flow") {
    # If flow, calculate proportional to each day's proportion of monthly total
    # This is the method on page 22, not page 16 of FWB App K
    outDF <- ressim_data_runTiming %>%
      mutate(
        prop_month = outflow_flow / MonthlyQ_total)
  }
  outDF %>%
    mutate(approaching_dailyDistribution = approaching_month * prop_month) %>%
    select(-c(DiM, YrMo, prop_month, MonthlyQ_mean, MonthlyQ_total))
}
