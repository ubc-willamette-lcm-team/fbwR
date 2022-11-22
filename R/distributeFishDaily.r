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

distributeFishDaily <- function(ressim_data, resv_data, quickset_data,
  type = "flat") {
    ressim_data_tmp <- ressim_data %>%
    # First, calculate days in month as DiM column
        dplyr::mutate(DiM = lubridate::days_in_month(Date),
           # Extract month from flow data
          YrMo = paste0(lubridate::year(Date), "-", lubridate::month(Date)),
          Month = lubridate::month(Date, label = TRUE, abbr = TRUE)
        ) %>%
        # Then calculate year-month statistics (mean and total outflow)
        dplyr::group_by(YrMo) %>%
        dplyr::mutate(MonthlyQ_mean = mean(outflow_flow),
               MonthlyQ_total = sum(outflow_flow)
        ) %>%
        dplyr::ungroup()
    # Assess which timing should be used - baseline or FPS-modified?
    if (quickset_data$fps_alternative == "Y") {
        fish_approaching <- quickset_data$quickset_fish_approaching
    } else {
        fish_approaching <- resv_data$baseline_fish_approaching
    }
    # Then, convert the date column into month only (instead of dyt format)
    fish_approaching$Date <- lubridate::month(fish_approaching$Date,
      label = TRUE, abbr = TRUE)
    colnames(fish_approaching) <- c("Month", "approaching")
    # Join the run timing dataframe and above DF by the "Month" column
    ressim_data_runTiming <- dplyr::left_join(ressim_data_tmp,
      fish_approaching, by = "Month")
    itercolumns <- colnames(fish_approaching %>% select(-c("Month")))
    # If flat, equal proportion across days in each month
    if (type == "flat") {
        outDF <- ressim_data_runTiming %>%
            mutate(prop_monthlyflow = 1 / DiM) %>%
            mutate(across(itercolumns), . * "prop_monthlyflow")
    # If flow, calculate proportional to each day's proportion of monthly total
    } else if (type == "flow" & !is.null(quickset_data)) {
        # This is the method on page 22, not page 16 of FWB App K
        outDF <- ressim_data_runTiming %>%
            mutate(prop_monthlyflow = outflow_flow / MonthlyQ_total) %>%
            # Using the names of columns above (itercolumns), multiply
            #   fish daily distribution by column value
            mutate(across(itercolumns, list(~prop_monthlyflow * .))) %>%
            rename_with(~ gsub("_1", "_dailyDistribution", .x))
    } else {
        stop("Error: type must be either 'flat' or 'flow'")
    }
    outDF %>%
        select(-c(DiM, YrMo, MonthlyQ_mean, MonthlyQ_total))
}