#' Distribute fish approaching a dam in a given month into daily approaching
#' subpopulations.
#' @aliases distributeFish_daily
#' @param ressim A dataframe that includes at least Date (in datetime
#' format) and Outflow (cfs)
#' @param param_list A named list of dam operation and biological parameters required to
#' run FBW. 
#' This list is typically created by reading in values from a template using the `loadFromTemplate` or 
#' `loadFromWorkbook` functions. <br> The list should include at least the following entries: 
#'   1. `alt_desc`, with named entry "collector", the fish passage structure name.
#'   2. `route_dpe`, a dataframe including columns `elev`, `baseline_dpe`, and any 
#' number of other columns to the right of these that can be used to look up DPE
#' at various pool elevations).
#'   3. `fps_max_elev`: a single numeric value, the maximum pool elevation at 
#'   which the fish passage structure can operate).
#'   4. `fps_bottom_elev`: a single numeric value, the minimum pool elevation (in
#'   feet) at which the fish passage structure can operate.
#'   5. `dpe_x_position`: a single numeric value referencing which column of the 
#'   DPE lookup table (provided in `param_list`). Used to determine how many
#'   columns of `route_dpe` AFTER the baseline should be selected?
#' @param verbose (for checking of outputs) If `FALSE`, the default value, the 
#' returned dataframe will not include intermediate calculations (e.g., monthly
#' flow rates). It may be helpful for debugging purposes to retain the columns
#' that contain these intermediate steps. Setting `verbose = TRUE` will return
#' all columns.
#' @return Dataframe matching ressim but with new columns appended:
#' "fry_DailyDistribution", "sub_DailyDistribution", "year_DailyDistribution"
#' containing the calculated proportion of fry, sub-yearlings, and yearlngs
#' approaching the dam on each day
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr case_when
#' @importFrom dplyr left_join
#' @importFrom lubridate days_in_month
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom rlang .data
#' 
#' @export

distributeFishDaily <- function(ressim, param_list, verbose = FALSE) {
  # First, calculate days in month as days_in_month column
  ressim_tmp <- dplyr::mutate(ressim,
    days_in_month = lubridate::days_in_month(.data$Date))
  leap_dates <- which(day(ressim$Date) == 29 & month(ressim$Date) == 2)
  # Skip leap years
  if (identical(leap_dates, integer(0))) {
    message("No instances of February 29 in the ResSim dataset, so leap years will be ignored. Only dates up to February 28th will be included in FBW.")
    ressim_tmp <- ressim_tmp %>%
      dplyr::mutate(days_in_month = dplyr::case_when(
          days_in_month == 29 ~ 28,
          TRUE ~ days_in_month))
  }
  ressim_tmp <- ressim_tmp %>%
    dplyr::mutate(
      YrMo = paste0(lubridate::year(.data$Date), "-",
        lubridate::month(.data$Date)),
      Month = lubridate::month(.data$Date, label = TRUE, abbr = TRUE)) %>%
    # Then calculate year-month statistics (mean and total outflow)
    dplyr::group_by(.data$YrMo) %>%
    # Q refers to flow rate through the dam
    dplyr::mutate(
      MonthlyQ_mean = mean(.data$outflow_flow),
      MonthlyQ_total = sum(.data$outflow_flow)
    ) %>%
    dplyr::ungroup()
  # Fish passage structures change the monthly run timing, so if there is
  #   an alternative in place (according to param_list$alt_desc), use the 
  #   alternative run timing
  if (param_list$alt_desc[["fp_alternative"]] == "Y") {
    fish_approaching <- data.frame(
      # Convert the date column into month only (instead of dyt format)
      Month = lubridate::month(param_list$monthly_runtiming[["Date"]],
      # Use an abbreviated label instead of month number (e.g., use "Feb"
      # instead of 2)
        label = TRUE, abbr = TRUE),
      # Because fp_alternative == "Yes", use the alternative column:
      approaching_monthly = param_list$monthly_runtiming$approaching_alternative)
  } else {
  # Baseline fish timing is used if there is no collector
    fish_approaching <- data.frame(
      Month = lubridate::month(param_list$monthly_runtiming$Date,
        label = TRUE, abbr = TRUE),
      # Because fp_alternative == "No", use the BASELINE approaching column:
      approaching_monthly = param_list$monthly_runtiming$approaching_baseline)
  }
  # Join the run timing dataframe and above DF by the "Month" column
  ressim_runTiming <- dplyr::left_join(ressim_tmp,
    fish_approaching, by = "Month")
  # Determine how the distribution will be done: flat or with flow
  dist_type <- ifelse(param_list$alt_desc[["fish_with_flow"]] == "Y", "flow", 
    "flat")
  if (dist_type == "flat") {
    # If flat, equal proportion across days in each month
    outDF <- ressim_runTiming %>%
      # Add new columns, prop_within_monthlyflow and approaching_daily
      dplyr::mutate(
        prop_within_month = 1 / .data$days_in_month)
  } else if (dist_type == "flow") {
    # If flow, calculate proportional to each day's proportion of monthly total
    # This is the method on page 22, not page 16 of FWB App K
    outDF <- ressim_runTiming %>%
      dplyr::mutate(
        prop_within_month = .data$outflow_flow / .data$MonthlyQ_total)
  }
  outDF <- outDF %>% dplyr::mutate(
    approaching_daily = .data$approaching_monthly * .data$prop_within_month)
  if (verbose) {
    return(outDF)
  } else {
    # If not verbose (i.e., not for testing), remove a bunch of columns 
    return(outDF %>%
      dplyr::select(-c(.data$days_in_month, .data$YrMo, .data$prop_within_month, 
        .data$MonthlyQ_mean, .data$MonthlyQ_total)))
  }
}
