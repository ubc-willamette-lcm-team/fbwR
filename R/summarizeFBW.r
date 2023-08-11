#' Summarize daily FBW outputs into by-month and by-water-year-type summarize.
#' @param fish_passage_survival A dataframe created by runFBW that includes 
#' daily estimates of at least flow rates, survival rates, pool elevation,
#' and distribution of fish within the dam's routes. This can be created by 
#' runFBW() if summarize is set to FALSE.
#' @param param_list A list including at least the following named objects: 
#' `water_year_type`, including each year in the period of records and its 
#' water year type classification; and `monthly_runtiming`, a dataframe of 
#' the percent of fish approaching a dam in each month.
#' @return A list containing two objects: a summary by month, and a summary
#' by water year type in the period of record, including stats::quantiles.
#'  
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom dplyr relocate
#' @importFrom dplyr bind_rows
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats median
#' @importFrom rlang .data
#' @export

summarizeFBW <- function(fish_passage_survival, param_list) {
  param_list <- attributes(fish_passage_survival)$inputData$param_list
  ressim <- attributes(fish_passage_survival)$inputData$ressim
  # Many parameters have to be summarized first by grouping into days of year
  # e.g., to match all January 1st's.
  summary <- fish_passage_survival %>%
      # R cannot handle year-less dates, so create a dummy column
      # for the sake of aggregation and summary
      dplyr::mutate(groupingDate = .data$Date)
  # groupingDate is a "dummy" variable that allows us to group months
  #    together between years.
  # To do this, all dates in groupingDate have the same (arbitrary) year
  lubridate::year(summary$groupingDate) <- 2000
  # Start summarizing
  summary_revised <- summary %>%
    # dplyr lets you summarize by grouping variables:
    #   Here, group by month and "dummy" date
    dplyr::group_by(.data$groupingDate, .data$Month) %>%
    dplyr::summarize(
      # Calculate hydrological information
      elev_P50 = stats::median(.data$elev, na.rm = TRUE),
      flow_P50 = stats::median(.data$turb_flow + .data$RO_flow + .data$spill_flow,
        na.rm = TRUE),
      # Average flow rates
      qFPS_mean = mean(.data$FPS_flow, na.rm = TRUE),
      qTurb_mean = mean(.data$turb_flow, na.rm = TRUE),
      qRO_mean = mean(.data$RO_flow, na.rm = TRUE),
      qSpill_mean = mean(.data$spill_flow, na.rm = TRUE),
      # Average % fish distribution
      dailyMeanForebay_pop = mean(.data$F.NoPass, na.rm = TRUE),
      dailyMeanRO_pop = mean(.data$F.RO, na.rm = TRUE),
      dailyMeanTurb_pop = mean(.data$F.turb, na.rm = TRUE),
      dailyMeanSpill_pop = mean(.data$F.spill, na.rm = TRUE),
      dailyMeanFPS_pop = mean(.data$F.FPS, na.rm = TRUE),
      # Calculate the mean survival through each passage
      dailyMeanRO_surv = mean(.data$passage_survRO, na.rm = TRUE),
      dailyMeanTurb_surv = mean(.data$passage_survTurb, na.rm = TRUE),
      dailyMeanSpill_surv = mean(.data$passage_survSpill, na.rm = TRUE),
      dailyMeanFPS_surv = mean(.data$passage_survFPS, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>% # Remove grouping, re-group to only Month
    dplyr::group_by(.data$Month) %>%
    dplyr::summarize(
      # Exceedance monthly
      exceedance_wse_feet = mean(.data$elev_P50),
      exceedance_flow_cfs = mean(.data$flow_P50),
      # Average flow by route
      qFPS_mean = mean(.data$qFPS_mean),
      qTurb_mean = mean(.data$qTurb_mean),
      qRO_mean = mean(.data$qRO_mean),
      qSpill_mean = mean(.data$qSpill_mean),
      # Now add together daily means into monthly sums
      meanForebay_pop = sum(.data$dailyMeanForebay_pop),
      meanFPS_pop = sum(.data$dailyMeanFPS_pop),
      meanTurb_pop = sum(.data$dailyMeanTurb_pop),
      meanRO_pop = sum(.data$dailyMeanRO_pop),
      meanSpill_pop = sum(.data$dailyMeanSpill_pop),
      # Average % fish distribution
      avgDPS_FPS = sum(.data$dailyMeanFPS_surv),
      avgDPS_PH = sum(.data$dailyMeanTurb_surv),
      avgDPS_RO = sum(.data$dailyMeanRO_surv),
      avgDPS_Spill = sum(.data$dailyMeanSpill_surv)
    )
    # Percent fish approaching
    summary_by_month <- dplyr::left_join(
      x = summary_revised,
      y = param_list$monthly_runtiming %>%
        dplyr::mutate(Month = as.character(lubridate::month(.data$Date,
          label = TRUE, abbr = TRUE))) %>%
        dplyr::select(-.data$Date),
      by = "Month") %>%
      dplyr::relocate(.data$approaching_baseline,
        .after = .data$exceedance_flow_cfs) %>%
      dplyr::relocate(.data$approaching_alternative,
        .after = .data$approaching_baseline) %>%
      # Change the order of the months to match, start with Sept.
      dplyr::mutate(Month = factor(.data$Month, levels = c("Sep", "Oct", "Nov",
        "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"))) %>%
      dplyr::arrange(.data$Month)
    # Summary by water year type - computed survival probabilities
    #   First, for all years in the period of record
    survprob <- fish_passage_survival %>%
      # Merge years and water year type data
      dplyr::mutate(year = as.character(
        lubridate::year(.data$Date))) %>%
      dplyr::left_join(y = param_list$water_year_types, by = "year") %>%
      dplyr::group_by(.data$year, .data$type) %>%
      dplyr::summarize(
        # Average % fish distribution
        fbw_surv = sum(.data$passage_survRO, na.rm = TRUE) +
          sum(.data$passage_survTurb, na.rm = TRUE) +
          sum(.data$passage_survSpill, na.rm = TRUE) +
          sum(.data$passage_survFPS, na.rm = TRUE)
      )
    # Broken into water year types
    survprob_wyt <- survprob %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$type) %>%
      dplyr::summarize(
        avg_surv = mean(.data$fbw_surv),
        p05_surv = stats::quantile(.data$fbw_surv, probs = 0.05),
        p10_surv = stats::quantile(.data$fbw_surv, probs = 0.10),
        p25_surv = stats::quantile(.data$fbw_surv, probs = 0.25),
        p50_surv = stats::quantile(.data$fbw_surv, probs = 0.50),
        p75_surv = stats::quantile(.data$fbw_surv, probs = 0.75),
        p90_surv = stats::quantile(.data$fbw_surv, probs = 0.90),
        p95_surv = stats::quantile(.data$fbw_surv, probs = 0.95)
      )
    # For ALL years in the POR (period of record)
    survprob_por <- survprob %>%
      dplyr::ungroup() %>%
      dplyr::summarize(
        avg_surv = mean(.data$fbw_surv),
        p05_surv = stats::quantile(.data$fbw_surv, probs = 0.05),
        p10_surv = stats::quantile(.data$fbw_surv, probs = 0.10),
        p25_surv = stats::quantile(.data$fbw_surv, probs = 0.25),
        p50_surv = stats::quantile(.data$fbw_surv, probs = 0.50),
        p75_surv = stats::quantile(.data$fbw_surv, probs = 0.75),
        p90_surv = stats::quantile(.data$fbw_surv, probs = 0.90),
        p95_surv = stats::quantile(.data$fbw_surv, probs = 0.95)
      ) %>%
      dplyr::mutate(type = "Period of Record") %>%
      dplyr::bind_rows(survprob_wyt) %>%
      dplyr::relocate(.data$type)
    summarized_fbw <- list(
      monthly_summary = summary_by_month,
      wyt_surv_summary = survprob_por
    )
    attr(summarized_fbw, "inputData") <- list(
      param_list = param_list,
      ressim = ressim)
    return(summarized_fbw)
}
