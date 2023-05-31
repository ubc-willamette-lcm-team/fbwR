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
#' by water year type in the period of record, including quantiles. 
#' @import lubridate
#' @import dplyr 
#' @export

summarizeFBW <- function(fish_passage_survival, param_list) {
  # Many parameters have to be summarized first by grouping into days of year
  # e.g., to match all January 1st's.
  summary <- fish_passage_survival %>%
      # R cannot handle year-less dates, so create a dummy column
      # for the sake of aggregation and summary
      dplyr::mutate(groupingDate = Date)
  # groupingDate is a "dummy" variable that allows us to group months
  #    together between years.
  # To do this, all dates in groupingDate have the same (arbitrary) year
  lubridate::year(summary$groupingDate) <- 2000
  # Start summarizing
  summary_revised <- summary %>%
    # dplyr lets you summarize by grouping variables:
    #   Here, group by month and "dummy" date
    dplyr::group_by(groupingDate, Month) %>%
    summarize(
      # Calculate hydrological information
      elev_P50 = median(elev, na.rm = T),
      flow_P50 = median(turb_flow + RO_flow + spill_flow + FPS_flow,
        na.rm = T),
      # Average flow rates
      qFPS_mean = mean(FPS_flow, na.rm = T),
      qTurb_mean = mean(turb_flow, na.rm = T),
      qRO_mean = mean(RO_flow, na.rm = T),
      qSpill_mean = mean(spill_flow, na.rm = T),
      # Average % fish distribution
      dailyMeanForebay_pop = mean(F.NoPass, na.rm = T),
      dailyMeanRO_pop = mean(F.RO, na.rm = T),
      dailyMeanTurb_pop = mean(F.turb, na.rm = T),
      dailyMeanSpill_pop = mean(F.spill, na.rm = T),
      dailyMeanFPS_pop = mean(F.FPS, na.rm = T),
      # Calculate the mean survival through each passage
      dailyMeanRO_surv = mean(passage_survRO, na.rm = T),
      dailyMeanTurb_surv = mean(passage_survTurb, na.rm = T),
      dailyMeanSpill_surv = mean(passage_survSpill, na.rm = T),
      dailyMeanFPS_surv = mean(passage_survFPS, na.rm = T)
    ) %>%
    dplyr::ungroup() %>% # Remove grouping, re-group to only Month
    dplyr::group_by(Month) %>%
    dplyr::summarise(
      # Exceedance monthly
      exceedance_wse_feet = mean(elev_P50),
      exceedance_flow_cfs = mean(flow_P50),
      # Average flow by route
      qFPS_mean = mean(qFPS_mean),
      qTurb_mean = mean(qTurb_mean),
      qRO_mean = mean(qRO_mean),
      qSpill_mean = mean(qSpill_mean),
      # Now add together daily means into monthly sums
      meanForebay_pop = sum(dailyMeanForebay_pop),
      meanRO_pop = sum(dailyMeanRO_pop),
      meanTurb_pop = sum(dailyMeanTurb_pop),
      meanSpill_pop = sum(dailyMeanSpill_pop),
      meanFPS_pop = sum(dailyMeanFPS_pop),
      # Average % fish distribution
      avgDPS_FPS = sum(dailyMeanFPS_surv),
      avgDPS_PH = sum(dailyMeanTurb_surv),
      avgDPS_RO = sum(dailyMeanRO_surv),
      avgDPS_Spill = sum(dailyMeanSpill_surv)
    )
    # Percent fish approaching
    summary_by_month <- left_join(
      x = summary_revised,
      y = param_list$monthly_runtiming %>% 
        mutate(Month = as.character(lubridate::month(Date, 
          label = TRUE, abbr = TRUE))) %>%
        select(-Date),
      by = "Month") %>%
      relocate(approaching_baseline, .after = exceedance_flow_cfs) %>%
      relocate(approaching_alternative, .after = approaching_baseline)
    # Summary by water year type - computed survival probabilities
    #   First, for all years in the period of record
    survprob <- fish_passage_survival %>%
      # Merge years and water year type data
      dplyr::mutate(year = as.character(
        lubridate::year(Date))) %>%
      left_join(y = param_list$water_year_types, by = "year") %>%
      dplyr::group_by(year, type) %>%
      dplyr::summarise(
        # Average % fish distribution
        fbw_surv = sum(passage_survRO, na.rm = TRUE) +
          sum(passage_survTurb, na.rm = TRUE) +
          sum(passage_survSpill, na.rm = TRUE) +
          sum(passage_survFPS, na.rm = TRUE)
      )
    # Broken into water year types
    survprob_wyt <- survprob %>%
      ungroup() %>%
      group_by(type) %>%
      summarize(
        avg_surv = mean(fbw_surv),
        p05_surv = quantile(fbw_surv, probs = 0.05),
        p10_surv = quantile(fbw_surv, probs = 0.10),
        p25_surv = quantile(fbw_surv, probs = 0.25),
        p50_surv = quantile(fbw_surv, probs = 0.50),
        p75_surv = quantile(fbw_surv, probs = 0.75),
        p90_surv = quantile(fbw_surv, probs = 0.90),
        p95_surv = quantile(fbw_surv, probs = 0.95)
      )
    # For ALL years in the POR (period of record)
    survprob_por <- survprob %>%
      ungroup() %>%
      summarize(
        avg_surv = mean(fbw_surv),
        p05_surv = quantile(fbw_surv, probs = 0.05),
        p10_surv = quantile(fbw_surv, probs = 0.10),
        p25_surv = quantile(fbw_surv, probs = 0.25),
        p50_surv = quantile(fbw_surv, probs = 0.50),
        p75_surv = quantile(fbw_surv, probs = 0.75),
        p90_surv = quantile(fbw_surv, probs = 0.90),
        p95_surv = quantile(fbw_surv, probs = 0.95)
      ) %>% 
      mutate(type = "Period of Record") %>%
      bind_rows(survprob_wyt) %>%
      relocate(type)
    return(list(
      monthly_summary = summary_by_month,
      wyt_surv_summary = survprob_por
    )
  )
}