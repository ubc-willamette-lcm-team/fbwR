#' Loads FBW data from a defined template spreadsheet in XLSX format.
#' @param template_file File path to an Excel spreadsheet with standardized
#' inputs. One of `template_path` or `param_list` must be provided; if using
#' a template, it is loaded and translated into a parameter list like
#' `param_list`.
#' @param param_list Named list of FBW parameters created when the template
#' is read. This allows parameters to be modified or created manually without
#' having to create a separate template file. One of `template_path` or
#' `param_list` must be provided.
#' @param ressim_file File path to an Excel spreadsheet with ResSim inputs.
#'  One of `ressim_path` or `ressim_file` must be provided.
#' @param ressim A loaded file (e.g., using the `loadResSim` function)
#' which may be provided instead of `ressim_path`. One of `ressim_path` or
#' `ressim_file` must be provided.
#' @param ressim_wide Are ResSim data in typical wide format? Defaults to TRUE,
#' which assumes that each column in the dataframe contains data for a single
#' year, with each row representing a single day in the 365-day year.
#' @param summarize Should the daily outputs be summarized into average monthly
#' survival estimates? This summarizes across years within the period of record
#' in the ResSim input file.
#' 
#' @import lubridate
#' @import dplyr
#' @export

runFBW <- function(template_file = NULL, param_list = NULL,
  ressim_file = NULL, ressim = NULL, ressim_wide = TRUE,
  summarize = FALSE, verbose = FALSE) {
  if (!is.null(ressim)) {
    message("...Using provided ResSim inputs")
  } else {
    message(paste0(
      "...Loading ResSim from file: ", basename(ressim_file)))
    ressim <- loadResSim(infile = ressim_file, wide = ressim_wide)
  }
  if (!is.null(param_list)) {
    message("...Using provided param_list inputs")
  } else {
    message(paste0(
      "...Loading parameters from template file: ", basename(template_file)))
    param_list <- loadFromTemplate(template_file = template_file)
  }
  # Distribute fish population into daily passing populations
  fish_daily <- data.frame(distributeFishDaily(ressim,
    param_list = param_list, verbose = verbose))
  # Calculate DPE
  dpe <- fetchDPE(fish_daily, param_list = param_list)
  # Multiply approaching population by dam passage efficiency
  fish_daily$approaching_daily_postDPE <- dpe$dam_passage *
    fish_daily$approaching_daily
  fish_distributed <- distributeFish_outlets(fish_postDPE = fish_daily,
    param_list = param_list, verbose = verbose)
  # Calculate survival rates from flow data, including distribution of fish 
  #   through gates in multi-gate outlets
  route_survival_rates <- distributeFlow_Survival_gates(
    fish_distributed_outlets = fish_distributed,
    param_list = param_list)
  # Perform final calculations, multiplying survival by the proportion of fish 
  #   in outlet X (F.X)
  fish_passage_survival <- route_survival_rates %>%
    dplyr::mutate(
      passage_survRO = ro_survival * F.RO,
      passage_survTurb = turb_survival * F.turb,
      passage_survSpill = spill_survival * F.spill,
      passage_survFPS = fps_survival * F.FPS,
      passage_survAllRoutes = passage_survRO + passage_survTurb +
        passage_survSpill + passage_survFPS
    )
    if (summarize == FALSE) {
      return(fish_passage_survival)
    } else {
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
}
