#' Loads FBW data from a defined template spreadsheet in XLSX format. 
#' @param template_path File path to an Excel spreadsheet with standardized 
#' inputs. One of `template_path` or `param_list` must be provided; if using
#' a template, it is loaded and translated into a parameter list like 
#' `param_list`.
#' @param param_list Named list of FBW parameters created when the template
#' is read. This allows parameters to be modified or created manually without
#' having to create a separate template file. One of `template_path` or 
#' `param_list` must be provided.
#' @param ressim_path File path to an Excel spreadsheet with ResSim inputs.
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
  ressim_path = NULL, ressim = NULL, ressim_wide = TRUE,
  summarize = FALSE) {
  if (!is.null(ressim)) {
    message("...Using provided ResSim inputs")
    ressim_data <- ressim
  } else {
    message(paste0(
      "...Loading ResSim from file: ", basename(ressim_file)))
    ressim_data <- loadResSim(infile = ressim_file, wide = ressim_wide)
  }
  if (!is.null(param_list)) {
    message("...Using provided param_list inputs")
  } else {
    message(paste0(
      "...Loading parameters from template file: ", basename(ressim_file)))
    param_list <- loadFromTemplate(template_file = template_file)
  }
  # Distribute fish population into daily passing populations
  fish_daily_distribution <- distributeFishDaily(ressim_df,
    param_list = param_list)
  # Calculate DPE
  dpe <- fetchDPE(fish_daily_distribution, param_list = param_list)
  # Multiply approaching population by dam passage efficiency
  fish_dailydist$approaching_daily_postDPE <- dpe$dam_passage *
    fish_dailydist$approaching_dailyDistribution
  fish_distributed <- distributeFish_outlets(fish_postDPE = fish_dailydist,
    param_list = param_list)
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
      passage_survTurb = turb_survival * F.turb_flow,
      passage_survSpill = spill_survival * F.spill_flow,
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
        # Force all dates to have the same (arbitrary) year
        lubridate::year(summary$groupingDate) <- 2000
      summary <- summary %>%
        # dplyr lets you summarize by grouping variables:
        #   Here, group by month and "dummy" date
        dplyr::group_by(groupingDate, Month) %>%
        summarize(
          # Calculate the mean survival through each passage
          dailyMeanRO = mean(passage_survRO * 100, na.rm = T),
          dailyMeanTurb = mean(passage_survTurb * 100, na.rm = T),
          dailyMeanSpill = mean(passage_survSpill * 100, na.rm = T),
          dailyMeanFPS = mean(passage_survFPS * 100, na.rm = T)
        ) %>%
        dplyr::ungroup() %>% # Remove grouping, re-group to only Month
        dplyr::group_by(Month) %>%
        dplyr::summarise( 
          # Now add together daily means into monthly sums
          meanFPS = sum(dailyMeanFPS, na.rm = TRUE),
          meanTurb = sum(dailyMeanTurb, na.rm = TRUE),
          meanRO = sum(dailyMeanRO, na.rm = TRUE),
          meanSpill = sum(dailyMeanSpill, na.rm = TRUE)
        )
      return(summary)
    }
}
