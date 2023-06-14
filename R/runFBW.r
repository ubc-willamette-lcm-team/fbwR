#' Run all steps of the FBW model, using only ResSim data and a biological/
#' dam operation template file.
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
#' @param verbose Should all intermediate columns be retained? If `TRUE`, every
#' intermediate step is included in the FBW output. Otherwise, only columns
#' required to run the main FBW functions are maintained.
#' 
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export

runFBW <- function(template_file = NULL, param_list = NULL,
  ressim_file = NULL, ressim = NULL, ressim_wide = TRUE,
  summarize = FALSE, verbose = FALSE) {
  if (!is.null(ressim)) {
    message("...Using provided ResSim inputs")
  } else {
    message(paste0(
      "...Loading ResSim from file: ", basename(ressim_file)))
    ressim <- fbwR::loadResSim(infile = ressim_file, wide = ressim_wide)
  }
  if (!is.null(param_list)) {
    message("...Using provided param_list inputs")
  } else {
    message(paste0(
      "...Loading parameters from template file: ", basename(template_file)))
    param_list <- fbwR::loadFromTemplate(template_file = template_file)
  }
  # Distribute fish population into daily passing populations
  fish_daily <- data.frame(fbwR::distributeFishDaily(ressim,
      param_list = param_list, verbose = verbose))
  # Calculate DPE
  fish_daily_postDPE <- dplyr::mutate(fish_daily,
      dpe = fbwR::fetchDPE(fish_daily,
        param_list = param_list)$dam_passage_efficiency,
      # Multiply approaching population by dam passage efficiency
      approaching_daily_postDPE = .data$approaching_daily * .data$dpe
    )
  fish_distributed <- fbwR::distributeFish_outlets(
    fish_postDPE = fish_daily_postDPE,
    param_list = param_list, verbose = verbose)
  # Calculate survival rates from flow data, including distribution of fish 
  #   through gates in multi-gate outlets
  route_survival_rates <- fbwR::distributeFlow_Survival_gates(
    fish_distributed_outlets = fish_distributed,
    param_list = param_list)
  # Perform final calculations, multiplying survival by the proportion of fish 
  #   in outlet X (F.X)
  fish_passage_survival <-  dplyr::mutate(route_survival_rates,
      passage_survRO = .data$ro_survival * .data$F.RO,
      passage_survTurb = .data$turb_survival * .data$F.turb,
      passage_survSpill = .data$spill_survival * .data$F.spill,
      passage_survFPS = .data$fps_survival * .data$F.FPS,
      passage_survAllRoutes = .data$passage_survRO + .data$passage_survTurb +
        .data$passage_survSpill + .data$passage_survFPS
    )
    if (summarize == FALSE) {
      return(fish_passage_survival)
    } else {
      return(summarizeFBW(fish_passage_survival, param_list))
    }
}
