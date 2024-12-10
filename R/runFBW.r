#' Run all steps of the FBW model using a HEC-ResSim run and biological parameters
#' 
#' @description
#' Using results from a HEC-ResSim hydrological model run and a set of biological
#' and dam operation parameters, `runFBW` performs all the steps of the dam passage
#' model. 
#' 
#' @details
#' FBW simulates dam passage of downstream migrants on a daily timestep. It requires
#' hydrological information be provided from a HEC-ResSim hydrological simulation model
#' run based on some dam operation rules. HEC-ResSim runs can represent status-quo dam 
#' management, or some operational alternative.
#' 
#' In addition to hydrological model results, FBW requires information about fish behaviour
#' and dam operating rules. Parameters are provided for a single "cohort" of downstream
#' migrants - included in a cohort are fish of approximately the same size, species, and fish 
#' passage behaviours. If there are multiple cohorts passing a dam, each could be parameterized
#' differently to reflect size- and species-specific passage behaviours and survival.
#' 
#' Briefly, the steps of the FBW model are: 
#' 1. From monthly run timing data, calculate the expected proportion of the cohort that
#' should pass in each day. The simulated period matches the period of record from the HEC-ResSim
#' model runs, and FBW simulates passage for each day included in the hydrological model outputs.
#' 2. In each day, calculate the proportion of fish which attempt to enter the dam but fail. 
#' Dam passage efficiency, DPE, is the proportion of fish which seek to enter the dam and succeed.
#' The 1-DPE component of the population is not modelled further with FBW (i.e. there are currently
#' no model processes reflecting in-reservoir survival and repeated attempts to pass after an
#' initial failure).
#' 3. Of those fish that successfully enter the dam, distribute them between available dam outlets. 
#' FBW is currently parameterized to include spillway gates, powerhouse turbines, regulating outlets, 
#' and several types of fish passage structures. Fish distribute between dam outlets according to
#' the distribution of flow and the relative attractiveness of each route.
#' 4. Once fish are distributed between outlets, calculate their survival (either as a point value
#' or as a function of flow through the outlet). In the case that there is a multi-gated outlet (e.g., 
#' a spillway with multiple gates or a powerhouse with multiple turbines), FBW accounts for how flow
#' distributes between gates according to dam operation rules. 
#' 5. (Optional) Summarize dam passage efficiency and route-specific survival from daily into annual 
#' passage metrics.
#' 
#' @param template_file Path to an Excel spreadsheet with standardized
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
#' @param verbose (Optional) Logical argument indicating whether to retain
#' intermediate calculation columns in the result. Intermediate calculations 
#' include the proportions of flow through each outlet, Defaults to `FALSE`.
#' 
#' @returns A dataframe which includes either daily estimates of fish passage and 
#' survival through dam outlets (if `summarize = FALSE`) or summaries of passage
#' after averaging across years in the period of record.
#' 
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export

runFBW <- function(template_file = NULL, param_list = NULL,
  ressim_file = NULL, ressim = NULL, ressim_wide = TRUE,
  summarize = FALSE, verbose = FALSE) {
  if (!(is.null(ressim))) {
    if (!(is.null(ressim)) & !(is.null(ressim_file))) {
      warning("Both `ressim` and `ressim_file` provided as arguments to runFBW(); using `ressim`")
    }
  } else if (!(is.null(ressim_file))) {
    message(paste0(
      "...Loading ResSim from file: ", basename(ressim_file)))
      ressim <- fbwR::loadResSim(infile = ressim_file, wide = ressim_wide)
  } else {
    stop("One of param_list or template_file must be provided to runFBW")
  }
  if (!is.null(param_list)) {
    if (!(is.null(template_file)) & !(is.null(param_list))) {
      warning("Both `param_list` and `template_file` provided as arguments to runFBW(); using `param_list`")
    }
  } else if (!(is.null(template_file))) {
    message(paste0(
      "...Loading parameters from template file: ", basename(template_file)))
    param_list <- fbwR::loadFromTemplate(template_file = template_file)
  } else {
    stop("One of param_list or template_file must be provided to runFBW")
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
    attr(fish_passage_survival, "param_list") <- param_list
    if (summarize == FALSE) {
      return(fish_passage_survival)
    } else {
      return(summarizeFBW(fish_passage_survival))
    }
}
