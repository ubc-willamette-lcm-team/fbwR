#' Calculate FPS flow
#' @description First distributes fish bearing flow through available outlets in
#' a dam, including fish passage structure calculations. Then, using the 
#' proportion of fish-bearing flow, calculates the distribution of fish passing
#' the dam according to route effectiveness and fish-bearing flows.
#' @param fish_postDPE Dataframe containing daily estimates of various 
#' hydrological data (e.g., ResSim estimates of flow) in addition to the prop.
#' of annually approaching fish who are actually in the dam.
#' This dataframe must include a column, `approaching_daily_postDPE`, the daily
#' proportion of fish that pass the dam (after accounting for dam passage 
#' efficiency)
#' @param param_list A list including at least the following named objects: 
#'   `alt_desc`, with named entry "collector", the fish passage structure name
#'   `route_dpe`, a dataframe including columns `elev`, `baseline_dpe`, and any 
#' number of other columns to the right of these that can be used to look up DPE
#' at various pool elevations)
#'   `fps_max_elev`: a single numeric value, the maximum pool elevation at 
#'   which the fish passage structure can operate)
#'   `fps_bottom_elev`: a single numeric value, the minimum pool elevation (in
#'   feet) at which the fish passage structure can operate
#'   `dpe_x_position`: a single numeric value referencing which column of the 
#'   DPE lookup table (provided in `param_list`). Used to determine how many
#' which column of `route_dpe` AFTER the baseline should be selected?
#' @param verbose (Optional) Logical argument indicating whether 
#' intermediate columns (proportion of spill in each outlet: `pB.spill`, 
#' `pB.turb`, `pB.RO`, and `pB.FPS`; fish-bearing flow through each 
#' outlet: `B.spill`, `B.turb`, `B.RO`, and `B.FPS`; and route
#' effectiveness for each outlet: `RE.spill`, `RE.turb`, `RE.RO`, and
#' `RE.FPS`). Defaults to FALSE, in which case only the proportion of fish 
#' through each outlet is returned in the output dataframe.
#' 
#' @return A dataframe with all of the columns of the input table, 
#' `fish_postDPE`, with the addition of some number of columns (depending on the
#' `verbose` argument). At least the flow rate through the fish passage
#' structure, `FPS_flow`, and the proportion of fish passing through each 
#' outlet in the dam is returned. Optionally, if `verbose = TRUE`, 
#' intermediate variables like the proportion of flow through each outlet, route
#' effectiveness, and fish-bearing flow are output.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom lubridate day
#' @importFrom lubridate month
#' @importFrom lubridate year
#' 
#' @export

calcFPSFlow <- function(fps, fish_postDPE, param_list, verbose) {
  stopifnot(fps %in% c("NONE", "FSS", "FSC", "FISH WEIR", "FSO"))
  #!# PASTED FROM distributeFish_outlets.R function
  # Set min and max elevation for FPS
  fps_specs <- param_list$route_specs[
    which(rownames(param_list$route_specs) == "FPS"), ]
  if (fps != "NONE") { # calculate FPS_flow
    # Max. elevation applies only for the FPS, so it is defined in the alt desc.
    if (is.na(param_list$alt_desc[["fps_max_elev"]])) { # if no max:
      param_list$alt_desc[["fps_max_elev"]] <- Inf
    }
    # All other FPS related parameters are defined in route_specs
    if (is.na(fps_specs$bottom_elev)) {
      # if no min set parameter value to - inf
      fps_specs$bottom_elev <- -Inf
    }
    if (is.na(fps_specs$max_flow)) {
      # if no max. set to inf
      fps_specs$max_flow <- Inf
    }
    # Overwrite param_list values
    # Determine which elevations are within the upper and lower boundaries 
    fish_to_passFPS <- fish_postDPE %>%
      dplyr::mutate(
        # Binary indicator of whether elevation is sufficient
        adequate_elev = case_when(
          # Only accessible if above bottom_elev and above max - NON inclusive
          .data$elev > fps_specs$bottom_elev &
            .data$elev < param_list$alt_desc[["fps_max_elev"]] ~ 1,
          TRUE ~ 0
        )
      )
    if (fps == "FSS") {
      # It is assumed that FSS will screen water from upper part of forebay
      #   FSS may not be able to reach full capacity b/x of water being
      #   withdrawn for temperature control
      # FSO is assumed to operate regardless of temperature split.
      # If temperature controls are active, account for this:
      if (tolower(param_list$alt_desc[["use_temp_dist"]]) == "y") {
        # if(is.null(param_list$water_year_types) | is.null(temp_splits)){
        #   stop('If using an FSS fish passage with temperature control, you must also supply "year_water_types", a data frame of water type by year in the ResSim data; and "temp_splits", a dataframe containing columns date, proportion of water taken from the FSS during cool/wet water years, the proportion taken during normal years, and the proportion taken during hot/dry years. ')
        # }
        # If there is an FSS and temperature distribution is active, calc.
        #   the proportion of flow to be used for temperature control
        cat('...using temperature split, calculating...\n')
        temp_split <- calcTempSplit(fish_postDPE,
          water_year_types = param_list$water_year_types,
          temp_dist_df = param_list$temp_dist)
        fish_to_passFPS <- fish_to_passFPS %>%
        # To calculate the flow through the FPS:
        # First, 0 if elevation is inadequate (i.e., adequate_elev==0)
        # Take the minimum of qmax and FLOW-OUT for that day * (1-TEMP-SPLIT)
          dplyr::mutate(
            temp_split = as.numeric(temp_split$split),
            FPS_flow = .data$adequate_elev * pmin(fps_specs$max_flow,
            (.data$outflow_flow * (1 - temp_split))))
        if (!verbose) {
          fish_to_passFPS <- fish_to_passFPS %>%
            dplyr::select(-c(.data$adequate_elev, .data$temp_split))
        }
      } else {
        # otherwise: minimum of max flow and PH+RO flows (if elevation is adequate)
        fish_to_passFPS <- fish_to_passFPS %>%
          dplyr::mutate(
            temp_split = NA,
            FPS_flow = .data$adequate_elev *
            # Multiply the binary adequate_elev by the combined turb + RO flow
            pmin(fps_specs$max_flow, (.data$turb_flow + .data$RO_flow)))
        if (!verbose) {
          fish_to_passFPS <- fish_to_passFPS %>%
            dplyr::select(-c(.data$adequate_elev, .data$temp_split))
        }
      }
    } else if(fps == "FSC") {
      fish_to_passFPS <- fish_to_passFPS %>% 
        dplyr::mutate(
          temp_split = NA,
          # Maximum flow so long as elevation is enough
          FPS_flow = fps_specs$max_flow * .data$adequate_elev)
        if (!verbose) {
          fish_to_passFPS <- fish_to_passFPS %>%
            dplyr::select(-c(.data$adequate_elev, .data$temp_split))
        }
    } else if (fps == "FISH WEIR") {
      # First check that the weir date is appropriate - issue warning if dates missing
      if (all(dim(param_list$alt_desc[["weir_start_date"]]) == c(0, 0)) |
        all(dim(param_list$alt_desc[["weir_end_date"]]) == c(0, 0))) {
        warning("Weir start date and/or end date are missing, assuming weir active all year.")
        fish_to_passFPS <- fish_to_passFPS %>%
          dplyr::mutate(
            # weir_boolean indicates if the weir is active
            #   In this case, because no dates given, assume active
            temp_split = NA,
            weir_boolean = 1,
            # Perform a parallel minimum
            FPS_flow = pmin(fps_specs$max_flow, .data$spill) *
              .data$weir_boolean * .data$adequate_elev
          )
        if (!verbose) {
          fish_to_passFPS <- fish_to_passFPS %>%
            dplyr::select(-c(.data$adequate_elev, .data$weir_boolean,
              .data$temp_split))
        }
      } else {
        startdate <- tryCatch({
          # Enforce 2020 to ensure that leap year is included
          as.Date(paste0(param_list$alt_desc[["weir_start_date"]], "-2020"),
            "%d-%m-%Y")}, error = function(e) {
            stop("Weir start date must be a date in %d-%m format (e.g., 25-05 for the 25th of May)\n")
        })
        enddate <- tryCatch({
          as.Date(paste0(param_list$alt_desc[["weir_end_date"]], "-2020"),
            "%d-%m-%Y")}, error = function(e) {
          stop("Weir end date must be a date in %d-%m format (e.g., 25-05 for the 25th of May)\n")
        })
        # If the start date falls "after" the end date, push the start date backwards an extra year 
        if (lubridate::month(startdate) >= lubridate::month(enddate) ||
          (lubridate::day(startdate) > lubridate::day(enddate) &&
            lubridate::month(startdate) == lubridate::month(enddate))
          ) {
          lubridate::year(startdate) <- lubridate::year(startdate) - 1
        }
        # daily_weir is a sequence of dates during which the weir is active
        daily_weir <- seq(from = startdate, to = enddate, by = "day")
        # Convert to month-date format
        daily_weir_md <- paste0(lubridate::month(daily_weir), "-", 
          lubridate::day(daily_weir))
        # Add binary weir column
        fish_to_passFPS <- fish_to_passFPS %>%
          dplyr::mutate(
            MoDay = paste0(lubridate::month(.data$Date), "-", day(.data$Date)),
            # Here, weir_boolean = 1 if in the active date range, 0 if not
            weir_boolean = ifelse(.data$MoDay %in% daily_weir_md, 1, 0),
            # Take the minimum of qMax or spillway flow,
            # multiplied by weir date (0/1) and adequate elevation (0/1)
            FPS_flow = pmin(fps_specs$max_flow, .data$spill) *
              .data$weir_boolean * .data$adequate_elev
          )
        if (!verbose) {
          fish_to_passFPS <- fish_to_passFPS %>%
            dplyr::select(-c(.data$adequate_elev, .data$weir_boolean))
        }
      }
    } else if (fps == "FSO") {
      fish_to_passFPS <- fish_to_passFPS %>% 
        dplyr::mutate(
          FPS_flow = pmin(fps_specs$max_flow, .data$outflow_flow) *
            .data$adequate_elev)
        if (!verbose) {
          fish_to_passFPS <- fish_to_passFPS %>%
            dplyr::select(-c(.data$adequate_elev))
        }
    } else {
      stop('FPS must be one of: "NONE", "FSC", "FSS", "FSO", or "FISH WEIR"')
    }
  } else { # Finally, if it is "NONE", 0 flow
    fish_to_passFPS <- fish_postDPE %>%
      dplyr::mutate(FPS_flow = 0)
  }
  return(fish_to_passFPS %>% mutate(
    Q.Tot = .data$turb_flow + .data$spill_flow + .data$RO_flow
  ))
}