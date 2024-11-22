#' Redistribute flows from ResSim according to calculated FPS flow
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

calcFishBearingFlow <- function(fps, fish_postDPE, param_list, verbose) {
  #!# Could add calcFPSFlow.R function as well
  #!# EVERYTHING BELOW HERE IS COPY-PASTED FROM THE distributeFish_outlets.R function
  #!# NEW FUNCTION
  fish_to_passFPS <- calcFPSFlow(fps, fish_postDPE, param_list)
  # "switch" provides different outcomes depending on the value of `fps`
  fishBearingFlow <- switch(as.character(fps),
    # If no collector, simple distribution
    "NONE" = data.frame(fish_to_passFPS) %>%
      dplyr::mutate(
        B.spill = .data$spill_flow,
        B.turb = .data$turb_flow,
        B.RO = .data$RO_flow,
        B.FPS = 0
      ),
    "FSC" = fish_to_passFPS %>% # If FSC, simply sum together B.total above and FPS_flow
    #   Here, if there is an FSC, the total attraction water is more than the
    #   outflow. It's dam + recirculating FSC water
      dplyr::mutate(
        # This will preserve the total flow, which needs to be maintained
        #   in later fish survival calculations
        # These new steps from FBW Basic commands
        multiplier = (.data$Q.Tot / (.data$Q.Tot + .data$FPS_flow)),
        B.spill = .data$spill_flow * .data$multiplier,
        B.turb = .data$turb_flow * .data$multiplier,
        B.RO = .data$RO_flow * .data$multiplier,
        B.FPS = .data$FPS_flow * .data$multiplier
        # B.Total=B.turb+B.RO+B.spill+B.FPS
      ),
    # If FSS:
    # Floating surface structure influences Turbine(PH) and reg. outlet (RO) flows
    #   Subract off the FSS flows from the total flows
    #   RO/PH will be proportioned based on the split between them
    "FSS" = fish_to_passFPS %>%
      dplyr::mutate(
        fishPctRO = .data$RO_flow / (.data$turb_flow + .data$RO_flow),
        B.spill = .data$spill_flow,
        B.turb = pmax(0, .data$turb_flow - (.data$FPS_flow * (1 - .data$fishPctRO))),
        B.RO = pmax(0, .data$RO_flow - (.data$FPS_flow * .data$fishPctRO)),
        B.FPS = .data$FPS_flow
      ) %>% 
      dplyr::select(-.data$fishPctRO),
    # The FSO collects flow needed for FPS_flow first from the spill, then the RO, then the PH
    "FSO" = fish_to_passFPS %>% #
      ### NOTE: These are coded "out of order", but the spill - RO - PH order is preserved
      dplyr::mutate(
        # First take from the spillway...
        B.spill = pmax(.data$spill_flow - .data$FPS_flow, 0),
        # any remaining flow from RO_flow and spill, otherwise the orig. value
        B.turb = pmax(.data$turb_flow +
          pmin(.data$RO_flow + pmin(.data$spill_flow - .data$FPS_flow, 0), 0),
            0), 
        B.RO = pmax(.data$RO_flow + pmin(.data$spill_flow - .data$FPS_flow, 0),
          0), # any remaining flow from spill, otherwise the original value
        B.FPS = .data$FPS_flow
      ) %>%
      # Remove flow from spill, RO, and powerhouse/turbine
      dplyr::mutate(
        spill_flow = .data$B.spill,
        turb_flow = .data$B.turb,
        RO_flow = .data$B.RO
      ),
    # If FISH WEIR, the FPS collects from the spillway, otherwise flows are the same
    "FISH WEIR" = fish_to_passFPS %>% 
      dplyr::mutate(
        B.spill = .data$spill_flow - .data$FPS_flow,
        B.turb = .data$turb_flow,
        B.RO = .data$RO_flow,
        B.FPS = .data$FPS_flow
      ) %>% 
      # Take away flow from the spillway with a FISH WEIR
      dplyr::mutate(spill_flow = .data$B.spill)
  )
  # A final check, return a warning if FPS flow is higher than spillway flow. 
  if (fps == "FISH WEIR" & length(which(fishBearingFlow$B.spill < 0)) > 0) {
    warning("Some B.spill values are <0 (this can happen when you specify a 'FISH WEIR' FPS and the spill flow is less than FPS_flow.")
  }
  #!# EVERYTHING ABOVE HERE IS COPY-PASTED
  return(fishBearingFlow)
}