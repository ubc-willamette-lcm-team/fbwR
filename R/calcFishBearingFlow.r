#' Redistribute flows from ResSim according to calculated FPS flow
#' @description First distributes fish bearing flow through available outlets in
#' a dam, including fish passage structure calculations. Then, using the 
#' proportion of fish-bearing flow, calculates the distribution of fish passing
#' the dam according to route effectiveness and fish-bearing flows.
#' @inheritParams calcFPSFlow
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

calcFishBearingFlow <- function(fish_postDPE, param_list, verbose) {
  fps <- as.character(param_list$alt_desc[["collector"]])
  stopifnot(fps %in% c("NONE", "FSS", "FSC", "FISH WEIR", "FSO"))
  fish_to_passFPS <- calcFPSFlow(fish_postDPE = fish_postDPE, 
    param_list = param_list, verbose = verbose)
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