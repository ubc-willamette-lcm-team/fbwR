#' Calculate distribution of fish through outlets
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
#'   1. `alt_desc`, with named entry "collector", the fish passage structure name; 
#'   2. `route_dpe`, a dataframe including columns `elev`, `baseline_dpe`, and any 
#' number of other columns to the right of these that can be used to look up DPE
#' at various pool elevations);
#'   3. `fps_max_elev`: a single numeric value, the maximum pool elevation at 
#'   which the fish passage structure can operate);
#'   4. `fps_bottom_elev`: a single numeric value, the minimum pool elevation (in
#'   feet) at which the fish passage structure can operate; 
#'   5. `dpe_column_name`: the name of the column in the `route_dpe` table that should
#'   be used to inform elevation-specific DPE values. This can be one of `baseline_dpe`
#'   or any other non-elevation columns in the `route_dpe` table. Typically, non-baseline
#'   DPE values are used when a fish passage structure is in place. When a non-baseline DPE
#'   column is used, it only applies if the pool elevation is within the operating 
#'   elevations of the passage structure.
#' @param verbose (Optional) Logical argument indicating whether to retain
#' intermediate columns in the output (proportion of spill in each outlet: `pB.spill`, 
#' `pB.turb`, `pB.RO`, and `pB.FPS`; fish-bearing flow through each 
#' outlet: `B.spill`, `B.turb`, `B.RO`, and `B.FPS`; and route
#' effectiveness for each outlet: `RE.spill`, `RE.turb`, `RE.RO`, and
#' `RE.FPS`). Defaults to `FALSE`, in which case only the proportion of fish 
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

distributeFish_outlets <- function(fish_postDPE, param_list, 
  verbose = FALSE) {
  ### STEP 1: FISH-BEARING FLOW - FPS FLOW
  ### Replace this step with resdistributeFlow
  # fps <- as.character(param_list$alt_desc[["collector"]]) # what kind of FPS?
  fishBearingFlow <- calcFishBearingFlow(fish_postDPE = fish_postDPE, 
    param_list = param_list, verbose)
  percentDist <- fishBearingFlow %>%
  # First, calculate proportional flow
    dplyr::mutate(
      pB.spill = .data$B.spill / .data$Q.Tot,
      pB.turb = .data$B.turb / .data$Q.Tot,
      pB.RO = .data$B.RO / .data$Q.Tot,
      pB.FPS = .data$B.FPS / .data$Q.Tot
    )
  # Now that fish-bearing flow is calculated, apply route effectiveness. 
  # It requires linear interpolation, so first create linear 
  # interpolation functions using the supplied Qratio columns
  #   (these are called later)
  ret <- data.frame(param_list$route_eff)
  if (all(is.na(ret$FPS)) | length(ret$FPS) == 0) {
    fps_RElookup <- function(x) return(0)
  } else {
    # If there are any values:
    fps_RElookup <- approxfun(
      x = ret$q_ratio,
      y = ret$FPS,
      rule = 2
    )
  }
  spill_RElookup <- approxfun(
    x = ret$q_ratio,
    y = ret$Spill,
    rule = 2
  )
  RO_RElookup <- approxfun(
    x = ret$q_ratio,
    y = ret$RO,
    rule = 2
  )
  PH_RElookup <- approxfun(
    x = ret$q_ratio,
    y = ret$Turb,
    rule = 2
  )
  # Use these functions to lookup RE's and calculate adjusted total
  #   Note: This is termed Denom_Array in FBW VB code
  RETable <- percentDist %>%
    # Apply RE using the interpolation functions
    dplyr::mutate(
      RE.spill = spill_RElookup(.data$pB.spill),
      RE.turb = PH_RElookup(.data$pB.turb),
      RE.RO = RO_RElookup(.data$pB.RO),
      RE.FPS = fps_RElookup(.data$pB.FPS),
      adj.Total = ((.data$RE.spill * .data$pB.spill) + 
        (.data$RE.FPS * .data$pB.FPS) +
        (.data$RE.RO * .data$pB.RO) + 
        (.data$RE.turb * .data$pB.turb))
    )
  # Now, adjust for proportion of fish through each outlet
  #   PercentToPass calculations
  fishDist <- RETable %>%
    # Then, adjust for proportion of fish through each outlet
    dplyr::mutate(
      F.spill = (.data$approaching_daily_postDPE * .data$RE.spill *
        .data$pB.spill) / .data$adj.Total,
      F.turb = (.data$approaching_daily_postDPE * .data$RE.turb *
        .data$pB.turb) / .data$adj.Total,
      F.RO = (.data$approaching_daily_postDPE * .data$RE.RO * .data$pB.RO) /
        .data$adj.Total,
      F.FPS = (.data$approaching_daily_postDPE * .data$RE.FPS *
        .data$pB.FPS) / .data$adj.Total
    )
    if (!verbose) {
      fishDist <- fishDist %>%
        dplyr::select(-c(.data$adj.Total))
    }
  # Incorporate nets if they are being used
  if(tolower(param_list$alt_desc[["nets"]]) == "y") {
    fishDist <- fishDist %>%
      dplyr::mutate(
        F.turb = 0,
        F.RO = 0)
    if (tolower(param_list$route_specs$normally_used[
      which(rownames(param_list$route_specs) == "Spill")]) == "n") {
      # If the spillway is not normally used, fish are distributed
      #   through it first then the FPS
      fishDist <- fishDist %>%
        dplyr::mutate(
          F.spill = .data$approaching_daily_postDPE *
            (.data$RE.spill * .data$pB.spill) / .data$adj.Total,
          F.FPS = .data$approaching_daily_postDPE - .data$F.spill
        )
    } else {
      fishDist <- fishDist %>%
        dplyr::mutate(
          F.spill = 0,
          F.FPS = .data$approaching_daily_postDPE
        )
    }
  }
  fishDist_out <- fishDist %>%
    dplyr::mutate(
      F.NoPass = .data$approaching_daily - .data$approaching_daily_postDPE)
  # Return the "fishDist" data frame depending on verbose output
  if (!verbose) {
    return(fishDist_out %>%
      dplyr::select(-c( # Remove the following
        .data$B.spill, .data$B.turb, .data$B.RO, .data$B.FPS,
        .data$pB.spill, .data$pB.turb, .data$pB.RO, .data$pB.FPS,
        .data$RE.spill, .data$RE.turb, .data$RE.RO, .data$RE.FPS
      )))
  } else {
    return(fishDist_out)
  }
}
