#' Look up dam passage efficiency (DPE) for a given set of fish passage 
#' structures and pool elevation (and elevation limits)
#' 
#' @param ressim Data frame including at least the following column names:
#' elev (pool elevation, one per day in the period of record); 
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
#' @return A vector of DPE with one entry for each pool elevation provided.
#' 
#' @import dplyr
#' @import stats
#' @export

fetchDPE <- function(ressim, param_list) {
  # First, check to see what kind of DPE data we need (only baseline, FSS, etc.)
  fps_type <- param_list$alt_desc[["collector"]]
  # Isolate the FPS row from the route_spec object
  fps_specs <- param_list$route_specs[which(rownames(
    param_list$route_specs) == "FPS"), ]
  ### ~~~ To make uncertain: find a linear function that this is derived
  ###   from real-world data. There is presumably some function that 
  ###   underlies this:
  ###     DPE ~ elev + NOISE
  ###   so if we generate the NOISE randomly we can add random deviates 
  ###   (centred on 0)
  ### 
  #### First, create the baseline interpolator: this is like calling a function
  ####   which can be called later. Providing new X values generates new Y values
  ###baseline_linear_interpolator <- stats::approxfun(
  ###  x = param_list$route_dpe$elev,
  ###  y = param_list$route_dpe$baseline_dpe,
  ###  rule = 2)
  
  # Check for min/max elevation for the fish passage structure: 
  #   If the elevation is not right, the baseline DPE will be used.
  if (fps_type == "NONE" || is.na(param_list$alt_desc[["dpe_column_name"]]) || 
    identical(param_list$alt_desc[["dpe_column_name"]], character(0))) {
    elevmin_FPS <- Inf # By making the minimum elevation infinite, only the
    # baseline will be applied
    elevmax_FPS <- -Inf
  } else {
    # If max elevation for the FPS is blank/empty, set to -Inf
    elevmax_FPS <- ifelse(identical(param_list$alt_desc[["fps_max_elev"]], 
      numeric(0)) || is.na(param_list$alt_desc[["fps_max_elev"]]),
      Inf,
      as.numeric(param_list$alt_desc[["fps_max_elev"]])
    )
    # Same for min elevation
    elevmin_FPS <- ifelse(identical(fps_specs$bottom_elev, numeric(0)) || 
      is.na(fps_specs$bottom_elev),
      -Inf,
      as.numeric(fps_specs$bottom_elev)
    )
  }
  selected_dpe_col <- which(
    colnames(param_list$route_dpe) == param_list$alt_desc[["dpe_column_name"]])
  # selected_dpe_interpolator <- stats::approxfun(
  #     x = param_list$route_dpe$elev,
  #     y = unlist(param_list$route_dpe[, selected_dpe_col]),
  #     rule = 2)
  # baseline_dpe <- baseline_linear_interpolator(ressim$elev)
  # fps_dpe <- selected_dpe_interpolator(ressim$elev)
  ### Replace those instances
  fps_elev <- which(param_list$route_dpe$elev >= elevmin_FPS & 
    param_list$route_dpe$elev <= elevmax_FPS)
  dpes <- param_list$route_dpe$baseline_dpe
  dpes[fps_elev] <- param_list$route_dpe[, selected_dpe_col][fps_elev]
  combined_interpolator <- stats::approxfun(
    x = param_list$route_dpe$elev,
    y = dpes
  )
  # ressim <- suppressWarnings(ressim %>%
  #   dplyr::mutate(
  #     which_dpe = dplyr::case_when(
  #       (elev >= elevmin_FPS & elev <= elevmax_FPS) ~ "fps_dpe",
  #       # Otherwise (i.e., TRUE, a catch-all for the rest) use baseline
  #       TRUE ~ "baseline_dpe"
  #     ),
  #     dam_passage_efficiency = dplyr::case_when(
  #       # If within elevation boundaries, apply DPE
  #       (elev >= elevmin_FPS & elev <= elevmax_FPS) ~ fps_dpe,
  #       # Otherwise (i.e., TRUE, a catch-all for the rest) use baseline
  #       TRUE ~ baseline_dpe
  #     )
  #   ))
  ressim <- ressim %>%
    dplyr::mutate(
      dam_passage_efficiency = combined_interpolator(elev)
    )
  return(ressim)
}
