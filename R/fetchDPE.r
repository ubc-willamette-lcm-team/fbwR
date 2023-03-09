#' Look up dam passage efficiency (DPE) for a given set of fish passage 
#' structures and pool elevation (and elevation limits)
#' 
#' @param ressim_data Data frame including at least the following column names:
#' elev (pool elevation, one per day in the period of record); 
#' @param resv_data A list including at least the following named object: 
#' route_dpe (a dataframe including columns `elev`, `baseline_dpe`, and any 
#' number of other columns to the right of these that can be used to look up DPE
#' at various pool elevations) 
#' @param quickset_data A list including named entries for the following: 
#' 1. `fps_max_elev`: a single numeric value, the maximum pool elevation at 
#'   which the fish passage structure can operate)
#' 2. `fps_bottom_elev`: a single numeric value, the minimum pool elevation (in
#'   feet) at which the fish passage structure can operate
#' 3. `dpe_x_position`: a single numeric value referencing which column of the 
#'   DPE lookup table (provided in `resv_data`). Used to determine how many
#' which column of `route_dpe` AFTER the baseline should be selected?
#' @return A vector of DPE with one entry for each pool elevation provided.
#' 
#' @import dplyr
#' @importFrom stats, approxfun
#' @export

fetchDPE <- function(ressim_data, param_list){
    # First, check to see what kind of DPE data we need (only baseline, FSS, etc.)
    fps_type <- param_list$alt_desc[["collector"]]

    ### ~~~ To make uncertain: find a linear function that this is derived
    ###     from real-world data. There is presumably some function that 
    ###     underlies this: 
    ###       DPE ~ elev + NOISE
    ###     so if we generate the NOISE randomly we can add random deviates 
    ###     (centred on 0)
    ### 
    # First, create the baseline interpolator: this is like calling a function
    #   which can be called later. Providing new X values generates new Y values 
    baseline_linear_interpolator <- approxfun(x = param_list$route_dpe$elev, 
        y=resv_data$route_dpe$baseline_dpe, rule=2)
    # now, fill in other details based on quickset_data
    if (fps_type == "noCollector") {
        elevmin_FPS <- Inf # By making the minimum elevation infinite, only the 
        # baseline will be applied
        elevmax_FPS <- -Inf
    } else {
        # If max elevation for the FPS is blank/empty, set to -Inf
        elevmax_FPS <- ifelse(identical(quickset_data$fps_max_elev, numeric(0)), 
            elevmax_FPS <- Inf,
            elevmax_FPS <- quickset_data$fps_max_elev
        )
        elevmin_FPS <- ifelse(identical(quickset_data$fps_bottom_elev, numeric(0)), 
            elevmin_FPS <- -Inf,
            elevmin_FPS <- quickset_data$fps_bottom_elev
        )
    }
    selected_dpe_col <- resv_data$route_dpe[, quickset_data$dpe_x_position + 2]
    selected_dpe_interpolator <- approxfun(
            x = resv_data$route_dpe$elev, 
            y = selected_dpe_col,
            rule=2
        )
    baseline_dpe <- baseline_linear_interpolator(ressim_data$elev)
    fps_dpe <- selected_dpe_interpolator(ressim_data$elev)
    ressim_data <- ressim_data %>%
        mutate(
            dam_passage = case_when(
                # If within elevation boundaries, apply DPE
                elev >= elevmin_FPS & elev <= elevmax_FPS ~ fps_dpe,
                # Otherwise use baseline
                ### elev < elevmin_FPS & elev > elevmax_FPS ~ baseline_dpe,
                TRUE ~ baseline_dpe
            )
        )
    return(ressim_data)
}