#' Calculate distribution of flow through individual gates within each dam
#' outlet. This is useful in cases when a given dam outlet -- for example,
#' turbines -- have multiple sub-outlets through which a fish can pass the dam.
#' This function distributes flow between sub-outlets, and calculates survival
#' through those individual gates as a function of flow (or as a fixed value,
#' depending on parameterization).
#' @param fish_distributed_outlets A list including at least the following named
#'  objects:
#' route_dpe (a dataframe including columns `elev`, `baseline_dpe`, and any
#' number of other columns to the right of these that can be used to look up DPE
#' at various pool elevations).
#' @param param_list A list containing named entries for at least the
#' following:
#' `route specifications`, with columns: `parameter`, `Spill`, `RO`, `Turb`, and
#' `FPS`, and rows
#' rows
#' 1. `resv_data_sub$gate_methods`: a dataframe containing one column for each
#'   outlet type, with one row of data. Columns should include only:
#'   "ro_gatemethod", "turb_gatemethod", "spill_gatemethod", or
#'   "fp_gatemethod" (for the FPS).
#'   There should be one row in the dataframe, containing a character string
#'   that defines the method of distributing flow between gates (one of
#'   "Equal Q", "Min Q to equal", "Unit to Max Q", "Target Q", or
#'   "Peaking Performance").
#' 2. `ro_surv`, `turb_surv`, `spill_surv`, and `fp_surv`: each of these named
#'   objects should be a length-1 object that is either: 1) a fixed survival
#'   rate provided as a proportion surviving (e.g., 0.95 indicates 95% survival)
#'   or 2) the word "table", indicating that survival should be calculated as a
#'   function of flow (see `fish_distributed_outlets` for details on the
#'   survival table, as the flow-survival relationship is invariant to which
#'   alternative is being modelled,). If not provided, assumes survival rate is
#'   0.
#' 3. `rereg`: "Y" or "N", is there re-regulating mortality associated with the
#'   dam? Fish that pass through the FPS do not experience re-regulation
#'   mortality unless they pass through an FSO (in which case, re-regulating
#'   mortality is applied).
#' 4. If `rereg` is equal to "Y": `rereg_mort`, a single numeric value of the
#'   mortality rate associated with reregulation (i.e., 1 - survival).
#' @param resv_data Data on number of gates, as well as minimum and maximum flow
#' rates for each outlet type at the dam
#' @export
#'
#' @import dplyr

distributeFlow_Survival_gates <- function(fish_distributed_outlets, 
  param_list) {
  resv_data <- param_list$route_specs
  # Loop through the outlet types to determine flow and survival through each
  for (i in c("RO", "Turb", "Spill", "FPS")) {
    # keep track of current structure
    structure <- tolower(i)
    if (i == "FPS" && param_list$alt_desc[["collector"]] == "NONE") {
      message(paste0("No collector used, setting FPS survival to 0.\n"))
      fish_distributed_outlets <- fish_distributed_outlets %>%
        dplyr::mutate("{structure}_survival" := 0)
      next
    }
    # Select only the relevant reservoir data
    resv_data_sub <- resv_data[which(tolower(
      rownames(resv_data)) == structure), ]
    if (structure != "fps" && tolower(resv_data_sub$normally_used) == "n") {
      message(paste0("Route ", i,
        " not normally used, setting survival rate = 0."))
      fish_distributed_outlets <- fish_distributed_outlets %>%
        dplyr::mutate("{structure}_survival" := 0)
      next # Move to the next iteration of i
    }
    # structure_surv indicates a point value or "table"
    structure_surv <- resv_data_sub$passage_surv_rate
    message(paste0("\n...calculating survival for ", i, ": ", structure_surv))
    if (length(structure_surv) == 0) { # if this returns 0, skip
      warning(paste0("No survival rates provided for ", structure,
        ", assuming 0 and skipping."))
      fish_distributed_outlets <- fish_distributed_outlets %>%
        dplyr::mutate("{structure}_survival" := 0)
      next
    }
    # If there is a survival value given, and survival is NOT calculated 
    # according to a table, use that point value.
    if (!is.na(structure_surv) && tolower(structure_surv) != "table") {
      structure_surv <- as.numeric(structure_surv)
      # Create a new column based on the current structures' survival rate
      fish_distributed_outlets <- fish_distributed_outlets %>%
        dplyr::mutate("{structure}_survival" := structure_surv)
    } else if (is.na(structure_surv)) {
      warning(paste0("Survival rate through ", " is NA!!"))
      fish_distributed_outlets <- fish_distributed_outlets %>%
        dplyr::mutate("{structure}_survival" := NA)
    } else {
      ### USE TABLE BASED APPROACHES BELOW
      # If there is no gate method AND if there is more than 1 gate, stop 
      # Previously, this stopped execution at the head of the function
      if (resv_data_sub$n_gates > 1) {
        # Check that a method has been provided
        stopifnot(
          resv_data_sub$n_gates > 1 &
          resv_data_sub$method %in% c("Equal Q", "Min Q to equal", "Unit to Max Q",
            "Target Q", "Peaking Performance")
      )}
      # Pull out the survival by flow table
      surv_table <- data.frame(param_list[[paste0(structure, "_surv_table")]]) %>%
        # Coerce into numeric type
        dplyr::mutate(across(everything(), as.numeric))
      # Check if there is a gate method provided, if not quit.
      if (is.na(resv_data_sub$gate_method)) {
        stop(paste0("No gate method provided for ", structure))
      } else {
        message(paste0(".....gate method: ", resv_data_sub$gate_method))
      }
      # Check if it is multi-elev - this is typical for ROs
      # Are there >2 columns in the survival table? Yes -> multi-elev
      multielev <- ncol(surv_table) > 2
      if (multielev) {
        bottomElev <- as.numeric(param_list[[paste0(structure, "_elevs")]]$value)
        if (length(bottomElev) != ncol(surv_table) - 1) {
          stop(paste0(
            'The number of columns in the survival table does not match the number of elevations provided for ', structure, '.'
          ))
        }
        nElevs <- ncol(surv_table) - 1
        message(paste0(
          '...Assuming ', nElevs,
            ' outlets in ', structure, ' based on the number of columns in "', structure, '_surv_table" and entries in ', 
            paste0(structure, "_elevs"), '\n'))
        # First, create a linear interpolation of gateflow vs. survival table column 1; repeat for each column after column #2
        # Create an empty vector, interpolated_survivals, to fill in a loop
        survInterp_multi <- c()
        # Each entry of survInterp_multi contains a lookup function for one of
        #   the outlets in the multielev dam
        for (e in 1:nElevs) {
          # add a linear interpolation function to the survInterp_multi list
          survInterp_multi <- c(survInterp_multi, approxfun(
            x = unlist(surv_table[, 1]),
            y = unlist(surv_table[, e + 1]),
            rule = 2
          ))
        }
        # Save these variables for later
        elevXs <- fish_distributed_outlets$elev   # elevation
        # Create multielev table
        multelev_table <- as.data.frame(
          matrix(
            # Populate with all NAs
            NA,
            # One for each day of the series
            nrow = nrow(fish_distributed_outlets),
            # Number of columns: flow, elevation, flow-based survival upper, 
            #  flow-based survival lower
            ncol = ncol(surv_table) + 1
          )
        )
        colnames(multelev_table) <- c("elevs", "newFlow", paste0("survOutlet",
          1:nElevs))
      } else { # if not multiOutlet
        # The "nonflow" column will be the survival rate
        nonflow <- which(colnames(surv_table) != "flow")
        survLinearInterp <- approxfun(
          x = surv_table$flow,
          y = surv_table[ , nonflow],
          rule = 2
        )
      }
      # Flow distribution calculation
      # Select only the relevant flow data
      # use "^" to indicate beginning of line
      flowData_tmp <- data.frame(
        fish_distributed_outlets)[,
        # Lookup the flow through the current structure
          grep(paste0("^", tolower(structure), "_flow"),
          tolower(colnames(fish_distributed_outlets)))]
      # This should exclude any fish-bearing flow columns, which begin with B or
      #   pB
      gates <- resv_data_sub$n_gates
      #
      # Calculate survival based on gate method and flow
      #
      if (resv_data_sub$gate_method == "Equal Q") {
        # Create these variables for weighting later
        weighted_survival <- 0
        newflow <- flowData_tmp / gates
        for (g in 1:gates) {
          varname_flow <- paste0(structure, "_flow_gate", g)
          varname_surv <- paste0(structure, "_surv_gate", g)
          fish_distributed_outlets <- mutate(fish_distributed_outlets, 
            # Replace the flow data with newflow based on equal Q
            !!varname_flow := newflow)
          if (multielev) {
            multelev_table[,1] <- elevXs
            multelev_table[,2] <- newflow
            for (e in 1:nElevs) {
              # Here, look up the flow-based survival for both upper and lower
              # survYs <- c(survYs,survInterp_multi[[i]](newflow))
              multelev_table[, (e + 2)] <- survInterp_multi[[e]](newflow)
            }
            # Create a simple y=mx+b style lookup here
            # y = survival rates (high and low)
            # x = elevation of the day
            # interpolate y~x
            ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
            nearestSurv <- pmin(pmax(multelev_table[,3], multelev_table[,4]), 
            # if the extrapolated value is greater than
            #  the maximum survival, just use max. survival 
              multelev_table[, 3] +      # y1 equivalent
              ((multelev_table[, 4] - multelev_table[, 3] )* # y2-y1 range
              ((bottomElev[[1]] - multelev_table[, 1])/(bottomElev[[1]]-bottomElev[[2]])) 
              # (x1 - new X)/(x1-x2)
              )
            )
          } else { # if not multielev
            nearestSurv <- survLinearInterp(newflow[[varname_flow]])
          }
          fish_distributed_outlets <- mutate(fish_distributed_outlets, 
            !!varname_surv := nearestSurv)
          weighted_survival <- ifelse(
            is.na(nearestSurv),
            weighted_survival,
            # Track weighted survival between gates this way
            weighted_survival + (newflow * 
              (1 / ifelse(flowData_tmp == 0, Inf,
                flowData_tmp)) * nearestSurv)
          )
          # weighted_survival <- weighted_survival+(newflow*(1/ifelse(flowData_tmp==0,Inf,flowData_tmp))*nearestSurv)
        }
        fish_distributed_outlets <- fish_distributed_outlets %>% 
          dplyr::mutate("{structure}_survival" := weighted_survival)
          # mutate(flow_weighted_survival = weighted_survival)
      } else if (resv_data_sub$gate_method == "Min Q to equal") {
        min_flow <- resv_data_sub$min_flow
        if (is.na(min_flow)) {
          stop(paste0("No minimum flow rate set for ", structure, 
            ", but 'Min Q to equal' method selected. Exiting."))
        }
        weighted_survival <- 0
        # VBA revision here: treat the gates differently
        fish_distributed_outlets <- fish_distributed_outlets %>%
          dplyr::mutate(realized_gates = pmin(floor(
            flowData_tmp / min_flow), gates, na.rm = T))
        newflow <- flowData_tmp/fish_distributed_outlets$realized_gates
        if (multielev) {
          multelev_table[, 1] <- elevXs
          multelev_table[, 2] <- newflow
          for (e in 1:nElevs) {
            # Here, look up the flow-based survival for both upper and lower
            # survYs <- c(survYs,survInterp_multi[[i]](newflow))
            multelev_table[, e + 2] <- survInterp_multi[[e]](newflow)
          }
          # Create a simple y=mx+b style lookup here
          ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
          # y = survival rates (high and low)
          # x = elevation of the day
          # interpolate y~x
          nearestSurv <- pmin(pmax(multelev_table[,3], multelev_table[,4]),
          # if the extrapolated value is greater than
          #  the maximum survival, just use max. survival
            multelev_table[, 3] +      # y1 
            ((multelev_table[, 4] - multelev_table[, 3]) * # y2-y1 range
            ((bottomElev[[1]] - multelev_table[, 1]) / (bottomElev[[1]] -
              bottomElev[[2]]))
            )
          )
          fish_distributed_outlets <- fish_distributed_outlets %>%
            dplyr::mutate("{structure}_survival" := nearestSurv)
        } else {
          fish_distributed_outlets <- fish_distributed_outlets %>%
          dplyr::mutate(
            "{structure}_survival" := survLinearInterp(newflow)
          )
        }
      } else if (resv_data_sub$gate_method == "Unit to Max Q") {
        max_flow <- resv_data_sub$max_flow
        if (is.na(max_flow)) {
          stop(paste0("No maximum flow rate set for ", structure, 
            ", but 'Unit to Max Q' method selected. Exiting."))
        }
        weighted_survival <- 0
        # Have to count down with flow
        remaining_flow <- flowData_tmp
        for (g in 1:gates) {
          varname_flow <- paste0(structure, "_flow_gate", g)
          varname_surv <- paste0(structure, "_surv_gate", g)
          # Here, set all flows > max to the maximum value
          newflow <- pmin(data.frame(remaining_flow)[, 1], max_flow)
          # Create a new column in the output df
          fish_distributed_outlets <- dplyr::mutate(fish_distributed_outlets,
            !!varname_flow := newflow)
          if (multielev) {
            multelev_table[, 1] <- elevXs
            multelev_table[, 2] <- newflow
            for (e in 1:nElevs) {
              # Here, look up the flow-based survival for both upper and lower
              # survYs <- c(survYs,survInterp_multi[[i]](newflow))
              multelev_table[, e + 2] <- survInterp_multi[[e]](newflow)
            }
            nearestSurv <- pmin(pmax(multelev_table[, 3], multelev_table[, 4]),
              multelev_table[, 3] +      # y1 equivalent
              ((multelev_table[, 4] - multelev_table[, 3] ) * # y2-y1 range
              ((bottomElev[[1]] - multelev_table[,1]) / (bottomElev[[1]] -
                bottomElev[[2]])) # (x1 - new X)/(x1-x2)
              ))
          } else {
            nearestSurv <- survLinearInterp(newflow)
          }
          fish_distributed_outlets <- dplyr::mutate(fish_distributed_outlets, 
            !!varname_surv := nearestSurv)
          remaining_flow <- pmax((remaining_flow - max_flow), 0)
          weighted_survival <- ifelse(
            is.na(nearestSurv),
            weighted_survival,

            # Here, have to use an ifelse to avoid dividing by 0
            weighted_survival + (newflow * (1 / ifelse(
              flowData_tmp == 0, Inf, flowData_tmp)) * nearestSurv)

          )
        }
        fish_distributed_outlets <- fish_distributed_outlets %>%
          dplyr::mutate("{structure}_survival" := weighted_survival)
      } else if (resv_data_sub$gate_method == "Target Q") {
        # If outlet flow is greater than target*gates:
        target_flow <- resv_data_sub$target_flow
        if (is.na(target_flow)) {
          stop(paste0("No target flow set for ", structure, 
            ", but 'Target Q' method selected. Exiting."))
        }
        ngates <- resv_data_sub$n_gates
        fish_distributed_outlets <- fish_distributed_outlets %>%
          dplyr::mutate(
            flowData = flowData_tmp,
            realized_gates = dplyr::case_when(
              # If flow data is more than can be handled,
              # set to max number of gates
              flowData_tmp > (target_flow * ngates) ~ ngates,
              # Otherwise, if there are 2 gates, use only 1
              ### MDeith: This step is in the original VBA code for FBW; the 
              ### logic isn't clear to me. But when condition 1 is not met, 
              ### and the number of gates is 2, use 1 gate. 
              flowData_tmp <= (target_flow * ngates) & ngates == 2 ~ 1,
              # Otherwise, the catch-all final case (indicated with TRUE ~ )
              # is outlet flow divided by target flow
              TRUE ~ ceiling(flowData / target_flow)
            ),
            # If 0 gates are being used, flow=0
            # Else,
            gateflow = ifelse(realized_gates == 0, 0, flowData / realized_gates)
          )
          if (multielev) {
            multelev_table[, 1] <- elevXs
            multelev_table[, 2] <- newflow
            for (e in 1:nElevs) {
              # Here, look up the flow-based survival for both upper and lower
              # [[e]] indexes across the list of interpolation functions in
              #   survInterp_multi
              multelev_table[, e + 2] <- survInterp_multi[[e]](newflow)
            }
            # Create a simple y=mx+b style lookup here
            # y = survival rates (high and low)
            # x = elevation of the day
            # interpolate y~x
            ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
            fish_distributed_outlets <- fish_distributed_outlets %>%
              dplyr::mutate("{structure}_survival" :=
              pmin(pmax(multelev_table[, 3], multelev_table[, 4]),
              # if the extrapolated value is greater than
              #  the maximum survival, just use max. survival
                multelev_table[, 3] +      # y1 equivalent
                ((multelev_table[, 4] - multelev_table[,3] ) * # y2-y1 range
                ((bottomElev[[1]] - multelev_table[, 1]) / (bottomElev[[1]] -
                  bottomElev[[2]])) # (x1 - new X)/(x1-x2)
                )
              )
            )
          } else {
            fish_distributed_outlets <- fish_distributed_outlets %>%
                dplyr::mutate("{structure}_survival" := 
                  survLinearInterp(fish_distributed_outlets$gateflow))
          }
      } else if (resv_data_sub$gate_method == "Peaking Performance") {
        min_flow <- resv_data_sub$min_flow
        if (is.na(min_flow)) {
          warning(paste0("No minimum flow rate given for ", structure, 
            "; assuming no minimum flow rate (i.e., minimum = 0)."))
          min_flow <- 0
        }
        max_flow <- resv_data_sub$max_flow
        if (is.na(max_flow)) {
          warning(paste0("No maximum flow rate given for ", structure, 
            "; assuming no maximum flow rate (i.e., maximum = infinite)."))
          max_flow <- Inf
        }
        ngates <- resv_data_sub$n_gates
        if (min_flow == 0) {
          realized_gates <- ngates
        } else {
          realized_gates <- data.frame(floor(flowData_tmp / min_flow))
          # Where greater than ngates, make ngates
          for (rg in 1:ncol(realized_gates)) {
            realized_gates[which(realized_gates[, rg] > ngates), rg] <- ngates
          }
        }
        if (multielev) {
          multelev_table[, 1] <- elevXs
          mo_table_min <- multelev_table
          mo_table_min[, 2] <- min_flow
          for (e in 1:nElevs) {
            # Here, look up the flow-based survival for both upper and lower
            # outlets
            mo_table_min[, e + 2] <- survInterp_multi[[e]](min_flow)
          }
          # Create a simple y=mx+b style lookup here
          # y = survival rates (high and low)
          # x = elevation of the day
          # interpolate y~x
          ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
          survMinFlow <- pmin(pmax(mo_table_min[, 3], mo_table_min[, 4]), # if the extrapolated value is greater than
          #  the maximum survival, just use max. survival
            mo_table_min[, 3] +      # y1 equivalent
            ((mo_table_min[, 4] - mo_table_min[, 3]) * # y2-y1 range
            ((bottomElev[[1]] - mo_table_min[, 1]) / (bottomElev[[1]] -
              bottomElev[[2]])) # (x1 - new X)/(x1-x2)
            )
          )
        } else {
          survMinFlow <- survLinearInterp(min_flow)
        }
        message(".....Please wait...peaking performance takes a moment to calculate...")
        # flow_weighted_survival <- c()
        volLeft_o <- flowData_tmp * 24 # Convert to cfs per hour
        # Remove the minimum flow/gate * 24h * number of active gates
        volLeft <- volLeft_o - (realized_gates * min_flow * 24)
        # Create these empty vectors to fill up
        if (multielev) {
          weighted_percent <- data.frame(matrix(
            0,
            ncol = ncol(volLeft),
            nrow = nrow(volLeft)
          ))
        } else {
          # If not multielev, there is no weighting necessary
          weighted_percent <- c()
        }
        survivalP <- c()
        #!# could this be made faster? apply?
        for (h in 1:24) { # Have to do this hour by hour...YUCK!
          # print(h)
          for (g in 1:ngates) {
            # If multielev, there will be multiple columns for volLeft
            if (multielev) {
              # get the minimum of (max flow - min flow) and the current flow left
              # This is flow on top of the minimum, "AboveMin"
              flowAboveMin <- volLeft
              for (v in 1:ncol(volLeft)) {
                flowAboveMin[which(flowAboveMin[, v] >= (max_flow -
                  min_flow)), v] <- max_flow - min_flow
              }
              for (e in 1:nElevs) {
                multelev_table[, e + 2] <- survInterp_multi[[e]](min_flow +
                  flowAboveMin[, e])
              }
              survivalP <- pmin(pmax(multelev_table[, 3], multelev_table[, 4]),
                multelev_table[, 3] +      # y1 equivalent
                ((multelev_table[, 4] - multelev_table[, 3]) * # y2-y1 range
                ((bottomElev[[1]] - multelev_table[, 1]) / (bottomElev[[1]] -
                  bottomElev[[2]])) # (x1 - new X)/(x1-x2)
                )
              )
              survivalP[which(flowAboveMin[, 1] == 0)] <- 0
              for (e in 1:nElevs) {
                weighted_percent[, e] <- weighted_percent[, e] + 
                  (min_flow + flowAboveMin[, e]) * survivalP
                # How much is left?
                volLeft[, e] <- volLeft[, e] - flowAboveMin[, e]
              }
              volLeft[volLeft < 0] <- 0 # Turn negatives into 0
            } else { # if NOT multielev, such that volLeft has one column:
              weighted_percent <- c()
              survivalP <- c()
              flowAboveMin <- volLeft
              flowAboveMin[which(flowAboveMin >= (max_flow -
                min_flow))] <- max_flow - min_flow
              # Now use this to lookup
              survivalP <- survLinearInterp(min_flow + flowAboveMin)
              survivalP[which(flowAboveMin == 0)] <- 0
              weighted_percent <- weighted_percent + (min_flow + flowAboveMin) *
                survivalP
              volLeft <- pmin(volLeft - flowAboveMin, 0)
            }
          }
        }
        survivalPercent <- rowMeans(weighted_percent / flowData_tmp / 24)
        #Often these are identical, in my test
        # only 5/27,010 do not match
        # Include this to be rid of divide by zero issues
        survivalPercent[survivalPercent == Inf] <- 0
        fish_distributed_outlets <- fish_distributed_outlets %>%
          dplyr::mutate("{structure}_survival" := survivalPercent)
      }
    }
    # Account for reregulating morality, if applicable
    ### Depends on structure type and, if FPS, what kind of FPS.
    if (tolower(param_list$alt_desc[["rereg"]]) == "y" && i != "FPS") {
      fish_distributed_outlets <- fish_distributed_outlets %>%
        # Rename the current survival column to "pre_rereg" for clarity
        dplyr::rename(
          !!paste0(structure, "_survival_pre_rereg") := !! sym(paste0(structure,
            "_survival"))) %>%
        # Now apply rereg mortality
        dplyr::mutate("{structure}_survival" := 
          !! sym(paste0(structure, "_survival_pre_rereg")) * (1 - as.numeric(
        param_list$alt_desc[["rereg_mortality"]])))
    # Otherwise, if in the fish passage structure and it's the FSO type,
    #   apply this mortality
    } else if (tolower(param_list$alt_desc[["rereg"]]) == "y" && 
      i == "FPS" && param_list$alt_desc[["collector"]] == "FSO") {
      fish_distributed_outlets <- fish_distributed_outlets %>%
        # Rename the current survival column to "pre_rereg" for clarity
        dplyr::rename(
          !!paste0(structure, "_survival_pre_rereg") := !! sym(paste0(structure,
            "_survival"))) %>%
        # Now apply rereg mortality
        dplyr::mutate("{structure}_survival" := 
          !! sym(paste0(structure, "_survival_pre_rereg")) * (1 - as.numeric(
        param_list$alt_desc[["rereg_mortality"]])))
    } 
    # Otherwise leave the current survival column
  }
  return(fish_distributed_outlets)
}
