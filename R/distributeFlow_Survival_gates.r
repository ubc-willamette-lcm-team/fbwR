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
    if (i == "FPS" && param_list$alt_desc$collector == "NONE") {
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
    structure_surv <- param_list$alt_desc[[paste0(structure, "_surv")]]
    message(paste0("\n...calculating survival for ", i, ": ", structure_surv))
    if (length(structure_surv) == 0) { # if this returns 0, skip
      warning(paste0("No survival rates provided for ", structure,
        ", assuming 0 and skipping."))
      fish_distributed_outlets <- fish_distributed_outlets %>%
        dplyr::mutate("{structure}_survival" := 0)
      next
    }
    # If a point value, and numeric
    if (structure_surv != "table" && !is.na(structure_surv)) {
      structure_surv <- as.numeric(structure_surv)
      # If reregulating and if we are not currently in the fish passage,
      #   apply rergulating mortality in addition to the point value mortality
      #   provided in the quickset_data
      if (tolower(param_list$alt_desc[["rereg"]]) == "y" && i != "FPS") {
        survival <- structure_surv * (1 - as.numeric(
          param_list$alt_desc[["rereg_mortality"]]))
        # Otherwise, if in the fish passage structure:
      } else if(tolower(param_list$alt_desc[["rereg"]]) == "y" && 
        i == "FPS" && param_list$alt_desc[["collector"]] == "FSO"){
        survival <- structure_surv * (1 - as.numeric(
          param_list$alt_desc[["rereg_mortality"]]))
      } else { # Otherwise (if FP that is not FSO) or non-rereg cases:
        survival <- structure_surv
      }
      # Create a new column based on the current structures' survival rate
      fish_distributed_outlets <- fish_distributed_outlets %>%
        dplyr::mutate("{structure}_survival" := survival)
    } else { # here, use table-based approaches
      # Addition: only stop the function if there is no gate method AND if there 
      # is more than 1 gate. Previously, this stopped execution at the head of 
      # the function.
      if (resv_data_sub$n_gates > 1) {
        # Check that a method has been provided
        stopifnot(
          resv_data_sub$n_gates > 1 &
          resv_data_sub$method %in% c("Equal Q", "Min Q to equal", "Unit to Max Q",
            "Target Q", "Peaking Performance")
      )}
      # Pull out the survival by flow table
      surv_table <- data.frame(param_list[[paste0(structure, "_surv_table")]]) %>%
        dplyr::mutate(across(everything(), as.numeric))
      # Check if there is a gate method provided, if not quit.
      if (is.na(resv_data_sub$gate_method)) {
        stop("No gate method provided.")
      } 
      else {
        message(paste0(".....gate method: ", resv_data_sub$gate_method))
      }
      # Check if it is multi-outlet - this is typical for ROs
      # Are there 3+ columns in the survival table? Yes -> multi-outlet
      multioutlet <- ncol(surv_table) > 2
      if (multioutlet) {
        bottomElev <- as.numeric(param_list[[paste0(structure, "_elevs")]]$value)
        if (length(bottomElev) != ncol(surv_table) - 1) {
          stop(paste0(
            'The number of columns in the survival table does not match the number of elevations provided for ', structure, '.'
          ))
        }
        nOutlet <- ncol(surv_table) - 1
        message(paste0(
          '...Assuming ', nOutlet, 
            ' outlets to distribute through gates (based on the number of columns in the survival table and entries in ', 
            paste0(structure, "_elevs"), ') \n'))
        # First, create a linear interpolation of gateflow vs. survival table column 1; repeat for each column after column #2
        # Create an empty vector, interpolated_survivals, to fill in a loop
        survInterp_multi <- c()
        for(o in 1:nOutlet){
          # add a linear interpolation function to the survInterp_multi list
          survInterp_multi <- c(survInterp_multi, approxfun(
            x = unlist(surv_table[, 1]),
            y = unlist(surv_table[, o+1]),
            rule = 2
          ))
        }
        # Save these variables for later
        elevXs <- fish_distributed_outlets$elev   # elevation
        # Create multi-outlet table
        mo_table <- as.data.frame(
          matrix(
            NA,
            # One for each day of the series
            nrow = nrow(fish_distributed_outlets),
            # Number of columns: flow, elevation, flow-based survival upper, flow-based survival lower
            ncol = ncol(surv_table) + 1
          )
        )
        colnames(mo_table) <- c("elevs", "newFlow", paste0("survOutlet", 1:nOutlet))
      } else { # if not multiOutlet
        # The "nonflow" column will be the survival rate
        nonflow <- which(colnames(surv_table) != "flow")
        survLinearInterp <- approxfun(
          x = surv_table$flow,
          y = surv_table[ , nonflow],
          rule = 2
        )
      } # end of if multiOutlet() else linear
      # Flow distribution calculation
      # Select only the relevant flow data
      # use "^" to indicate beginning of line
      flowData_tmp <- data.frame(
        fish_distributed_outlets)[,
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
          if (multioutlet) {
            mo_table[,1] <- elevXs
            mo_table[,2] <- newflow
            for (o in 1:nOutlet) {
              # Here, look up the flow-based survival for both upper and lower
              # survYs <- c(survYs,survInterp_multi[[i]](newflow))
              mo_table[, (o + 2)] <- survInterp_multi[[o]](newflow)
            }
            # Create a simple y=mx+b style lookup here
            # y = survival rates (high and low)
            # x = elevation of the day
            # interpolate y~x
            ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
            nearestSurv <- pmin(pmax(mo_table[,3], mo_table[,4]), 
            # if the extrapolated value is greater than
            #  the maximum survival, just use max. survival 
              mo_table[,3] +      # y1 equivalent
              ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
              ((bottomElev[[1]] - mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) 
              # (x1 - new X)/(x1-x2)
              )
            )
          } else { # if not multioutlet
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
      } else if(resv_data_sub$gate_method == "Min Q to equal") {
        weighted_survival <- 0
        min_flow <- resv_data_sub$min_flow
        if (is.na(min_flow)) {
          stop(paste0("No minimum flow provided for outlet ", structure))
        }
        # VBA revision here: treat the gates differently
        fish_distributed_outlets <- fish_distributed_outlets %>%
          dplyr::mutate(realized_gates = pmin(floor(
            flowData_tmp / min_flow), gates, na.rm = T))
        newflow <- flowData_tmp/fish_distributed_outlets$realized_gates
        if (multioutlet) {
          mo_table[, 1] <- elevXs
          mo_table[, 2] <- newflow
          for (o in 1:nOutlet) {
            # Here, look up the flow-based survival for both upper and lower
            # survYs <- c(survYs,survInterp_multi[[i]](newflow))
            mo_table[, o+2] <- survInterp_multi[[o]](newflow)
          }
          # Create a simple y=mx+b style lookup here
          ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
          # y = survival rates (high and low)
          # x = elevation of the day
          # interpolate y~x
          nearestSurv <- pmin(pmax(mo_table[,3], mo_table[,4]),
          # if the extrapolated value is greater than
          #  the maximum survival, just use max. survival
            mo_table[, 3] +      # y1 
            ((mo_table[, 4] - mo_table[, 3]) * # y2-y1 range
            ((bottomElev[[1]] - mo_table[, 1]) / (bottomElev[[1]] -
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
      } else if(resv_data_sub$gate_method == "Unit to Max Q") {
        weighted_survival <- 0
        max_flow <- resv_data_sub$max_flow
        # Have to count down with flow
        remaining_flow <- flowData_tmp
        for (g in 1:gates) {
          varname_flow <- paste0(structure, "_flow_gate", g)
          varname_surv <- paste0(structure, "_surv_gate", g)
          # Here, set all flows > max to the maximum value
          ### This line is creating lists
          newflow <- pmin(data.frame(remaining_flow)[,1], max_flow)
          # Create a new column in the output df
          fish_distributed_outlets <- dplyr::mutate(fish_distributed_outlets,
            !!varname_flow := newflow)
          if (multioutlet) {
            mo_table[, 1] <- elevXs
            mo_table[, 2] <- newflow
            for (o in 1:nOutlet) {
              # Here, look up the flow-based survival for both upper and lower
              # survYs <- c(survYs,survInterp_multi[[i]](newflow))
              mo_table[, o + 2] <- survInterp_multi[[o]](newflow)
            }
            nearestSurv <- pmin(pmax(mo_table[, 3], mo_table[, 4]),
              mo_table[, 3] +      # y1 equivalent
              ((mo_table[, 4] - mo_table[, 3] ) * # y2-y1 range
              ((bottomElev[[1]] - mo_table[,1]) / (bottomElev[[1]] -
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
        ngates <- resv_data_sub$n_gates
        fish_distributed_outlets <- fish_distributed_outlets %>%
          dplyr::mutate(
            flowData = flowData_tmp,
            realized_gates = dplyr::case_when(
              # If flow data is more than can be handled,
              # set to max number of gates
              flowData_tmp > (target_flow * ngates) ~ ngates,
              # Otherwise, if there are two gates use only one
              ngates == 2 ~ 1,
              # Otherwise, the catch-all final case (indicated with TRUE ~ )
              # is outlet flow divided by target flow
              TRUE ~ ceiling(flowData / target_flow)
            ),
            # If 0 gates are being used, flow=0
            # Else,
            gateflow = ifelse(realized_gates == 0, 0, flowData / realized_gates)
          )
          if (multioutlet) {
            mo_table[, 1] <- elevXs
            mo_table[, 2] <- newflow
            for (o in 1:nOutlet) {
              # Here, look up the flow-based survival for both upper and lower
              # [[o]] indexes across the list of interpolation functions in
              #   survInterp_multi
              mo_table[, o + 2] <- survInterp_multi[[o]](newflow)
            }
            # Create a simple y=mx+b style lookup here
            # y = survival rates (high and low)
            # x = elevation of the day
            # interpolate y~x
            ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
            fish_distributed_outlets <- fish_distributed_outlets %>%
              dplyr::mutate("{structure}_survival" :=
              pmin(pmax(mo_table[, 3], mo_table[, 4]),
              # if the extrapolated value is greater than
              #  the maximum survival, just use max. survival
                mo_table[, 3] +      # y1 equivalent
                ((mo_table[, 4] - mo_table[,3] ) * # y2-y1 range
                ((bottomElev[[1]] - mo_table[, 1]) / (bottomElev[[1]] -
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
        max_flow <- resv_data_sub$max_flow
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
        if (multioutlet) {
          mo_table[, 1] <- elevXs
          mo_table_min <- mo_table
          mo_table_min[, 2] <- min_flow
          for (o in 1:nOutlet) {
            # Here, look up the flow-based survival for both upper and lower
            # outlets
            mo_table_min[, o + 2] <- survInterp_multi[[o]](min_flow)
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
        if (multioutlet) {
          weighted_percent <- data.frame(matrix(
            0,
            ncol = ncol(volLeft),
            nrow = nrow(volLeft)
          ))
        } else {
          # If not multi-outlet, there is no weighting necessary
          weighted_percent <- c()
        }
        survivalP <- c()
        #!# could this be made faster? apply?
        for (h in 1:24) { # Have to do this hour by hour...YUCK!
          # print(h)
          for (g in 1:ngates) {
            # If multioutlet, there will be multiple columns for volLeft
            if (multioutlet) {
              # get the minimum of (max flow - min flow) and the current flow left
              # This is flow on top of the minimum, "AboveMin"
              flowAboveMin <- volLeft
              for (v in 1:ncol(volLeft)) {
                flowAboveMin[which(flowAboveMin[, v] >= (max_flow -
                  min_flow)), v] <- max_flow - min_flow
              }
              for (o in 1:nOutlet) {
                mo_table[, o + 2] <- survInterp_multi[[o]](min_flow +
                  flowAboveMin[, o])
              }
              survivalP <- pmin(pmax(mo_table[, 3], mo_table[, 4]),
                mo_table[, 3] +      # y1 equivalent
                ((mo_table[, 4] - mo_table[, 3]) * # y2-y1 range
                ((bottomElev[[1]] - mo_table[, 1]) / (bottomElev[[1]] -
                  bottomElev[[2]])) # (x1 - new X)/(x1-x2)
                )
              )
              survivalP[which(flowAboveMin[, 1] == 0)] <- 0
              # volLeft <- volLeft-flowAboveMin
              # volLeft[volLeft < 0] <- 0
              # weighted_percent <- weighted_percent + (min_flow+flowAboveMin)*survivalP
              for (o in 1:nOutlet) {
                weighted_percent[, o] <- weighted_percent[, o] + (min_flow +
                  flowAboveMin[, o]) * survivalP
                volLeft[, o] <- volLeft[, o] - flowAboveMin[, o]
              }
              volLeft[volLeft < 0] <- 0 # Turn negatives into 0
            } else { # if NOT multioutlet, such that volLeft has one column:
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
    # save survival estimates
  }
  return(fish_distributed_outlets)
}
