#' Calculate distribution of fish through outlets
#' @description First distributes fish bearing flow through available outlets in
#' a dam, including fish passage structure calculations. Then, using the 
#' proportion of fish-bearing flow, calculates the distribution of fish passing
#' the dam according to route effectiveness and fish-bearing flows.
#' @param ressim_data Data frame of ResSim estimated flow rates for each day in
#' the period of record. Columns must include at least date and flow through the
#' spillway (spill), powerhouse (PH), and regulating outlet (RO_flow). 
#' @param quickset_data Data frame of information regarding the alternative
#' being modelled. This includes information on the type of FPS being modelled 
#' and its specifications. `quickset_data` expects entries with the 
#' following names: 
#' 1. `collector`, one of "noCollector" , "FSC", "FSS", "FSO", or 
#'   "FISH WEIR"; 
#' 2. if `collector` is not "noCollector": `fps_max_elev`, numeric, the maximum
#'   elevation at which the FPS operates (in the same unit as pool elevation 
#'   provided in ResSim, typically feet). If left empty, assumes no upper bound.
#' 3. if `collector` is not "noCollector": `fps_bottom_elev`, numeric, the 
#'   minimum elevation at which the FPS operates. If left empty (`numeric(0)`), 
#'   assumes no lower bound.
#' 4. if `collector` is not "noCollector": `fps_q_max`, numeric, the maximum 
#'   flow rate (in the same unit as provided in ResSim, typically cubic feet per
#'   second, cfs)
#' 5. if `collector` is not "noCollector": `fps_route_effectiveness`, numeric 
#'   vector with 11 entries. Each entry in `fps_route_effectiveness` corresponds
#'   to the route effectiveness multiplier of the FPS at a given ratio of flow 
#'   (aka. Q-ratio), the proportion of flow that passes through the FPS. Q-ratio
#'   values are not provided in `fps_route_effectiveness`; instead, only the 
#'   route effectiveness values that correspond to Q-ratios ranging from 0 to 1
#'   (increasing in 0.1 increments for a total of 11 Q-ratios and 11 
#'   corresponding FPS route effectiveness values). These are used to create
#'   a route effectiveness lookup table which is used to interpolate FPS
#'   route effectiveness in each day in the period of record.
#' 6. if `collector` is "FSS": `temp_dist`, character, "y" or "n". If an FSS is
#'   in place, should flow be diverted from the FSS to facilitate temperature 
#'   control operations?
#' 7. if `collector` is "FISH_WEIR": both `weir_start_date` and `weir_end_date`, 
#'   date class objects in the %d-%b format (e.g., 25-May), indicating the 
#'   Julian dates when weir operations begin and end. The weir is only assumed
#'   to be operational between these dates in all years of simulation. 
#'   If left blank, FBW assumes that the weir is active all year.
#' 8. if `collector` is "FISH_WEIR": `weir_end_date`
#' 
#' @param water_year_types A dataframe containing "year" and "watertype", one 
#' of "abundant", "adequate", "insufficient", and "deficient". Required only when the FPS being 
#' simulated includes temperature control operations (e.g., a floating screen
#' structure, "FSS").
#' @param temp_splits A dataframe containing the proportion of flow which will 
#' be redirected from the spillway to accomodate temperature control operations.
#' Temperature splits are typically provided month-by-month, with the proportion
#' of water being diverted from the spillway (i.e., "split") depending on the
#' water year type in each year ("C/W" for cool/wet years, "N" for normal years,
#' and "H/D" for hot/dry years). The water year type in each year is provided in
#' the `water_year_types` parameter.
#' @return A dataframe identical to the the input table, `ressim_data`, with the
#' additional of columns describing fish-bearing flow through each outlet 
#' (including the FPS, which is not calculated by ResSim), as well as the 
#' proportion of fish that pass through each outlet after accounting for route 
#' effectivness.
#' @import dplyr
#' @import lubridate 
#' @export
# fish_to_pass = subfdd; resv_data = resvdata; quickset_data=qsdata; water_year_types=water_year_types; temp_splits = temp_dist_df

distributeFish_outlets <- function(fish_to_pass, resv_data, quickset_data, 
    water_year_types = NULL, temp_splits = NULL){ # optional parameters if FSS=T and tempsplit needed
    # First, pull out important variables from the resv_data and quickset_data 
    #   objects
    fps <- as.character(quickset_data$collector) # what kind of FPS?
    if (fps != "noCollector") { # calculate qFPS
        # Set min and max elevation for FPS
        if (identical( quickset_data$fps_max_elev, numeric(0) ) ){ # if no max:
            quickset_data$fps_max_elev <- Inf
        }
        if (identical( quickset_data$fps_bottom_elev, numeric(0))) { # if no min:
            quickset_data$fps_bottom_elev <- -Inf        
        }
        if (identical( quickset_data$fps_q_max, numeric(0))){ # if no max:
            quickset_data$fps_q_max <- Inf
        }
        # Determine which elevations are within the upper and lower boundaries 
        fish_to_passFPS <- fish_to_pass %>% 
            mutate(
                # Binary indicator of whether elevation is sufficient
                adequate_elev = case_when(
                    elev >= quickset_data$fps_bottom_elev & elev <= quickset_data$fps_max_elev ~ 1, 
                    TRUE ~ 0
                )
            )
        if (fps == "FSS") {
            # It is assumed that FSS will screen water from upper part of forebay
            #   FSS may not be able to reach full capacity b/x of water being
            #   withdrawn for temperature control
            # FSO is assumed to operate regardless of tempearture split.
            # If temperature controls are active, account for this:
            if (tolower(quickset_data$temp_dist) == "y") {
                if(is.null(water_year_types) | is.null(temp_splits)){
                    stop('If using an FSS fish passage with temperature control, you must also supply "year_water_types", a data frame of water type by year in the ResSim data; and "temp_splits", a dataframe containing columns date, proportion of water taken from the FSS during cool/wet water years, the proportion taken during normal years, and the proportion taken during hot/dry years. ')
                }
                # If there is an FSS and temperature distribution is active, calc.
                #   the proportion of flow to be used for temperature control
                cat('...calculating temperature split...\n')
                temp_split <- calcTempSplit(fish_to_pass, water_year_types, temp_splits)$split
                fish_to_passFPS <- fish_to_passFPS %>%
                # To calculate the flow through the FPS:
                # First, 0 if elevation is inadequate (i.e., adequate_elev==0)
                # Take the minimum of qmax and FLOW-OUT for that day * (1-TEMP-SPLIT)
                    mutate(qFPS = adequate_elev * pmin(quickset_data$fps_q_max, 
                      (outflow_flow * (1 - temp_split)))) %>%
                    # "forget" the elevation column, no longer needed
                    select(-adequate_elev)
            } else {
                # otherwise: minimum of max flow and PH+RO flows (if elevation is adequate)
                fish_to_passFPS <- fish_to_passFPS %>%
                    mutate(qFPS = adequate_elev * pmin(quickset_data$fps_q_max, (turb_flow + RO_flow))) %>%
                    select(-adequate_elev)
            }
        } else if(fps == "FSC") {
            fish_to_passFPS <- fish_to_passFPS %>% 
                mutate(
                    # Maximum flow so long as elevation is enough
                    qFPS = quickset_data$fps_q_max * adequate_elev) %>%
                select(-adequate_elev)
        } else if(fps == "FISH WEIR") {
            # First check that the weir date is appropriate - issue warning if dates missing
            if(all(dim(quickset_data$weir_start_date)==c(0,0)) || all(dim(quickset_data$weir_end_date)==c(0,0))){
                warning("Weir start date and/or end date are missing, assuming weir active all year.")
                fish_to_passFPS <- fish_to_passFPS %>%
                    mutate(
                        # binaryWeir indicates if the weir is active
                        #   In this case, because no dates given, assume active
                        binaryWeir = 1,
                        # Perform a parallel minimum
                        qFPS = pmin(quickset_data$fps_q_max, spill)*binaryWeir*adequate_elev
                    ) %>% 
                    select(-c(adequate_elev, binaryWeir))
            } else {
                startdate <- tryCatch({
                    # Enforce 2020 to ensure that leap year is included
                    as.Date(paste0(quickset_data$weir_start_date, "-2020"), "%d-%b-%Y")
                }, error=function(e){
                    stop("Weir start date must be a date in %d-%b format (e.g., 25-May)\n")
                })
                enddate <- tryCatch({
                    as.Date(paste0(quickset_data$weir_end_date, "-2020"), "%d-%b-%Y")
                }, error=function(e){
                    stop("Weir end date must be a date in %d-%b format (e.g., 25-May)\n")
                })
                # If the start date falls "after" the end date, push the start date backwards an extra year 
                if(month(startdate)>=month(enddate) || (day(startdate)>day(enddate) && month(startdate)==month(enddate))){
                    year(startdate) <- year(startdate)-1
                }
                # daily_weir is a sequence of dates during which the weir is active
                daily_weir <- seq(from=startdate, to=enddate, by="day")
                # Convert to month-date format
                daily_weir_md <- paste0(month(daily_weir), "-", day(daily_weir))
                # Include 
                fish_to_passFPS <- fish_to_passFPS %>%
                    mutate(
                        MoDay = paste0(month(Date), "-", day(Date)),
                        # Here, binaryWeir = 1 if in the active date range, 0 if not
                        binaryWeir = ifelse(MoDay %in% daily_weir_md, 1, 0),
                        # Take the minimum of qMax or spillway flow, 
                        # multiplied by weir date (0/1) and adequate elevation (0/1)
                        qFPS = pmin(quickset_data$fps_q_max, spill)*binaryWeir*adequate_elev
                    ) %>% 
                    # Get rid of "extra" columns
                    select(-c(adequate_elev, binaryWeir))
            }
        } else if(fps == "FSO") {
            fish_to_passFPS <- fish_to_passFPS %>% 
                mutate(
                    qFPS = pmin(quickset_data$fps_q_max, outflow_flow) * adequate_elev)
                select(-adequate_elev)
        } else {
            stop('FPS must be one of: "noCollector", "FSC", "FSS", "FSO", or "FISH WEIR"')
        }
    } else { # Finally, if it is "noCollector", 0 flow
        fish_to_passFPS <- fish_to_pass %>%
            mutate(qFPS = 0) 
    }
    # Perform calculations based on which FPS was indicated in the function call,
    # Save outputs into intermediate dataframe fishBearingFlow with new columns 
    # for fish bearing flow (B.___) created using mutate()
    fish_to_passFPS <- fish_to_passFPS %>% 
        mutate(Q.Tot = turb_flow + spill_flow + RO_flow)
    fishBearingFlow <- switch(as.character(fps),
        "noCollector" = data.frame(fish_to_passFPS) %>% # If no collector, simple distribution
            mutate(
                B.spill_flow=spill_flow,
                B.turb_flow=turb_flow,
                B.RO_flow=RO_flow,
                B.FPS=0
            ),
        "FSC" = fish_to_passFPS %>% # If FSC, simply sum together B.total above and qFPS
        #   Here, if there is an FSC, the total attraction water is more than the
        #   outflow. It's dam + recirculating FSC water
            mutate(
                # This will preserve the total flow, which needs to be maintained
                #   in later fish survival calculations
                # These new steps from FBW Basic commands
                multiplier=(Q.Tot/(Q.Tot+qFPS)),
                B.spill_flow=spill_flow*multiplier,
                B.turb_flow=turb_flow*multiplier,
                B.RO_flow=RO_flow*multiplier,
                B.FPS=qFPS*multiplier
                # B.Total=B.turb_flow+B.RO_flow+B.spill_flow+B.FPS
            ),
        "FSS" = fish_to_passFPS %>% 
            # # For testing
            # filter(
            #     year(Date) == 2019,
            #     month(Date) == 6
            # ) %>%
        # Floating surface structure influences Turbine(PH) and reg. outlet (RO) flows
        #   Subract off the FSS flows from the total flows
        #   RO/PH will be proportioned based on the split between them
            mutate(
                fishPctRO = RO_flow/(turb_flow+RO_flow),
                B.spill_flow=spill_flow,
                B.turb_flow=pmax(0,turb_flow-(qFPS*(1-fishPctRO))),
                B.RO_flow=pmax(0,RO_flow-(qFPS*fishPctRO)),
                B.FPS=qFPS
                # B.Total=B.turb_flow+B.RO_flow+B.spill_flow+B.FPS
            ), #%>%
            #select(-fishPctRO), # Remove the extra column
        "FSO" = fish_to_passFPS %>% # The FSO collects flow needed for qFPS first from the spill, then the RO, then the PH
            mutate(
            ### NOTE: These are coded "out of order", but the spill - RO - PH order is preserved
                B.spill_flow=pmax(spill_flow - qFPS, 0), # First take from the spillway...
                B.turb_flow=pmax(turb_flow + 
                  pmin(RO_flow + pmin(spill_flow-qFPS,0),0),0), # any remaining flow from RO_flow and spill, otherwise the original value
                B.RO_flow=pmax(RO_flow + 
                  pmin(spill_flow - qFPS,0),0), # any remaining flow from spill, otherwise the original value
                B.FPS=qFPS
                # B.Total=B.turb_flow+B.RO_flow+B.spill_flow+B.FPS
            ) %>%
            # Actually remove flow from spill, RO, PH
            mutate(
                spill_flow=B.spill_flow,
                turb_flow = B.turb_flow, 
                RO_flow = B.RO_flow
            ),
        "FISH WEIR" = fish_to_passFPS %>% # If FISH WEIR, the FPS collects from the spillway, otherwise flows are the same
            mutate(
                B.spill_flow=spill_flow-qFPS,
                B.turb_flow=turb_flow,
                B.RO_flow=RO_flow, 
                B.FPS=qFPS
                # B.Total=B.turb_flow+B.RO_flow+B.spill_flow+B.FPS
            ) %>% 
            # Actually take away flow from the spillway with a FISH WEIR
            mutate(spill_flow = B.spill_flow)
    )
    # A final check, return a warning if FPS flow is higher than spillway flow. 
    if(fps=="FISH WEIR" & length(which(fishBearingFlow$B.spill_flow < 0))>0){
        warning("Some B.spill_flow values are <0 (this happens when you specify a 'FISH WEIR' FPS and the spill flow is less than qFPS.")
    }
    percentDist <- fishBearingFlow %>% 
    # First, calculate proportional flow
        mutate(
            p.spill_flow=B.spill_flow/Q.Tot,
            p.turb_flow=B.turb_flow/Q.Tot,
            p.RO_flow=B.RO_flow/Q.Tot,
            p.FPS=B.FPS/Q.Tot
        )
    # Now that fish-bearing flow is calculated, apply route effectiveness. 
    # It requires linear interpolation, so first create linear 
    # interpolation functions using the supplied Qratio columns
    #   (these are called later)
    ret <- data.frame(resv_data$route_effectiveness)
    if(all(dim(quickset_data$fps_route_effectiveness) == c(0,0))){
        fps_RElookup <- function(x){return(0)}
    } else {
        # If there are any values:
        fps_RElookup <- approxfun(
            x=seq(0, 1, by=0.1), 
            y=quickset_data$fps_route_effectiveness,
            rule=2
        )
    }
    spill_RElookup <- approxfun(
        x=ret$q_ratio, 
        y=ret$Spill,
        rule=2
    )
    RO_RElookup <- approxfun(
        x=ret$q_ratio, 
        y=ret$RO,
        rule=2
    )
    PH_RElookup <- approxfun(
        x=ret$q_ratio, 
        y=ret$Turb,
        rule=2
    )
    # Use these functions to lookup RE's and calculate adjusted total
    #   Note: This is termed Denom_Array in FBW VB code
    RETable <- percentDist %>%
        # Apply RE using the interpolation functions
        mutate(
            RE.spill_flow=spill_RElookup(p.spill_flow),
            RE.turb_flow=PH_RElookup(p.turb_flow),
            RE.RO=RO_RElookup(p.RO_flow),
            RE.FPS=fps_RElookup(p.FPS),
            adj.Total=((RE.spill_flow*p.spill_flow)+(RE.FPS*p.FPS)+(RE.RO*p.RO_flow)+(RE.turb_flow*p.turb_flow))
        )
    # Now, adjust for proportion of fish through each outlet
    #   PercentToPass calculations
    fishDist <- RETable %>% 
        # Then, adjust for proportion of fish through each outlet
        mutate(
            F.spill_flow=(approaching_daily_postDPE*RE.spill_flow*p.spill_flow)/adj.Total,
            F.turb_flow=(approaching_daily_postDPE*RE.turb_flow*p.turb_flow)/adj.Total,
            F.RO=(approaching_daily_postDPE*RE.RO*p.RO_flow)/adj.Total,
            F.FPS=(approaching_daily_postDPE*RE.FPS*p.FPS)/adj.Total
        ) %>%
        select(-c(adj.Total))
        # Remove these columns from the final output, they were just intermediates
        ### NOTE: Comment this out for debugging to check intermediate values
        # select(-c(p.spill_flow, p.turb_flow, p.RO_flow, p.FPS, 
        #    RE.spill_flow, RE.turb_flow, RE.RO, RE.FPS, adj.Total))
    # Incorporate nets if they are being used
    if(tolower(quickset_data$nets) == "y"){
        fishDist <- fishDist %>% 
            mutate(
                F.turb_flow = 0,
                F.RO = 0)
        if((resv_data$outlet_data$normally_used[which(resv_data$outlet_data$outlet == "Spill")])=="n"){
            # If the spillway is not normally used, fish are distributed
            #   through it first then the FPS
            fishDist <- fishDist %>% 
                mutate(
                    F.spill_flow = approaching_daily_postDPE*(RE.spill_flow*p.spill_flow)/adj.Total,
                    F.FPS = approaching_daily_postDPE-F.spill_flow
                )
        } else {
            fishDist <- fishDist %>%
                mutate(
                    F.spill_flow = 0,
                    F.FPS = approaching_daily_postDPE
                )
        }
    # Return the "fishDist" data frame
    }
    fishDist_out <- fishDist %>% 
        mutate(F.NoPass = approaching_dailyDistribution-approaching_daily_postDPE)
    return(fishDist_out)
}
