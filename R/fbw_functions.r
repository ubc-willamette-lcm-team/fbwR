

# FUNCTION calcTempSplit - internal function to calculate the distribution of 
#   flow between temperature control and the fish passage structure - 
#
#   INPUTS:
#   ressim_data : Data frame of flows vs. dates (used mainly to ID which dates are pertinent)
#   water_year_types : a dataframe containing "year" and "watertype", one of "abundant", "adequate", "insufficient", and "deficient"
#   temp_dist_df : a dataframe containing dated temperature splits (often month-by-month) 
#       based on year types: "C/W" (cool/wet), "N" (normal), and "H/D" (hot/dry) 
#
#   OUTPUTS:
#   tempSplitOut : a dataframe with two columns, "Date" and "split"

calcTempSplit <- function(ressim_data, water_year_types, temp_dist_df){
    # Create an output data frame
    tempSplitOut <- data.frame(
        Date = as.Date(ressim_data$Date, "%Y-%m-%d"),
        split = NaN
    )
    # Iterate over years
    for(i in 1:length(water_year_types$year)){
        yr <- water_year_types$year[i]
        # Identify year type and corresponding column in temp_dist_df
        type <- water_year_types$type[i]
        typeCol <- which(colnames(temp_dist_df)==type)
        # Find the lower bound for the temperature split data
        lower_date <- temp_dist_df$Date[1]
        # Coerce the year to be current (cannot be year-agnostic)
        lubridate::year(lower_date) <- yr
        origin_date <- lower_date # Make a copy for later
        # Iterate over these rows
        for(r in 2:nrow(temp_dist_df)){
            upper_date <- temp_dist_df$Date[r]-1
            lubridate::year(upper_date) <- yr
            date_interval <- lubridate::interval(lower_date, upper_date)
            tempSplitOut$split[which(tempSplitOut$Date %within% date_interval)] <- temp_dist_df[r-1,typeCol]
            lower_date <- upper_date+1
        }
        # Now, finish up with the final interval (lower_date to the original date of the next year)
        lubridate::year(origin_date) <- lubridate::year(origin_date)+1
        # New interval
        date_interval <- lubridate::interval(lower_date, origin_date)
        # Here, 'r' is saved from the iterator above and can be used at its highest value
        tempSplitOut$split[which(tempSplitOut$Date %within% date_interval)] <- temp_dist_df[r,typeCol]
    }
    tempSplitOut
}

# FUNCTION flowFPS - distribute flow through the fish passage structure
#
#   INPUTS:
#   ressim_data : Data frame of flows vs. dates. Columns must include at least date and flow 
#           through the spillway (spill), powerhouse (PH), reg. outlet (RO_flow), along with
#           tempSplit (split of flow)
#   FPS : What is the type of FPS? A character string, one of "FSC", "FSS", "FSO", or "FISH WEIR"
#   quickset_data$weir_start_date : if FPS="FISH WEIR", when should the weir be established? Use %d-%b format, eg. 25-May 
#   quickset_data$fps_q_max : What is the max. flow through the FPS? If not provided, assumed there is no max.
#   bottomElev : (from resvData) what is the bottom elevation for each outlet
#   tempRegulation : Boolean (TRUE/FALSE); should temperature splits be implemented?
#   tempSplitDF : Data frame; if tempRegulation is TRUE, a dataframe with two columns "date" and "split", 
#       created with the function calcTempSplit or otherwise supplied

flowFPS_old <- function(ressim_data, FPS, weirStartDate=NaN, weirEndDate=NaN, FPSQMax=Inf, FPSbottomElev=-Inf, FPStopElev=Inf, tempRegulation=FALSE, tempSplitDF=NULL){
    if(tempRegulation==TRUE){
        if(!("tempdist" %in% colnames(ressim_data))){
            # First, check to see if tempSplit is needed and verify that DF is provided
            if(is.null(tempSplitDF)){
                stop("If 'tempSplit' is used, you must also provide 'tempSplitDF' or a column 'tempdist' in the provided ressim_data")
            } else {
                ressim_data_tmp <- ressim_data %>% dplyr::left_join(x=ressim_data, y=tempSplitDF, by=c("Date"="date")) %>% 
                rename(tempSplit=split)
            }
        }
    } else {
        # If there is no temperature regulation, just make split=0
        ressim_data_tmp <- ressim_data %>% mutate(tempSplit=0)
    }
    # Weirs are more complicated, and may need to use year-agnostic interval manipulation/assignment as above
    if(FPS=="FISH WEIR"){
        # If no dates supplied, assume
        if(is.na(weirStartDate) || is.na(weirEndDate)){
            warning("weirStartDate and/or weirEndDate are missing, assuming weir active all year.")
            flowFishPass <- ressim_data_tmp %>%
            mutate(
                adequateElev=case_when(elev>FPSbottomElev ~ 1, TRUE~0),
                binaryWeir = 1,
                # Perform a parallel minimum
                qFPS = pmin(FPSQMax, spill)*binaryWeir*adequateElev
            )
        } else {
            startdate <- tryCatch({
                # Enforce 2020 to ensure that leap year is included
                as.Date(paste0(weirStartDate, "-2020"), "%d-%b-%Y")
            }, error=function(e){
                stop("Weir start date must be a date in %d-%b format (e.g., 25-May)\n")
            })
            enddate <- tryCatch({
                as.Date(paste0(weirEndDate, "-2020"), "%d-%b-%Y")
            }, error=function(e){
                stop("Weir end date must be a date in %d-%b format (e.g., 25-May)\n")
            })
            # If the start date falls "after" the end date, push the start date backwards an extra year 
            if(month(startdate)>=month(enddate) || (day(startdate)>day(enddate) && month(startdate)==month(enddate))){
                year(startdate) <- year(startdate)-1
            }
            # weir_interval <- lubridate::interval(startdate, enddate)
            daily_weir <- seq(from=startdate, to=enddate, by="day")
            daily_weir_md <- paste0(month(daily_weir), "-", day(daily_weir))
            flowFishPass <- ressim_data_tmp %>%
                mutate(
                    adequateElev=case_when(elev>FPSbottomElev ~ 1
                        # Otherwise
                        ,TRUE~0),
                    MoDay = paste0(month(Date), "-", day(Date)),
                    binaryWeir = ifelse(MoDay %in% daily_weir_md, 1, 0),
                    qFPS = pmin(FPSQMax, spill)*binaryWeir*adequateElev
                ) %>% 
                select(-c(adequateElev, binaryWeir))
        }
    } else if(FPS=="FSC"){
        # Following FBW VBA comments:
        #   if collector is FSC, the total attraction water
        #   is greater than the dam outflow
        # It also adds recirculating FSC water
        flowFishPass <- ressim_data_tmp %>% 
            mutate(
                # multiplier = outflow_flow+
                qFPS = ifelse(elev>FPSbottomElev, FPSQMax, 0)
            )
    } else if(FPS=="FSS"){
        flowFishPass <- ressim_data_tmp %>%
            mutate(
                adequateElev=case_when(elev>FPSbottomElev ~ 1
                    # Otherwise
                    ,TRUE~0)
            )
        if(tempRegulation==TRUE){
            flowFishPass <- flowFishPass %>%
                mutate(qFPS = adequateElev*pmin(FPSQMax, (Outflow*(1-split)))) %>%
                select(-adequateElev)
        } else {
            flowFishPass <- flowFishPass %>%
                mutate(qFPS = pmin(FPSQMax, turb_flow+RO_flow)) %>%
                select(-adequateElev)
        }
    } else if(FPS=="FSO"){
        flowFishPass <- ressim_data_tmp %>% 
            mutate(
                adequateElev=case_when((elev>FPSbottomElev && elev<FPStopElev) ~ 1
                    ,TRUE~0), 
                qFPS = adequateElev*pmin(FPSQMax, Outflow)
            ) %>%
            select(-adequateElev) 

    } else {
        stop('Fish passage structure (FPS) must be one of "noCollector", "FSC", "FSS", "FSO", or "weir"\n')
    }
    return(flowFishPass)
}


# FUNCTION distributeFish_outlets - Calculate distribution of fish through outlets
#
#   First distributes fish bearing flow through outlets, then calculates the percentage of
#   fish through all outlets according to the provided route effectiveness table. 
#
#   INPUTS:
#   ressim_data : Data frame of flows vs. dates. Columns must include at least date and flow 
#           through the spillway (spill), powerhouse (PH), reg. outlet (RO_flow),
#           fish passage structure (FPS), 
#   FPS : What is the type of FPS? A character string, one of "noCollector" [default],
#           "FSC", "FSS", "FSO", or "weir" 
#   qFPS : If any FPS is specified (i.e., other than "noCollector"), you must provide 
#           flow in cfs as a numeric value
#   RET : Route effectiveness table, as a data frame. Input from .csv here
#
#   OUTPUTS:
#   fishDist : A dataframe identical to the the input table, ressim_data, except for the 
#           addition of columns "B.spill", "B.turb_flow", "B.RO_flow", "B.FPS", and "B.Total",
#           (containing fish-bearing flows through the spillway, powerhouse, regulating 
#           outlet, FPS, and total fish bearing flow respectively), as well as "F.spill",
#           "F.turb_flow", etc. for proportion of fish (after accounting for route effectiveness).
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
                    mutate(qFPS = adequate_elev * pmin(quickset_data$fps_q_max, (outflow_flow*(1-temp_split)))) %>%
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
                B.spill_flow=pmax(spill_flow-qFPS, 0), # First take from the spillway...
                B.turb_flow=pmax(turb_flow+pmin(RO_flow+pmin(spill_flow-qFPS,0),0),0), # any remaining flow from RO_flow and spill, otherwise the original value
                B.RO_flow=pmax(RO_flow+pmin(spill_flow-qFPS,0),0), # any remaining flow from spill, otherwise the original value
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

# FUNCTION distributeFlow_gates - Calculate distribution of flow through outlet gates
#
#   Provided ResSim flows through alternative structures, calculates the gate-based flow
#       for each 
#
#   INPUTS:
#   ressim_data : a data frame (created by previous steps/functions here) including
#            at least 
#   multioutlet : a TRUE/FALSE conditional which indicates whether multiple outlets (e.g., RO) 
#            should be used. This changes how survival rates are calculated.
#   resvData : data on number of gates, min/max flow for each outlet type for each reservoir 
#   outlet_type : One of "RO", "Spill", or "Turb"; used to filter resvData
#   res : Reservoir name; used to filter resvData
#   method : one of "Equal Q", "Min Q to equal", "Unit to Max Q", "Target Q", or "Peaking Power"
#
#   OUTPUTS:
#   gate-based passage

# distributeFlow_Survival_gates_original <- function(ressim_data, quickset_data, resv_data){
#     ### These should be replaced with in-list parameters
#     # ressim_data, 
#     # multioutlet=FALSE, 
#     # bottomElev=0, 
#     # resvData=NULL, 
#     # survTable=NULL, 
#     # survValue=NULL, 
#     # outlet_type=NULL, 
#     # res=NULL, 
#     # method="Equal Q"){
#     method <- as.character(method) # ensure character type
#     # Check inputs meet expectations
#     if(!(method %in% c("Equal Q", "Min Q to equal", "Unit to Max Q", "Target Q", "Peaking Power"))){
#         stop('method must be one of: "Equal Q", "Min Q to equal", "Unit to Max Q", "Target Q", or "Peaking Power"')
#     }
#     if(!(outlet_type %in% c("RO", "Spill", "Turb"))){
#         stop('outlet_type must be one of: "RO", "Spill", or "Turb"')
#     }
#     if(!(res %in% unique(resvData$reservoir))){
#         stop(paste0('reservoir "', res, '" not found in resvData$reservoir. Options: \n',
#         paste0(unique(gates_maxflow$reservoir), '\n')
#         ))
#     }
#     if(!is.null(survTable)){
#         if(ncol(survTable)>2  && multioutlet==FALSE){
#             stop('survTable must only have two columns: "flow" and a column containing survival rates')
#         } else {
#             survType <- "table"
#         }
#     }
#     if(!is.null(survValue) & is.null(survTable)){
#         if(multioutlet==TRUE){
#             stop('If using multioutlet, multiple columns for survTable must be provided.')
#         } else {
#             survTable <- data.frame(flow=1, surv=survValue)
#             survType <- "point"
#         }
#     }
#     # First, address the simple case where it's a point estimate
#     if(survType == "point"){
#         # If a point value, then return that value and leave the function
#         ressim_data <- ressim_data %>% 
#             mutate(flow_weighted_survival = survValue)
#         return(ressim_data)
#     } else if(survType == "table"){
#         if(multioutlet){
#             if(length(bottomElev) != (ncol(survTable)-1)){
#                 stop('Number of survival lookup columns in survTable does not match the length of elevations provided in bottomElev.')
#             }
#             # The number of outlets is assumed to be the number of columns minus one
#             #   i.e. survTable is assumed to have 3 columns for systems with multiple ROs:
#             #   If there is an upper and a lower RO, columns should be: flow, lower RO flow-based survival, 
#             #   upper RO flow-based survival
#             nOutlet <- ncol(survTable)-1
#             cat(paste0('Assuming ', nOutlet, ' outlets to distribute through gates (based on number of columns in survTable) \n'))
#             # First, create a linear interpolation of gateflow vs. survival table column 1; repeat for each column after column #2
#             # Create an empty vector, interpolated_survivals, to fill in a loop
#             survInterp_multi <- c()
#             for(i in 1:nOutlet){
#                 # add a linear interpolation function to the survInterp_multi list
#                 survInterp_multi <- c(survInterp_multi, approxfun(
#                     x=survTable[,1], 
#                     y=survTable[,i+1],
#                     rule=2
#                 ))
#             }
#             # Save these variables for later
#             elevXs <- ressim_data$elev   # elevation
#             mo_table <- as.data.frame(
#                 matrix(
#                     NA, 
#                     # One for each day of the series
#                     nrow=nrow(ressim_data),
#                     # Number of columns: flow, elevation, flow-based survival upper, flow-based survival lower
#                     ncol=ncol(survTable)+1
#                 )
#             )
#             colnames(mo_table) <- c("elevs", "newFlow", paste0("survOutlet", 1:nOutlet))
#         } else { # if not multiOutlet
#             nonflow <- which(colnames(survTable)!="flow")
#             survLinearInterp <- approxfun(
#                 x=survTable$flow, 
#                 y=survTable[,nonflow],
#                 rule=2
#             )
#         } # end of if multiOutlet() else linear
#         # Flow distribution calculation
#         # Select only the relevant reservoir data
#         resvData_sub <- resvData %>% filter(outlet==outlet_type & reservoir==res)
#         # Select only the relevant flow data
#         #### if(outlet_type == "Turb"){
#         ####     outlet_type = "powerhouse"
#         #### }
#         flowData_tmp <- ressim_data[,grep(paste0(tolower(outlet_type), "_flow"), tolower(colnames(ressim_data)))]
#         gates <- resvData_sub$n_gates
#         # Perform calculations based on which method was indicated in the function call
#         if(method=="Equal Q"){
#             # Create these variables for weighting later
#             weighted_survival <- 0
#             for(g in 1:gates){
#                 varname_flow <- paste0(outlet_type, "_flow_gate", g)
#                 varname_surv <- paste0(outlet_type, "_surv_gate", g)
#                 newflow <- flowData_tmp/gates
#                 ressim_data <- mutate(ressim_data, !!varname_flow := newflow)
#                 if(multioutlet){
#                     mo_table[,1] <- elevXs
#                     mo_table[,2] <- newflow
#                     for(i in 1:nOutlet){
#                         # Here, look up the flow-based survival for both upper and lower
#                         # survYs <- c(survYs,survInterp_multi[[i]](newflow))
#                         mo_table[,i+2] <- survInterp_multi[[i]](newflow)
#                     }
#                     # Create a simple y=mx+b style lookup here
#                     # y = survival rates (high and low)
#                     # x = elevation of the day
#                     # interpolate y~x
#                     ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
#                     nearestSurv <- pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
#                     #    the maximum survival, just use max. survival 
#                         mo_table[,3] +          # y1 equivalent
#                         ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
#                         ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
#                         )
#                     )
#                 } else { # if not multioutlet
#                     nearestSurv <- survLinearInterp(newflow)
#                 }
#                 ressim_data <- mutate(ressim_data, !!varname_surv := nearestSurv)
#                 weighted_survival <- ifelse(
#                         is.na(nearestSurv),
#                         weighted_survival,
#                         weighted_survival+(newflow*(1/ifelse(flowData_tmp==0,Inf,flowData_tmp))*nearestSurv)
#                     )
#                 # weighted_survival <- weighted_survival+(newflow*(1/ifelse(flowData_tmp==0,Inf,flowData_tmp))*nearestSurv)
#             }
#             ressim_data <- ressim_data %>% 
#                 mutate(flow_weighted_survival = weighted_survival)
#         } else if(method=="Min Q to equal"){
#             weighted_survival <- 0
#             min_flow <- resvData_sub$minQ_cfs
#             if(is.na(min_flow)){
#                 stop("No minimum flow provided in resvData for outlet")
#             }
#             # VBA revision here: treat the gates differently
#             ressim_data <- ressim_data %>%
#                 mutate(
#                     # The number of gates is the maximum number that 
#                     # can be supplied with minimum flow
#                     # !!!
#                     # !!! CHANGE: Replaced RO flow with "flowData_tmp" - used to only be for RO? 
#                     realized_gates = pmin(floor(flowData_tmp/min_flow), gates, 1, na.rm=T),
#                     flowXgate = flowData_tmp/realized_gates,
#                 ) %>%
#                 select(-c(realized_gates, flowXgate))
#                 if(multioutlet){
#                     mo_table[,1] <- elevXs
#                     mo_table[,2] <- newflow
#                     for(i in 1:nOutlet){
#                         # Here, look up the flow-based survival for both upper and lower
#                         # survYs <- c(survYs,survInterp_multi[[i]](newflow))
#                         mo_table[,i+2] <- survInterp_multi[[i]](newflow)
#                     }
#                     # Create a simple y=mx+b style lookup here
#                     # y = survival rates (high and low)
#                     # x = elevation of the day
#                     # interpolate y~x
#                     ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
#                     nearestSurv <- pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
#                     #    the maximum survival, just use max. survival 
#                         mo_table[,3] +          # y1 equivalent
#                         ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
#                         ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
#                         )
#                     )
#                     ressim_data$weighted_survival <- nearestSurv
#                 } else {
#                     ressim_data <- ressim_data %>%
#                     muate(
#                         flow_weighted_survival = survLinearInterp(flowXgate)
#                     )
#                 }
#         } else if(method=="Unit to Max Q"){
#             weighted_survival <- 0
#             max_flow <- resvData_sub$maxQ_cfs
#             remaining_flow <- flowData_tmp
#             for(g in 1:gates){
#                 varname_flow <- paste0(outlet_type, "_flow_gate", g)
#                 varname_surv <- paste0(outlet_type, "_surv_gate", g)
#                 newflow <- ifelse(remaining_flow > max_flow, max_flow, remaining_flow)
#                 ressim_data <- mutate(ressim_data, !!varname_flow := newflow)
#                 if(multioutlet){
#                     mo_table[,1] <- elevXs
#                     mo_table[,2] <- newflow
#                     for(i in 1:nOutlet){
#                         # Here, look up the flow-based survival for both upper and lower
#                         # survYs <- c(survYs,survInterp_multi[[i]](newflow))
#                         mo_table[,i+2] <- survInterp_multi[[i]](newflow)
#                     }
#                     # Create a simple y=mx+b style lookup here
#                     # y = survival rates (high and low)
#                     # x = elevation of the day
#                     # interpolate y~x
#                     ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
#                     nearestSurv <- pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
#                     #    the maximum survival, just use max. survival 
#                         mo_table[,3] +          # y1 equivalent
#                         ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
#                         ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
#                         )
#                     )
#                 } else {
#                     nearestSurv <- survLinearInterp(newflow)
#                 }
#                 ressim_data <- mutate(ressim_data, !!varname_surv := nearestSurv)
#                 remaining_flow <- pmax((remaining_flow - max_flow), 0)
#                 weighted_survival <- weighted_survival+(newflow*(1/ifelse(flowData_tmp==0,Inf,flowData_tmp))*nearestSurv)
#             }
#             ressim_data <- ressim_data %>% 
#                 mutate(flow_weighted_survival = weighted_survival)
#         } else if(method=="Target Q"){
#             # If outlet flow is greater than target*gates:
#             target_flow <- resvData_sub$targetQ_cfs
#             ngates <- resvData_sub$n_gates
#             ressim_data <- ressim_data %>%
#                 mutate(
#                     flowData = flowData_tmp,
#                     realized_gates = case_when(
#                         flowData_tmp > (target_flow*ngates) ~ ngates, 
#                         ngates == 2 ~ 1,
#                         # The final case is indicated with "TRUE"
#                         TRUE ~ ceiling(flowData/target_flow)
#                     ),
#                     gateflow = flowData/realized_gates
#                 )
#                 #!# NEW
#                 newflow <- ressim_data$gateflow
#                 # gateflow = flowByGate
#                 if(multioutlet){
#                     mo_table[,1] <- elevXs
#                     mo_table[,2] <- newflow
#                     for(i in 1:nOutlet){
#                         # Here, look up the flow-based survival for both upper and lower
#                         # survYs <- c(survYs,survInterp_multi[[i]](newflow))
#                         mo_table[,i+2] <- survInterp_multi[[i]](newflow)
#                     }
#                     # Create a simple y=mx+b style lookup here
#                     # y = survival rates (high and low)
#                     # x = elevation of the day
#                     # interpolate y~x
#                     ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
#                     ressim_data$flow_weighted_survival <- pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
#                     #    the maximum survival, just use max. survival 
#                         mo_table[,3] +          # y1 equivalent
#                         ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
#                         ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
#                         )
#                     )
#                 } else {
#                     ressim_data$flow_weighted_survival <- survLinearInterp(ressim_data$gateflow)
#                 }
#         } else if(method=="Peaking Performance"){
#             min_flow <- resvData_sub$minQ_cfs
#             max_flow <- resvData_sub$maxQ_cfs
#             ngates <- resvData_sub$n_gates
#             if(min_flow==0){
#                 realized_gates <- ngates
#             } else {
#                 realized_gates <- data.frame(floor(flowData_tmp/min_flow))
#                 # Where greater than ngates, make ngates
#                 for(i in 1:ncol(realized_gates)){
#                     realized_gates[which(realized_gates[,i] > ngates),i] <- ngates
#                 }
#             }
#             if(multioutlet){
#                 mo_table[,1] <- elevXs
#                 mo_table_min <- mo_table
#                 mo_table_min[,2] <- min_flow
#                 for(i in 1:nOutlet){
#                     # Here, look up the flow-based survival for both upper and lower
#                     # survYs <- c(survYs,survInterp_multi[[i]](newflow))
#                     mo_table_min[,i+2] <- survInterp_multi[[i]](min_flow)
#                 }
#                 # Create a simple y=mx+b style lookup here
#                 # y = survival rates (high and low)
#                 # x = elevation of the day
#                 # interpolate y~x
#                 ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
#                 survMinFlow <- pmin(pmax(mo_table_min[,3], mo_table_min[,4]), # if the extrapolated value is greater than
#                 #    the maximum survival, just use max. survival 
#                     mo_table_min[,3] +          # y1 equivalent
#                     ((mo_table_min[,4] - mo_table_min[,3] )* # y2-y1 range
#                     ((bottomElev[[1]]-mo_table_min[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
#                     )
#                 )            
#             } else {
#                 survMinFlow <- survLinearInterp(min_flow)
#             }
#             print("Please wait..peaking performance takes some time to calculate")
#             # flow_weighted_survival <- c()
#             volLeft_o <- flowData_tmp*24 # Convert to cfs per hour
#             # Remove the minimum flow/gate * 24h * number of active gates
#             volLeft <- volLeft_o-(realized_gates*min_flow*24)
#             # Create these empty vectors to fill up
#             if(multioutlet){
#                 weighted_percent <- data.frame(matrix(
#                     0,
#                     ncol=ncol(volLeft), 
#                     nrow=nrow(volLeft) 
#                 ))
#             } else {
#                 weighted_percent <- c()
#             }
#             survivalP <- c()
#             for(h in 1:24){ # Have to do this hour by hour...YUCK!
#                 print(h)
#                 for(g in 1:ngates){
#                     # If multioutlet, there will be multiple columns for volLeft
#                     if(multioutlet){
#                         # get the minimum of (max flow - min flow) and the current flow left
#                         flowAboveMin <- volLeft 
#                         for(i in 1:ncol(volLeft)){
#                             flowAboveMin[which(flowAboveMin[,i] >= (max_flow-min_flow)),i] <- max_flow-min_flow
#                         }
#                         for(i in 1:nOutlet){
#                             mo_table[,i+2] <- survInterp_multi[[i]](min_flow+flowAboveMin[,i])
#                         }
#                         survivalP <- pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
#                             mo_table[,3] +          # y1 equivalent
#                             ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
#                             ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
#                             )
#                         )
#                         survivalP[which(flowAboveMin[,1]==0)] <- 0
#                         # volLeft <- volLeft-flowAboveMin
#                         # volLeft[volLeft < 0] <- 0
#                         # weighted_percent <- weighted_percent + (min_flow+flowAboveMin)*survivalP
#                         for(i in 1:nOutlet){
#                             weighted_percent[,i] <- weighted_percent[,i] + (min_flow+flowAboveMin[,i])*survivalP
#                             volLeft[,i] <- volLeft[,i] - flowAboveMin[,i]
#                         }
#                         volLeft[volLeft < 0] <- 0
#                     } else { # if NOT multioutlet, such that volLeft has one column:
#                         weighted_percent <- c()
#                         survivalP <- c()
#                         flowAboveMin <- volLeft
#                         flowAboveMin[which(flowAboveMin >= (max_flow-min_flow))] <- max_flow-min_flow
#                         # Now use this to lookup
#                         survivalP <- survLinearInterp(min_flow+flowAboveMin)
#                         survivalP[which(flowAboveMin==0)] <- 0
#                         weighted_percent <- weighted_percent + (min_flow+flowAboveMin)*survivalP
#                         volLeft <- pmin(volLeft - flowAboveMin, 0)
#                     }
#                 }
#             }
#             survivalPercent <- rowMeans(weighted_percent/flowData_tmp/24) #Often these are identical, in my test
#             # only 5/27,010 do not match
#             # Include this to be rid of divide by zero issues
#             survivalPercent[survivalPercent==Inf] <- 0
#             ressim_data$flow_weighted_survival <- survivalPercent
#         }
#     }# if survType=="table"
#     # Finally, if any elevations are below the minimum elevation of the outlet, survival is automatically 0 
#     # This is a code bug which has been forwarded from the VBA code
#     ressim_data$flow_weighted_survival[which(ressim_data$elev < min(unlist(bottomElev)))] <- 0
#     return(ressim_data)
# }


# ressim_data = fish_distributed; ### ressim_df; 
# quickset_data=qsdata; resv_data=resvdata

distributeFlow_Survival_gates <- function(ressim_data, quickset_data, resv_data){
    # Loop through the outlet types to determine flow and survival through each
    for(i in c("RO", "Turb", "Spill", "FP")){
        # keep track of current structure
        structure <- ifelse(i=="FP", "fps", tolower(i))
        # Select only the relevant reservoir data
        resvData_sub <- resv_data$outlet_data %>% filter(tolower(outlet)==structure)
        if(structure != "fps" && tolower(resvData_sub$normally_used)=="n"){
            cat(paste0("! Route ", structure, " not normally used, setting survival rate = 0.\n"))
            ressim_data <- ressim_data %>%
                mutate("{structure}_survival" := 0)
            next
        }
        cat(paste0("\n...calculating survival for ", structure, "\n"))
        # structure_surv indicates a point value or "table"
        structure_surv <- quickset_data[which(names(quickset_data) == paste0(structure, "_surv"))][[1]]
        if (length(structure_surv) == 0) { # if this returns 0, skip
            cat(paste0("! No survival rates provided for ", structure, ", assuming 0 and skipping."))
            ressim_data <- ressim_data %>%
                mutate("{structure}_survival" := 0)
            next
        }
        # If a point value, and numeric
        if (structure_surv != "table" & is.numeric(structure_surv)) {
            # If reregulating and if we are not currently in the fish passage,
            #   apply rergulating mortality in addition to the point value mortality
            #   provided in the quickset_data
            if (quickset_data$rereg == "Y" & i != "FP"){ # if re-regulation occurs:
                survival <- structure_surv * (1-quickset_data$rereg_mort)
                # Otherwise, if in the fish passage structure:
            } else if(quickset_data$rereg == "Y" & 
                      i == "FP" & 
                      # Only fish that go through the FSO are subject to rereg mortality
                      quickset_data$collector == "FSO"){
                survival <- structure_surv * (1-quickset_data$rereg_mort)
                # return(survival)             
            } else { # Otherwise, if FP that is not FSO and all others:
                survival <- structure_surv
            }
            ressim_data <- ressim_data %>%
                mutate("{structure}_survival" := survival)
        } else { # here, use table-based approaches
            # Check if it is multi-outlet - this is typical for ROs
            surv_table <- resv_data[which(names(resv_data)==paste0(structure, "_surv"))][[1]]
            # paste(surv_table)
            multioutlet <- ifelse(ncol(surv_table) > 2, TRUE, FALSE) # if >2 columns, multioutlet is true
            # cat(paste0("...multioutlet: ", multioutlet, "\n"))
            gate_method <- quickset_data$gate_methods[1,which(
                colnames(quickset_data$gate_methods) == paste0(structure, "_gatemethod"))]
            cat(paste0("...gate method: ", gate_method, "\n"))
            if( is.na(gate_method)){
                stop("No gate method provided.")
            }
            if (multioutlet) {
                bottomElev <- resv_data[which(names(resv_data)==paste0(structure, "_elevs"))][[1]]
                if( length(bottomElev) != ncol(surv_table)-1){
                    stop(paste0(
                        'The number of columns in the survival table does not match the number of elevations provided for ', structure, '.'
                    ))
                }
                nOutlet <- ncol(surv_table)-1
                cat(paste0('...Assuming ', nOutlet, ' outlets to distribute through gates (based on number of columns in the survival table) \n'))
                # First, create a linear interpolation of gateflow vs. survival table column 1; repeat for each column after column #2
                # Create an empty vector, interpolated_survivals, to fill in a loop
                survInterp_multi <- c()
                for(o in 1:nOutlet){
                    # add a linear interpolation function to the survInterp_multi list
                    survInterp_multi <- c(survInterp_multi, approxfun(
                        x=surv_table[,1], 
                        y=surv_table[,o+1],
                        rule=2
                    ))
                }
                # Save these variables for later
                elevXs <- ressim_data$elev   # elevation
                # Create multi-outlet table
                mo_table <- as.data.frame(
                    matrix(
                        NA, 
                        # One for each day of the series
                        nrow=nrow(ressim_data),
                        # Number of columns: flow, elevation, flow-based survival upper, flow-based survival lower
                        ncol=ncol(surv_table)+1
                    )
                )
                colnames(mo_table) <- c("elevs", "newFlow", paste0("survOutlet", 1:nOutlet))
            } else { # if not multiOutlet
                # The "nonflow" column will be the survival rate
                nonflow <- which(colnames(surv_table)!="flow")
                survLinearInterp <- approxfun(
                    x=surv_table$flow, 
                    y=surv_table[,nonflow],
                    rule=2
                )
            } # end of if multiOutlet() else linear
            # Flow distribution calculation
            # Select only the relevant flow data
            # use "^" to indicate beginning of line
            flowData_tmp <- data.frame(ressim_data)[,grep(paste0("^", tolower(structure), "_flow"), tolower(colnames(ressim_data)))]
            # This should exclude the fish-bearing flow columns, which begin with "B."

            gates <- resvData_sub$n_gates
            #
            # Perform calculations based on which method was indicated in the function call
            #
            if(gate_method=="Equal Q"){
                # Create these variables for weighting later
                weighted_survival <- 0
                for(g in 1:gates){
                    varname_flow <- paste0(structure, "_flow_gate", g)
                    varname_surv <- paste0(structure, "_surv_gate", g)
                    newflow <- flowData_tmp/gates
                    ressim_data <- mutate(ressim_data, !!varname_flow := newflow)
                    if(multioutlet){
                        mo_table[,1] <- elevXs
                        mo_table[,2] <- newflow
                        for(o in 1:nOutlet){
                            # Here, look up the flow-based survival for both upper and lower
                            # survYs <- c(survYs,survInterp_multi[[i]](newflow))
                            mo_table[,o+2] <- survInterp_multi[[o]](newflow)
                        }
                        # Create a simple y=mx+b style lookup here
                        # y = survival rates (high and low)
                        # x = elevation of the day
                        # interpolate y~x
                        ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
                        nearestSurv <- pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
                        #    the maximum survival, just use max. survival 
                            mo_table[,3] +          # y1 equivalent
                            ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
                            ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
                            )
                        )
                    } else { # if not multioutlet
                        nearestSurv <- survLinearInterp(newflow)
                    }
                    ressim_data <- mutate(ressim_data, 
                        !!varname_surv := nearestSurv)
                    weighted_survival <- ifelse(
                        is.na(nearestSurv),
                        weighted_survival,
                        weighted_survival+(newflow*(1/ifelse(flowData_tmp==0,Inf,flowData_tmp))*nearestSurv)
                    )
                    # weighted_survival <- weighted_survival+(newflow*(1/ifelse(flowData_tmp==0,Inf,flowData_tmp))*nearestSurv)
                }
                ressim_data <- ressim_data %>% 
                    mutate("{structure}_survival" := weighted_survival)
                    # mutate(flow_weighted_survival = weighted_survival)
            } else if(gate_method=="Min Q to equal"){
                weighted_survival <- 0
                min_flow <- resvData_sub$min_flow
                if(is.na(min_flow)){
                    stop(paste0("No minimum flow provided for outlet ", structure))
                }
                # VBA revision here: treat the gates differently
                ressim_data <- ressim_data %>%
                    mutate(
                        # The number of gates is the maximum number that 
                        # can be supplied with minimum flow
                        # !!!
                        # !!! CHANGE: Replaced RO flow with "flowData_tmp" - used to only be for RO? 
                        realized_gates = pmin(floor(flowData_tmp/min_flow), gates, 1, na.rm=T),
                        flowXgate = flowData_tmp/realized_gates,
                    ) %>%
                    select(-c(realized_gates, flowXgate))
                    if(multioutlet){
                        mo_table[,1] <- elevXs
                        mo_table[,2] <- newflow
                        for(o in 1:nOutlet){
                            # Here, look up the flow-based survival for both upper and lower
                            # survYs <- c(survYs,survInterp_multi[[i]](newflow))
                            mo_table[,o+2] <- survInterp_multi[[o]](newflow)
                        }
                        # Create a simple y=mx+b style lookup here
                        # y = survival rates (high and low)
                        # x = elevation of the day
                        # interpolate y~x
                        ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
                        nearestSurv <- pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
                        #    the maximum survival, just use max. survival 
                            mo_table[,3] +          # y1 equivalent
                            ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
                            ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
                            )
                        )
                        ressim_data <- ressim_data %>% 
                            mutate("{structure}_survival" := nearestSurv)
                    } else {
                        ressim_data <- ressim_data %>%
                        mutate(
                            "{structure}_survival" := survLinearInterp(flowXgate)
                        )
                    }
            } else if(gate_method=="Unit to Max Q"){
                weighted_survival <- 0
                max_flow <- resvData_sub$max_flow
                # Have to count down with flow
                remaining_flow <- flowData_tmp
                for(g in 1:gates){
                    varname_flow <- paste0(structure, "_flow_gate", g)
                    varname_surv <- paste0(structure, "_surv_gate", g)
                    # Here, set all flows > max to the maximum value
                    ### This line is creating lists
                    # newflow <- ifelse(remaining_flow > max_flow, max_flow, remaining_flow)
                    newflow <- pmin(data.frame(remaining_flow)[,1], max_flow)
                    # Create a new column in the output df
                    ressim_data <- mutate(ressim_data, !!varname_flow := newflow)
                    if(multioutlet){
                        mo_table[,1] <- elevXs
                        mo_table[,2] <- newflow
                        for(o in 1:nOutlet){
                            # Here, look up the flow-based survival for both upper and lower
                            # survYs <- c(survYs,survInterp_multi[[i]](newflow))
                            mo_table[,o+2] <- survInterp_multi[[o]](newflow)
                        }
                        nearestSurv <- pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
                        #    the maximum survival, just use max. survival 
                            mo_table[,3] +          # y1 equivalent
                            ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
                            ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
                            )
                        )
                    } else {
                        nearestSurv <- survLinearInterp(newflow)
                    }
                    ressim_data <- mutate(ressim_data, !!varname_surv := nearestSurv)
                    remaining_flow <- pmax((remaining_flow - max_flow), 0)
                    weighted_survival <- ifelse(
                        is.na(nearestSurv),
                        weighted_survival,
                        weighted_survival+(newflow*(1/ifelse(flowData_tmp==0,Inf,flowData_tmp))*nearestSurv)
                    )
                }
                ressim_data <- ressim_data %>% 
                    mutate("{structure}_survival" := weighted_survival)
            } else if(gate_method=="Target Q"){
                # If outlet flow is greater than target*gates:
                target_flow <- resvData_sub$target_flow
                ngates <- resvData_sub$n_gates
                ressim_data <- ressim_data %>%
                    mutate(
                        # This matches 
                        flowData = flowData_tmp,
                        realized_gates = case_when(
                            # If flow data is more than can be handled, 
                            # set to max number of gates
                            flowData_tmp > (target_flow*ngates) ~ ngates, 
                            # Otherwise, if there are two gates use only one
                            ngates == 2 ~ 1,
                            # Otherwise, the catch-all final case (indicated with TRUE ~ )
                            # is outlet flow divided by target flow
                            TRUE ~ ceiling(flowData/target_flow)
                        ),
                        # If 0 gates are being used, flow=0
                        # Else, 
                        gateflow = ifelse(realized_gates == 0, 0, flowData/realized_gates)
                    )
                    if(multioutlet){
                        mo_table[,1] <- elevXs
                        mo_table[,2] <- newflow
                        for(o in 1:nOutlet){
                            # Here, look up the flow-based survival for both upper and lower
                            # survYs <- c(survYs,survInterp_multi[[i]](newflow))
                            mo_table[,o+2] <- survInterp_multi[[o]](newflow)
                        }
                        # Create a simple y=mx+b style lookup here
                        # y = survival rates (high and low)
                        # x = elevation of the day
                        # interpolate y~x
                        ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
                        ressim_data <- ressim_data %>%
                            mutate("{structure}_survival" := 
                            pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
                        #    the maximum survival, just use max. survival 
                                mo_table[,3] +          # y1 equivalent
                                ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
                                ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
                                )
                            )
                        )
                    } else {
                        ressim_data <- ressim_data %>% 
                                mutate("{structure}_survival" := survLinearInterp(ressim_data$gateflow))
                    }
            } else if(gate_method=="Peaking Performance"){
                min_flow <- resvData_sub$min_flow
                max_flow <- resvData_sub$max_flow
                ngates <- resvData_sub$n_gates
                if(min_flow==0){
                    realized_gates <- ngates
                } else {
                    realized_gates <- data.frame(floor(flowData_tmp/min_flow))
                    # Where greater than ngates, make ngates
                    for(rg in 1:ncol(realized_gates)){
                        realized_gates[which(realized_gates[,rg] > ngates),rg] <- ngates
                    }
                }
                if(multioutlet){
                    mo_table[,1] <- elevXs
                    mo_table_min <- mo_table
                    mo_table_min[,2] <- min_flow
                    for(o in 1:nOutlet){
                        # Here, look up the flow-based survival for both upper and lower
                        # survYs <- c(survYs,survInterp_multi[[i]](newflow))
                        mo_table_min[,o+2] <- survInterp_multi[[o]](min_flow)
                    }
                    # Create a simple y=mx+b style lookup here
                    # y = survival rates (high and low)
                    # x = elevation of the day
                    # interpolate y~x
                    ### min(y2, y1+((y2-y1)*((x1-2100)/(x1-x2))))
                    survMinFlow <- pmin(pmax(mo_table_min[,3], mo_table_min[,4]), # if the extrapolated value is greater than
                    #    the maximum survival, just use max. survival 
                        mo_table_min[,3] +          # y1 equivalent
                        ((mo_table_min[,4] - mo_table_min[,3] )* # y2-y1 range
                        ((bottomElev[[1]]-mo_table_min[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
                        )
                    )            
                } else {
                    survMinFlow <- survLinearInterp(min_flow)
                }
                cat("...Please wait...peaking performance takes a few moments to calculate...")
                # flow_weighted_survival <- c()
                volLeft_o <- flowData_tmp*24 # Convert to cfs per hour
                # Remove the minimum flow/gate * 24h * number of active gates
                volLeft <- volLeft_o-(realized_gates*min_flow*24)
                # Create these empty vectors to fill up
                if(multioutlet){
                    weighted_percent <- data.frame(matrix(
                        0,
                        ncol=ncol(volLeft), 
                        nrow=nrow(volLeft) 
                    ))
                } else {
                    weighted_percent <- c()
                }
                survivalP <- c()
                for(h in 1:24){ # Have to do this hour by hour...YUCK!
                    print(h)
                    for(g in 1:ngates){
                        # If multioutlet, there will be multiple columns for volLeft
                        if(multioutlet){
                            # get the minimum of (max flow - min flow) and the current flow left
                            flowAboveMin <- volLeft 
                            for(v in 1:ncol(volLeft)){
                                flowAboveMin[which(flowAboveMin[,v] >= (max_flow-min_flow)),v] <- max_flow-min_flow
                            }
                            for(o in 1:nOutlet){
                                mo_table[,o+2] <- survInterp_multi[[o]](min_flow+flowAboveMin[,o])
                            }
                            survivalP <- pmin(pmax(mo_table[,3], mo_table[,4]), # if the extrapolated value is greater than
                                mo_table[,3] +          # y1 equivalent
                                ((mo_table[,4] - mo_table[,3] )* # y2-y1 range
                                ((bottomElev[[1]]-mo_table[,1])/(bottomElev[[1]]-bottomElev[[2]])) # (x1 - new X)/(x1-x2)
                                )
                            )
                            survivalP[which(flowAboveMin[,1]==0)] <- 0
                            # volLeft <- volLeft-flowAboveMin
                            # volLeft[volLeft < 0] <- 0
                            # weighted_percent <- weighted_percent + (min_flow+flowAboveMin)*survivalP
                            for(o in 1:nOutlet){
                                weighted_percent[,o] <- weighted_percent[,o] + (min_flow+flowAboveMin[,o])*survivalP
                                volLeft[,o] <- volLeft[,o] - flowAboveMin[,o]
                            }
                            volLeft[volLeft < 0] <- 0
                        } else { # if NOT multioutlet, such that volLeft has one column:
                            weighted_percent <- c()
                            survivalP <- c()
                            flowAboveMin <- volLeft
                            flowAboveMin[which(flowAboveMin >= (max_flow-min_flow))] <- max_flow-min_flow
                            # Now use this to lookup
                            survivalP <- survLinearInterp(min_flow+flowAboveMin)
                            survivalP[which(flowAboveMin==0)] <- 0
                            weighted_percent <- weighted_percent + (min_flow+flowAboveMin)*survivalP
                            volLeft <- pmin(volLeft - flowAboveMin, 0)
                        }
                    }
                }
                survivalPercent <- rowMeans(weighted_percent/flowData_tmp/24) #Often these are identical, in my test
                # only 5/27,010 do not match
                # Include this to be rid of divide by zero issues
                survivalPercent[survivalPercent==Inf] <- 0
                ressim_data <- ressim_data %>%
                    mutate("{structure}_survival" := survivalPercent)
            }
        }
        # save survival estimates 
    }
    return(ressim_data)
}
