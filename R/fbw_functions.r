
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
                survival <- structure_surv * (1 - quickset_data$rereg_mort)
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
            # Perform calculations based on which method was indicated in the 
            #   function call
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
                        nearestSurv <- pmin(pmax(mo_table[,3], mo_table[,4]), 
                        # if the extrapolated value is greater than
                        #    the maximum survival, just use max. survival 
                            mo_table[,3] +          # y1 equivalent
                            ((mo_table[,4] - mo_table[,3] ) * # y2-y1 range
                            ((bottomElev[[1]] - mo_table[,1]) / 
                            (bottomElev[[1]] - bottomElev[[2]])) # (x1 - new X)/(x1-x2)
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
