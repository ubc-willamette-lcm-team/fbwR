
# Packages currently used for these functions
suppressMessages(require(dplyr))
suppressMessages(require(tidyr))
suppressMessages(require(lubridate))
suppressMessages(require(data.table))
suppressMessages(require(readxl))

# FUNCTION readExcelFBW() - Function that will read "raw" data from a completed 
#   and fully run Excel FBW workbook
#
#   INPUTS:
#   workbook_file : a string indicating the full file path including the 
#       workbook file (.xlsm) 
#       (may be relative to the working directory or a global path)
#   input_sheet: defaults to "FISH-Routing&Survival", can be changed if raw data
#       is named something else. 
#       WARNING: If a different sheet is used, it is likely that the row and col 
#       indices/cell range will be broken and data will not be read correctly. 
#   slam_summary: defaults to FALSE; should monthly mean summaries be created
#       and returned as outputs? 
#
#   OUTPUTS:
#   a data frame containing columns: 
#       water_year_type: current water year type (runs from Oct-Sept)
#       ressim_date: full date and year
#       ph_population: proportion of the population in the powerhouse
#       ph_survival: powerhouse/turbine survival (including DPE)
#       ro_population: proportion of the population through the reg. outlet
#       ro_survival: regulating outlet survival (incl. DPE)
#       spill_population: proportion of the population through the spillway
#       spill_survival: spillway survival (incl. DPE)
#       fps_population: proportion of the population through the fish passage
#       fps_survival: fish passage structure survival (incl. DPE)
#       forebay_population: proportion of the population that remains in the
#           forebay (i.e., the 1-DPE component of the population)

# NOTE! THIS FUNCTION FAILS IF THERE ARE ANY ERRORS IN THE ORIGINAL DATA

readExcelFBW <- function(workbook_file=NULL, input_sheet="FISH-Routing&Survival",
    slam_summary=FALSE){
    if(is.null(workbook_file)){
        stop("readExcelFBW() requires input: workbook_file")
    }
    water_year_types <- data.frame(t(
        readxl::read_excel(workbook_file, sheet = "TEMP-SPLIT", 
        col_names = F, range = "B1:BW7",
        col_types = "text"
    ))[,c(1,7)]) # select only cols 1 and 7 (lots of empties)
    # Assign column names
    colnames(water_year_types) <- c("water_year_type", "year")
    rownames(water_year_types) <- NULL
    water_year_types$year <- as.numeric(as.character(water_year_types$year))
    water_year_types$water_year_type <- as.character(water_year_types$water_year_type)
    # Enforce numeric year ID
    water_year_types$year <- as.numeric(as.character(water_year_types$year))
    # New addition: only include water year types from Oct-Sept
    water_year_months <- water_year_types %>%
        mutate(water_year_type_rowIDX = row_number()) %>%
        group_by(year, water_year_type_rowIDX) %>% 
        expand(month=1:12) %>%
        ungroup() %>%
        # order(c(year, month)) %>%
        mutate(
            row_idx = case_when(
                month %in% 10:12 ~ as.integer(water_year_type_rowIDX), 
                month %in% 1:9 ~ as.integer(water_year_type_rowIDX - 1)
                ))
        # The first 9 rows (i.e., pre-first year of water year type classes) are NA
        water_year_months$row_idx[1:9] <- NA
        water_year_months <- water_year_months %>%
            mutate(water_year_type_corrected = water_year_types$water_year_type[row_idx])                    
        # PH = powerhouse, read in population first then survival
        ph_population <- readxl::read_excel(workbook_file, sheet = input_sheet, 
                  col_names = T, range = "B3:BX368") 
        ph_survival <- readxl::read_excel(workbook_file, sheet = input_sheet, 
                  col_names = T, range = "B371:BX736")
                # Regulating outlet
        ro_population <- readxl::read_excel(workbook_file, sheet = input_sheet, 
                  col_names = T, range = "B741:BX1106")
        ro_survival <- readxl::read_excel(workbook_file, sheet = input_sheet, 
                  col_names = T, range = "B1109:BX1474")
                # Spillway
        spill_population <- readxl::read_excel(workbook_file, sheet = input_sheet, 
                  col_names = T, range = "B1479:BX1844")
        spill_survival <- readxl::read_excel(workbook_file, sheet = input_sheet, 
                  col_names = T, range = "B1847:BX2212")
                # Fish passage survival
        fps_population <- readxl::read_excel(workbook_file, sheet = input_sheet, 
                  col_names = T, range = "B2217:BX2582")
        fps_survival <- readxl::read_excel(workbook_file, sheet = input_sheet, 
                  col_names = T, range = "B2585:BX2950")
        # Lost to forebay population (1-DPE)
        forebay_population <- readxl::read_excel(workbook_file, 
                  sheet = input_sheet, col_names = T, range = "B2955:BX3320")
        elevation <- readxl::read_excel(workbook_file,
                  sheet = "POOL-ELEV", range="A7:BW372", col_names = T)
        colnames(elevation)[1] <- "Date"
        raw_dat_list <- list(
            "ph_population" = ph_population, 
            "ph_survival" = ph_survival, 
            "ro_population" = ro_population, 
            "ro_survival" = ro_survival, 
            "spill_population" = spill_population, 
            "spill_survival" = spill_survival, 
            "fps_population" = fps_population, 
            "fps_survival" = fps_survival, 
            "forebay_population" = forebay_population,
            "pool_elevation" = elevation
        )
        # Convert each to a long data frame in a loop
        for (l in seq_along(raw_dat_list)) {
            # get the name of the object to use as a column name 
            # (e.g., ro_survival)
            current_colname <- names(raw_dat_list)[[l]]
            df_tmp <- raw_dat_list[[l]]
            df_out <- data.frame(
                # pivot along all other columns except Date (the shared col)
                tidyr::pivot_longer(
                    df_tmp, cols = -Date, 
                    names_to = "year", values_to = current_colname,
                    values_drop_na = FALSE
                )
            )
            # add the year to the Date column in the long dataframe
            lubridate::year(df_out$Date) <- as.numeric(df_out$year) 
            raw_dat_list[[l]] <- df_out %>% select(-"year")
        }
        # Then merge (Reduce()) the list objects using Date
        raw_data <- Reduce(function(x, y)
                  merge(x, y, by = "Date"),
                  raw_dat_list)
        raw_data <- raw_data %>%
            # Use mutate to create new columns, Month and year                
            mutate(
                month = lubridate::month(Date), 
                year = as.numeric(lubridate::year(Date)))
        wyr_raw <- merge(x = water_year_months, y = raw_data)
        if(slam_summary == FALSE){
            return(wyr_raw %>% 
                rename(ressim_date = Date) %>%
                rename(water_year_type = water_year_type_corrected) %>%
                select(-c(year, month, water_year_type_rowIDX, row_idx)) %>%
                dplyr::arrange(ressim_date)
                )
        } else {
            summary_wyt <- wyr_raw %>%
                  group_by(month, year, water_year_type_corrected) %>%
                  summarize(
                    ph_survival_monthly=sum(ph_survival, na.rm=T),
                    ro_survival_monthly=sum(ro_survival, na.rm=T),
                    spill_survival_monthly=sum(spill_survival, na.rm=T),
                    fps_survival_monthly=sum(fps_survival, na.rm=T),
                    ph_population_monthly=sum(ph_population, na.rm=T),
                    ro_population_monthly=sum(ro_population, na.rm=T),
                    spill_population_monthly=sum(spill_population, na.rm=T),
                    fps_population_monthly=sum(fps_population, na.rm=T),
                    monthly_1_minus_dpe=sum(forebay_population, na.rm=T),
                    mean_pool_elev=mean(pool_elevation, na.rm=T)
                  ) %>%
                  mutate(
                    project_FBW_Survival_inclDPE = sum(
                      ph_survival_monthly,
                      ro_survival_monthly,
                      spill_survival_monthly,
                      fps_survival_monthly),
                    project_FBW_population_afterDPE = sum(
                      ph_population_monthly,
                      ro_population_monthly,
                      spill_population_monthly,
                      fps_population_monthly),
                    # Finally, project survival WITHOUT DPE
                    project_survival_withoutDPE = project_FBW_Survival_inclDPE/project_FBW_population_afterDPE
                  ) %>%
                  dplyr::arrange(year, month) %>%
                  relocate(project_FBW_Survival_inclDPE, monthly_1_minus_dpe, 
                    .before=ph_survival_monthly) %>%
                  select(year, month, water_year_type_corrected, mean_pool_elev,
                    project_FBW_Survival_inclDPE, 
                    monthly_1_minus_dpe, 
                    project_survival_withoutDPE) %>%
                  rename(water_year_type = water_year_type_corrected)
            return(summary_wyt)
        }       
}

# This function isn't always used - requires specific data formatting which is currently replaced with a  
# FUNCTION loadResvData_old - Function that will read in a series of .csv files provided a file path and file search pattern
#   then save this as a global list object, "resvData" 
#
#   INPUTS:
#   path : a string indicating the file path (may be relative to the working directory or a global path) where
#       the following csv files can be found:
#       1) ..._BaselineFishApproaching.csv; columns [date (first of month), reservoir, pcentFish]
#       2) ..._BottomElev.csv; columns [reservoir, outlet [RO, spill, Turb], bottomElev (feet)]
#       3) ..._DamPassage.csv; columns [reservoir, baselinesource,reservoirElevations,elevationDescription,
#           baselineDamPasasge,FSSDamPassage,FSCDamPassage,WeirBoxDamPassage]
#       4) ..._MaxFlow.csv; columns [reservoir, outlet, maxFlow (cfs)]
#       5) ..._MinFlowPerUnit.csv; columns [reservoir, outlet, minFlowPer]
#       !!! Currently empty as of FBW-Steelhead-Year_25-Jul-2014_MinFlowPerUnit.csv
#       6) ..._nGates.csv; columns [reservoir, outlet, nGates]
#       7) ..._normally_used.csv; columns [reservoir, outlet, normally_used (Y/N)]
#       8) ..._TargetFlowRate.csv; columns [reservoir, outlet, target (cfs)]
#       9) ..._RouteEffectiveness.csv; columns [reservoir, qratio, outlet, routeEffectiveness]
#       10) ..._RoutePassageSurvival.csv; columns[reservoir, poolHighLow(high or low for ROs), outlet (RO/spill), 
#           Q, surv]
#   prefix : (optional) if there is a common prefix to all files listed above (1-10), only read in files with that prefix.
#           If not provided, the first files that match the file suffixes above are used (everything after "_"). 
#   reservoir: (optional) which reservoir's data should be loaded? If not provided, returns all data
#
#   OUTPUTS:
#   a global variable, resvData, containing a list of reservoir-specific parameter lists 
#       (e.g., to get route effectiveness for Detroit, try: resvData$Detroit$RouteEffectiveness) 

loadResvData_old <- function(path, prefix=NULL, targetreservoir=NULL){
    # required files 
    req_files <- c("BaselineFishApproaching","BottomElev","DamPassage","MaxFlow",
        "MinFlowPerUnit","nGates","normally_used","TargetFlowRate", "RouteEffectiveness",
        "RoutePassageSurvival")
    # Initialize an empty list to save files within
    out <- list()
    # Counter to keep track of which index in "out" is being filled 
    rf_counter <- 1
    # List all files with prefix if provided
    if(!is.null(prefix)){
        # If there is a prefix provided (! = not), use this to find files
        file_list <- list.files(path=path, pattern=prefix)
    } else {
        # If there is no prefix, simply list all csv files. The first matching will be selected.
        file_list <- list.files(path=path, pattern="*.csv")
    }
    for(rf in req_files){
        matching_file <- file_list[grep(rf, file_list)]
        if(length(matching_file)>1){
            # If there are >1 matching files, read in the first one. 
            warning(paste0("Multiple .csvs in ", path, " match search key '", rf, "'. Reading the first match, '", file_list[1]))
            matching_file <- matching_file[1]
        }
        # If there are no files matching, leave the function and return an error (error depends on whether a prefix is provided). 
        if(is.na(matching_file)){
            if(!is.null(prefix)){
                stop(
                    paste0("No file matches search key '", prefix, "...", rf, ".csv'. Please verify the file exists in ", path, " and try loading your data again.\n")
                )
            } else {
                stop(
                    paste0("No file matches search key '...", rf, ".csv'. Please verify the file exists in ", path, " and try loading your data again.\n")
                ) 
            }
        }
        # Read the csv as a data.frame
        out[[rf_counter]] <- as.data.frame(read.csv(paste0(path, matching_file), header=T, sep=","))
        # Name the entry in "out" with the variable
        names(out[rf_counter]) <- rf
        # Increase the counter
        rf_counter=rf_counter+1
    }
    # If the user provides a target reservoir, return all data
    if(is.null(targetreservoir)){
        names(out) <- req_files
        return(out)
    # Otherwise filter to that reservoir if available, exit with error if the reservoir is missing from any dataframes 
    } else {
        filtered_out <- list()
        for(i in 1:length(out)){
            resv_tmp <- unique(out[[i]]$reservoir)
            # Exit if reservoir not found
            if(!(targetreservoir %in% resv_tmp)){
                stop(paste0("Reservoir '", targetreservoir, "' not in data file ", names(out)[i], "\n"))
            } else {
                filtered_out[[i]] <- out[[i]] %>% filter(reservoir == targetreservoir)
            }
        }
        names(filtered_out) <- req_files
        # Return the filtered data
        return(filtered_out)
    }
}

# FUNCTION loadResvData_FBWworkbook - Function that will read in reservoir data from an FBW workbook
#   given the row and column locations provided by a user
#   INPUTS:
#   infile : a string indicating the full file path to the FBW workbook with loaded reservoir data
#   resv_column: the name of the Excel column where the reservoir data are found
#       in the ResvData sheet (in excel string format, e.g. "C", "AD", etc.)
#
#   OUTPUTS:
#   A named list of reservoir and quickset parameters:
#       baseline_fishApproaching - distribution of fish through time according to the baseline
#       quickset_fishApproaching - same as above, but using timing specified in the quickset (may be blank)
#       fps_type - the kind of fish passage structure, if any
#       routeE - route effectiveness, a multiplier on the route's ability to "attract" fish 
#           beyond what would be expected by flow alone
#       routeDPE - route dam passage efficiency, the expected proportion of fish that will 
#           remain in the forebay as a function of pool elevation
#       survRO - survival through a regulating outlet (often a function of flow)
loadResvData_FBWworkbook <- function(infile, resv_column) {
    outlist <- list()
    outlet = c("RO", "Turb", "Spill", "FPS")
    # These are given only for RO, TURB, Spill - need to add an "NA" for the 
    #   fish passage structure
    max_flow_RO <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "9")
    )
    max_flow_Turb <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "10")
    )
    max_flow_Spill <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "11")
    )
    max_flow_RO <- ifelse(length(max_flow_RO)==0 || is.na(max_flow_RO), NA, max_flow_RO[[1]])
    max_flow_Turb <- ifelse(length(max_flow_Turb)==0 || is.na(max_flow_Turb), NA, max_flow_Turb[[1]])
    max_flow_Spill <- ifelse(length(max_flow_Spill)==0 || is.na(max_flow_Spill), NA, max_flow_Spill[[1]])
    max_flow_FPS <- NaN
    max_flow <- c(max_flow_RO, max_flow_Turb, max_flow_Spill, max_flow_FPS)
    # Bottom functional elevation
    bottom_elev_RO <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "9")
    )
    bottom_elev_Turb <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "10")
    )
    bottom_elev_Spill <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "11")
    )
    bottom_elev_RO <- ifelse(length(bottom_elev_RO)==0 || is.na(bottom_elev_RO), NA, bottom_elev_RO[[1]])
    bottom_elev_Turb <- ifelse(length(bottom_elev_Turb)==0 || is.na(bottom_elev_Turb), NA, bottom_elev_Turb[[1]])
    bottom_elev_Spill <- ifelse(length(bottom_elev_Spill)==0 || is.na(bottom_elev_Spill), NA, bottom_elev_Spill[[1]])
    bottom_elev_FPS <- NaN
    bottom_elev <- c(bottom_elev_RO, bottom_elev_Turb, bottom_elev_Spill, bottom_elev_FPS)
    # How many passages through the route?
    n_gates_RO <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "17")
    )
    n_gates_Turb <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "18")
    )
    n_gates_Spill <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "19")
    )
    n_gates_FPS <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "20")
    )
    n_gates_RO <- ifelse(length(n_gates_RO)==0 || is.na(n_gates_RO), NA, n_gates_RO[[1]])
    n_gates_Turb <- ifelse(length(n_gates_Turb)==0 || is.na(n_gates_Turb), NA, n_gates_Turb[[1]])
    n_gates_Spill <- ifelse(length(n_gates_Spill)==0 || is.na(n_gates_Spill), NA, n_gates_Spill[[1]])
    n_gates_FPS <- ifelse(length(n_gates_FPS)==0 || is.na(n_gates_FPS), NA, n_gates_FPS[[1]])
    n_gates <- c(n_gates_RO, n_gates_Turb, n_gates_Spill, n_gates_FPS)
    # Min flow
    min_flow_RO <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "22")
    )
    min_flow_Turb <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "23")        
    )    
    min_flow_Spill <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "24")
    )
    min_flow_FPS <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "25")
    )
    min_flow_RO <- ifelse(length(min_flow_RO)==0 || is.na(min_flow_RO), NA, min_flow_RO[[1]])
    min_flow_Turb <- ifelse(length(min_flow_Turb)==0 || is.na(min_flow_Turb), NA, min_flow_Turb[[1]])
    min_flow_Spill <- ifelse(length(min_flow_Spill)==0 || is.na(min_flow_Spill), NA, min_flow_Spill[[1]])
    min_flow_FPS <- ifelse(length(min_flow_FPS)==0 || is.na(min_flow_FPS), NA, min_flow_FPS[[1]])
    min_flow <- c(min_flow_RO, min_flow_Turb, min_flow_Spill, min_flow_FPS)
    # Target flow rate, used to calculate powerhouse flow typically
    target_flow_RO <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "27")
    )
    target_flow_Turb <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "28")
    )
    target_flow_Spill <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "29")
    )
    target_flow_FPS <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "30")
    )
    target_flow_RO <- ifelse(length(target_flow_RO)==0 || is.na(target_flow_RO), NA, target_flow_RO[[1]])
    target_flow_Turb <- ifelse(length(target_flow_Turb)==0 || is.na(target_flow_Turb), NA, target_flow_Turb[[1]])
    target_flow_Spill <- ifelse(length(target_flow_Spill)==0 || is.na(target_flow_Spill), NA, target_flow_Spill[[1]])
    target_flow_FPS <- ifelse(length(target_flow_FPS)==0 || is.na(target_flow_FPS), NA, target_flow_FPS[[1]])
    target_flow <- c(target_flow_RO, target_flow_Turb, target_flow_Spill, target_flow_FPS)
    # Is this usually used?
    used_ro <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "32")
    )
    used_turb <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "33")
    )
    used_spill <- read_excel(na=character(), trim_ws = F,
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column, "34")
    )
    used_ro <- ifelse(is.na(used_ro), "N", used_ro[[1]])
    used_turb <- ifelse(is.na(used_turb), "N", used_turb[[1]])
    used_spill <- ifelse(is.na(used_spill), "N", used_spill[[1]])
    used <- c(used_ro, used_turb, used_spill, NA) # add NA because FPS is not given here
    outlist$outlet_data <- data.frame(
        # FPS = fish passage structure
        outlet = outlet,
        max_flow = max_flow, 
        bottom_elev = bottom_elev, 
        n_gates = n_gates, 
        min_flow = min_flow,
        target_flow = target_flow, 
        normally_used = used
    )
    # Remove all-NA rows
    ### outlist$outlet_data <- outlist$outlet_data[rowSums(is.na(
    ###   outlist$outlet_data)) != ncol(outlist$outlet_data), ]
    #colnames(outlist$outlet_data) <- c("outlet", "min_flow", "target_flow", 
    #    "max_flow", "n_gates", "bottom_elev", "normally_used")
    # Route_effectivness
    q_ratio = seq(0, 1, by = 0.1)
    spill_routeE <- read_excel(na=character(), trim_ws = F,  
        infile, sheet = "ResvData", col_names = FALSE, 
        range = paste0(resv_column,"49:", resv_column,"59"))
    ro_routeE <- read_excel(na=character(), trim_ws = F,  
        infile, sheet = "ResvData", col_names = FALSE,
        range = paste0(resv_column,"61:", resv_column,"71"))
    turbine_routeE <- read_excel(na=character(), trim_ws = F,  
        infile, sheet = "ResvData", col_names=FALSE,
        range = paste0(resv_column,"73:", resv_column,"83"))
    outlist$route_effectiveness <- data.frame(
        q_ratio = seq(0, 1, by = 0.1),
        spill = ifelse(
            nrow(spill_routeE) == length(q_ratio),
            spill_routeE,
            rep(NA, length(q_ratio))
        ),
        ro = ifelse(
            nrow(ro_routeE) == length(q_ratio),
            ro_routeE,
            rep(NA, length(q_ratio))
        ),
        turbine = ifelse(
            nrow(turbine_routeE) == length(q_ratio),
            turbine_routeE,
            rep(NA, length(q_ratio))
        )
    )
    outlist$route_effectiveness <- outlist$route_effectiveness[rowSums(is.na(
        outlist$route_effectiveness)) != ncol(outlist$route_effectiveness), ]
    colnames(outlist$route_effectiveness) <- c("q_ratio", "Spill", "RO", "Turb")
    # Dam passage efficiency (DPE)
    elev <- read_excel(na=character(),
            infile, sheet = "ResvData", col_names = FALSE,
            range = paste0(resv_column,"85:", resv_column,"98"))
    elev_description <- read_excel(na=character(),
            infile, sheet = "ResvData", col_names=FALSE, 
            range = paste0(resv_column,"100:", resv_column,"113"))
    baseline_dpe <- read_excel(na=character(),
            infile, sheet = "ResvData", col_names = FALSE, 
            range = paste0(resv_column,"115:", resv_column,"128"))
    fss_dpe <- read_excel(na=character(),
            infile, sheet="ResvData", col_names=FALSE,
            range=paste0(resv_column,"130:", resv_column,"143"))
    fsc_dpe <- read_excel(na=character(),
            infile, sheet="ResvData", col_names=FALSE, 
            range=paste0(resv_column,"145:", resv_column,"158"))
    weir_dpe <- read_excel(na=character(),
            infile, sheet="ResvData", col_names=FALSE, 
            range=paste0(resv_column,"160:", resv_column,"173"))
    outlist$route_dpe <- data.frame(
        elev = elev,
        elev_description = ifelse(
            nrow(elev_description) == nrow(elev),
            elev_description,
            rep(NA, nrow(elev))
        ),
        baseline_dpe = ifelse(
            nrow(baseline_dpe) == nrow(elev),
            baseline_dpe,
            rep(NA, nrow(elev))
        ),
        fss_dpe = ifelse(
            nrow(fss_dpe) == nrow(elev),
            fss_dpe,
            rep(NA, nrow(elev))
        ),
        fsc_dpe = ifelse(
            nrow(fsc_dpe) == nrow(elev),
            fsc_dpe,
            rep(NA, nrow(elev))
        ),
        weir_dpe = ifelse(
            nrow(weir_dpe) == nrow(elev),
            weir_dpe,
            rep(NA, nrow(elev))
        )
    )
    outlist$route_dpe <- outlist$route_dpe[rowSums(
        is.na(outlist$route_dpe)) != ncol(outlist$route_dpe), ]
    colnames(outlist$route_dpe) <- c("elev","elev_description", 
        "baseline_dpe", "fss_dpe", "fsc_dpe", "weir_dpe")
     # End of the outlet data, let's do fish approaching next
    month <- read_excel(infile, col_types = "date", sheet="ResvData",
        col_names=TRUE, range="A35:A47")
    approaching <- read_excel(infile, sheet="ResvData", col_names=FALSE,
        range=paste0(resv_column,"36:",resv_column,"47"))
    # 
    approaching <- ifelse(all(dim(approaching)==c(0,0)),
        rep(NA, dim(month)[1]), approaching
    )
    outlist$baseline_fish_approaching <- data.frame(
        Date = month, 
        approaching = approaching
    )
    colnames(outlist$baseline_fish_approaching) <- c('Date', 'approaching')
    ###
    # Flow-based survivals
    ###
    ro_flow <- read_excel(na=character(), trim_ws = F,  
        infile, sheet="ResvData", col_names=FALSE, 
        range=paste0(resv_column,"183:", resv_column,"202"))
    if (all(dim(ro_flow) == c(0, 0))) {
        outlist$ro_surv <- data.frame(
            flow = NA,
            ro_surv_low = NA,
            ro_surv_high = NA
        )
    } else {
        ro_surv_lower = read_excel(na=character(), trim_ws = F,  
            infile, sheet="ResvData", col_names=FALSE, 
            range=paste0(resv_column,"204:", resv_column,"223"))
        ro_surv_upper = read_excel(na=character(), trim_ws = F,  
            infile, sheet="ResvData", col_names=FALSE, 
            range=paste0(resv_column,"225:", resv_column,"244"))
        outlist$ro_surv <- data.frame(
            flow = ro_flow, 
            ro_surv_low = ro_surv_lower,
            ro_surv_upper = ro_surv_upper
        )
        # This removes any all-NA rows/trims to only rows with some data in any
        #   column
        outlist$ro_surv <- outlist$ro_surv[rowSums(is.na(
            outlist$ro_surv)) != ncol(outlist$ro_surv), ]
        colnames(outlist$ro_surv) <- c("flow", "ro_surv_low", "ro_surv_high")
    }
    lower_ro_elev <- as.numeric(read_excel(na=character(), trim_ws = F,  infile, sheet="ResvData", col_names=FALSE, range=paste0(resv_column,"180")))
    upper_ro_elev <- as.numeric(read_excel(na=character(), trim_ws = F,  infile, sheet="ResvData", col_names=FALSE, range=paste0(resv_column,"181")))
    outlist$ro_elevs <- c(
        ifelse(length(lower_ro_elev) == 0, NA, lower_ro_elev), 
        ifelse(length(upper_ro_elev) == 0, NA, upper_ro_elev))
    # outlist$ro_elevs <- c(
    #     unlist(read_excel(na=character(), trim_ws = F,  infile, sheet="ResvData", col_names=FALSE, range=paste0(resv_column,"180"))),
    #     unlist(read_excel(na=character(), trim_ws = F,  infile, sheet="ResvData", col_names=FALSE, range=paste0(resv_column,"181")))
    # )
    names(outlist$ro_elevs) <- c("ro_lower", "ro_upper")
        # "ro_upper"))
    # Flow-based survival through turbines
    turb_flow <- read_excel(na=character(), trim_ws = F,  
            infile, sheet = "ResvData", col_names = FALSE, 
            range = paste0(resv_column, "246:", resv_column, "265"))
    if (all(dim(turb_flow) == c(0, 0))) {
        outlist$turb_surv <- data.frame(
            flow = NA,
            turb_surv = NA
        )
    } else {
        turb_surv = read_excel(na=character(), trim_ws = F,  
            infile, sheet = "ResvData", col_names = FALSE, 
            range = paste0(resv_column, "267:", resv_column, "286"))
        outlist$turb_surv <- data.frame(
            flow = turb_flow, 
            turb_surv = turb_surv
        )
        # This removes any all-NA rows/trims to only rows with some data in any
        #   column
        outlist$turb_surv <- outlist$turb_surv[rowSums(is.na(
            outlist$turb_surv)) != ncol(outlist$turb_surv), ]
        colnames(outlist$turb_surv) <- c("flow", "turb_surv")
    }
    # Flow-based survival through spillways
    spill_flow <- read_excel(na=character(), trim_ws = F,  
            infile, sheet="ResvData", col_names=FALSE, 
            range=paste0(resv_column,"288:", resv_column,"307"))
    if (all(dim(spill_flow) == c(0, 0))) {
        outlist$spill_surv <- data.frame(
            flow = NA,
            spill_surv = NA
        )
    } else {
        spill_surv = read_excel(na=character(), trim_ws = F,  
            infile, sheet = "ResvData", col_names = FALSE, 
            range = paste0(resv_column, "309:", resv_column, "328"))
        outlist$spill_surv <- data.frame(
            flow = spill_flow, 
            spill_surv = spill_surv
        )
        # This removes any all-NA rows/trims to only rows with some data in any
        #   column
        outlist$spill_surv <- outlist$spill_surv[rowSums(is.na(
            outlist$spill_surv)) != ncol(outlist$spill_surv), ]
        colnames(outlist$spill_surv) <- c("flow", "spill_surv")
    }
    return(outlist)
}

# FUNCTION loadQuicksetData_FBWworkbook - Function that will read in "quickset" data from an FBW workbook
#   given the row provided by a user
#
#   INPUTS:
#   infile : a string indicating the full file path to the FBW workbook with loaded reservoir data
#   quickset_row : a number indicating the row number where the "quickset" (i.e., set of
#       measures put in place for that reservoir) can be found

loadQuicksetData_FBWworkbook <- function(infile, quickset_row) {
    outlist <- list()
    outlist$quickset_name <- as.character(
        read_excel(na=character(), trim_ws = F,  infile, sheet="QuickSets", 
            col_names=FALSE, col_types = "text", 
            range = paste0("B", quickset_row)))
    outlist$quickset_desc <- as.character(
        read_excel(na=character(), trim_ws = F,  infile, sheet="QuickSets", 
            col_names=FALSE, col_types = "text", 
            range = paste0("C", quickset_row)))
    # Read these in one at a time, then spin into a list at the end
    outlist$quickset_fish_approaching <- data.frame(
        read_excel(na=character(), trim_ws = F,  infile, sheet="ResvData", 
            col_names=TRUE, range = "A35:A47", col_types = "date"),
        transpose(
            read_excel(na=character(), trim_ws = F,  infile, sheet="QuickSets", 
                col_names=FALSE, col_types="numeric", 
                range=paste0("W", quickset_row,":AH", quickset_row)))
    )
    colnames(outlist$quickset_fish_approaching) <- c("Date", "approaching")
    # FPS information
    outlist$fps_alternative <- as.character(
        read_excel(na=character(), trim_ws = F,  infile,
        sheet="QuickSets", col_names=FALSE, col_types = "text", 
        range=paste0("D",quickset_row)))
    outlist$nets <- as.character(
        read_excel(na=character(), trim_ws = F,  infile, 
        sheet="QuickSets", col_names=FALSE, col_types = "text", 
        range=paste0("E",quickset_row)))
    outlist$collector <- as.character(
        read_excel(na=character(), trim_ws = F,  infile, 
        sheet="QuickSets", col_names=FALSE, col_types = "text", 
        range=paste0("F",quickset_row)))
    if(outlist$collector == "NONE"){
        outlist$collector <- "noCollector"
    }
    outlist$rereg <- as.character(
        read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "text", 
            range = paste0("G", quickset_row)
    ))
    outlist$rereg_mortality <- as.numeric(
        read_excel(na=character(), 
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "numeric", 
            range = paste0("H", quickset_row)
    ))
    outlist$fish_with_flow <- as.character(
        read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "text", 
            range = paste0("I", quickset_row)
    ))
    outlist$temp_dist <- as.character(
        read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "text", 
            range = paste0("J", quickset_row)
    ))
    outlist$fps_q_max <- as.numeric(
        read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "numeric", 
            range = paste0("K", quickset_row)
    ))
    outlist$fps_bottom_elev <- as.numeric(
        read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "numeric", 
            range = paste0("L", quickset_row)
    ))
    outlist$fps_max_elev <- as.numeric(
        read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "numeric",
            range = paste0("BB", quickset_row)
    ))
    ro_surv <- as.character(
        read_excel(na=character(), trim_ws = F, 
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "text",
            range = paste0("M", quickset_row)
    ))
    outlist$ro_surv <- ifelse(tolower(ro_surv) == "table", "table",as.numeric(ro_surv))
        # This may be either a point value or the word "Table"
    turb_surv <- as.character(read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "text",
            range = paste0("N", quickset_row)
    ))
    outlist$turb_surv <- ifelse(tolower(turb_surv) == "table", "table",as.numeric(turb_surv))
    spill_surv <- as.character(read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "text", 
            range = paste0("O", quickset_row)
    ))
    outlist$spill_surv <- ifelse(tolower(spill_surv) == "table", "table",as.numeric(spill_surv))
    fps_surv <- as.character(read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE, col_types = "text",
            range = paste0("P", quickset_row)
    ))
    outlist$fps_surv <- ifelse(tolower(fps_surv) == "table", "table",as.numeric(fps_surv))
    # Section on gate methods - need to split to allow for different lengths 
    #   of inputs (i.e., blanks are removed by read_excel, need to accommodate this)
    ro_gatemethod = read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE,
            range = paste0("R", quickset_row)
    )
    ro_gatemethod <- ifelse(all(dim(ro_gatemethod)==c(0,0)),
        NA, ro_gatemethod)
    turb_gatemethod = read_excel(na=character(), trim_ws = F,  
        infile, sheet = "QuickSets", col_names = FALSE,
        range = paste0("S", quickset_row)
    )
    # Have to modify the code here to handle NAs
    turb_gatemethod <- ifelse(all(dim(turb_gatemethod)==c(0,0)),
        NA, turb_gatemethod)
    spill_gatemethod = read_excel(na=character(), trim_ws = F,  
        infile, sheet = "QuickSets", col_names = FALSE,
        range = paste0("T", quickset_row)
    )
    spill_gatemethod <- ifelse(all(dim(spill_gatemethod)==c(0,0)),
        NA, spill_gatemethod)
    fps_gatemethod = read_excel(na=character(), trim_ws = F,  
        infile, sheet = "QuickSets", col_names = FALSE,
        range = paste0("U", quickset_row)
    )
    fps_gatemethod <- ifelse(all(dim(fps_gatemethod)==c(0,0)),
        NA, fps_gatemethod)
    outlist$gate_methods <- data.frame(
        ro_gatemethod = as.character(ro_gatemethod),
        turb_gatemethod = as.character(turb_gatemethod),
        spill_gatemethod = as.character(spill_gatemethod),
        fps_gatemethod = as.character(fps_gatemethod)
    )
    colnames(outlist$gate_methods) <- c("ro_gatemethod", "turb_gatemethod", 
        "spill_gatemethod", "fps_gatemethod")
    outlist$fps_route_effectiveness <- read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE,
            range = paste0("AP", quickset_row, ":AZ", quickset_row), col_types = "numeric"
    )
    outlist$weir_start_date <- 
        read_excel(na=character(), trim_ws = F, col_types = "date",
            infile, sheet = "QuickSets", col_names = FALSE,
            range = paste0("BC", quickset_row)
    )
    outlist$weir_end_date <- read_excel(na=character(), trim_ws = F,
        col_types = "date",  
        infile, sheet = "QuickSets", col_names = FALSE,
        range = paste0("BD", quickset_row)
    )
    dpe_x_range <- read_excel(na=character(), trim_ws = F,
        col_types = "text",
        infile, sheet = "QuickSets", col_names = FALSE,
        range = paste0("AK", quickset_row, ":AN",quickset_row)
    )
    if(all(is.na(dpe_x_range))){
        # Just in case no 'X' is provided in the DPE selection portion of the quickset,
        #   default to the baseline DPE values
        warning("Nothing in the 'Effectiveness \"x\" columns' of the QuickSets FBW sheet.\nAssuming you want to use the baseline case - if not, please fill in the values and try again (or input by hand).")
        outlist$dpe_x_position <- 1
    } else if("X" %in% dpe_x_range) {
        outlist$dpe_x_position <- which(toupper(dpe_x_range)=="X")
    } else {
        # Otherwise, use the position of the one which is not NA but throw a warning
        outlist$dpe_x_position <- which(!(is.na(dpe_x_range)))
        indicator_character <- as.character(dpe_x_range[outlist$dpe_x_position])
        warning(paste0(
            "No \"x\" or \"X\" entered in the 'Effectiveness \"x\" columns' of the QuickSets FBW sheet.\nUsing ", indicator_character, " as indicator of the column you want to use (position ", outlist$dpe_x_position, ")."))
    }
    outlist
}

# As above, but designed to read in EIS wide ressim data
#   New parameters:
# prefix: 
# range (optional, defaults to that in FBW workbooks): what range are the data contained within?
loadResSim_EIS <- function(infile, prefix = "", range = "A7:BW372", wide = FALSE) {
    sheetnames <- readxl::excel_sheets(path = infile)
    sheetnames <- sheetnames[grepl(pattern = paste0("^", prefix), x = sheetnames)]
    ressim_list <- lapply(sheetnames, readxl::read_excel, path = infile,
            col_names = TRUE,
            range = range
        )
    names(ressim_list) <- sheetnames
    if (wide) {
        for (l in seq_along(ressim_list)) {
            print(names(ressim_list)[l])
            current_colname <- tolower(gsub(x = gsub(x = names(ressim_list)[l], 
                    pattern = paste0(prefix, "-"), 
                    replacement = ""), 
                pattern = "-", 
                replacement = "_"))
            # if (names(ressim_list)[l] == "elev") {
            #     current_colname <- "elev"
            # } else {
            #     current_colname <- paste0(names(ressim_list)[[l]], "_flow")
            # }
            df_tmp <- ressim_list[[l]]
            df_out <- data.frame(
                tidyr::pivot_longer(df_tmp, cols = !("Date"),
                names_to = "year", values_to = current_colname,
                # names_prefix = "X", 
                values_drop_na = FALSE))
            lubridate::year(df_out$Date) <- as.numeric(df_out$year)
            ressim_list[[l]] <- df_out %>%
                select(-"year")
        }
    }
    ressim_data <- Reduce(function(x, y)
        merge(x, y, by = "Date"),
        ressim_list)
    ressim_data
}