#' Load FBW parameters from an Excel workbook, requiring only the name of the 
#' reservoir and the name of the quickset. If none are given, the reservoir
#' name and quickset name are read from the "Route Survival Model" landing page
#' of the FBW workbook.
#' @param fbw_excel Character string referencing the location of the Excel file 
#' to be read. Parameters will be compiled from the ResvData and QuickSets 
#' sheets.
#' @param reservoir (Optional) Name of the reservoir being modelled. Used to 
#' lookup which entries of the ResvData and QuickSets should be loaded into FBW.
#' If not provided, `reservoir` is read from cell B6 of the Route Survival Model
#' sheet in `fbw_excel`.
#' @param quickset (Optional) Name of the quickset being modelled. Used to 
#' lookup which entries of the QuickSets sheet should be loaded into FBW.
#' If not `reservoir` and `quickset` are not provided, `quickset` is read from 
#' cell B7 of the Route Survival Model sheet in `fbw_excel`.
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @export

loadFromWorkbook <- function(fbw_excel, reservoir = NULL, quickset = NULL) {
  # If reservoir is NULL, let's read both the quickset and reservoir
  reservoir <- ifelse(is.null(reservoir),
    # If null, read it in
    unlist(as.character(suppressMessages(
      readxl::read_excel(fbw_excel, col_names = FALSE,
        sheet = "Route Survival Model", range = "B6")))),
    reservoir)
  quickset <- ifelse(is.null(quickset),
    # If null, read it in
    unlist(as.character(suppressMessages(
      readxl::read_excel(fbw_excel, col_names = FALSE,
        sheet = "Route Survival Model", range = "B7")))),
    quickset)
  # Start reading in parameters
  resvsheet <- readxl::read_excel(fbw_excel,
    sheet = "ResvData")
  suppressMessages(qset <- readxl::read_excel(fbw_excel, sheet = "QuickSets"))
  stopifnot(rs_model_resv %in% na.omit(unique(qset$Reservoir)))
  # Subset the quickset data rows to those which correspond to the reservoirs
  # Find those which are NOT empty: These are the breaks in rows between
  #  reservoir quickset "chunks"
  qs_breaks <- which(!(is.na(qset$Reservoir)))
  names(qs_breaks) <- na.omit(unique(qset$Reservoir))
  # Now, we can use the reservoir to look up the indices
  qset_range <- qs_breaks[c(
      # Start at the row which corresponds to the reservoir
          which(names(qs_breaks) == rs_model_resv),
      # End at the row of the next reservoir, minus 1
          (which(names(qs_breaks) == rs_model_resv) + 1)
      )]
  # Don't read the first row of the next reservoir's quickset block
  qset_range[2] <- qset_range[2] - 1
  qset_subset <- qset[seq(from = qset_range[1], to = qset_range[2], by = 1), ]
  stopifnot(!is.na(quickset) && quickset %in% qset_subset$`Quick Set Name`)
  qset_subset <- qset_subset[which(qset_subset$`Quick Set Name` == quickset), ]
  ### Build the output list, following template format
  ### ALT DESCRIPTION
  alt_desc_list <- list(
    fps_alternative = qset_subset$`Fish Passage Alt`,
    nets = qset_subset$`Nets`,
    collector = qset_subset$`Collector`, # Quickset
    rereg = qset_subset$`Rereg?`, # Quickset
    rereg_mortality = qset_subset$`Rereg Mort.`,
    fish_with_flow = qset_subset$`Fish w Flow`,
    # Lookup the name of the DPE column 
    # that is being used based on the position of the "X" cell,
    dpe_column_name = c("baseline_dpe", "fsc_dpe", "fss_dpe", "weir_dpe")[
    #  yeesh
      which(c(qset_subset$`...37`, qset_subset$`...38`,
        qset_subset$`...39`, qset_subset$`...40`) == "X")
    ],
    temp_dist = qset_subset$`Temp_Dist?`,
    fps_q_max = qset_subset$`Fish Passage Q`,
    fps_bottom_elev = qset_subset$`Fish Pass bottom`,
    fps_max_elev = qset_subset$`topElevFP`,
    ro_surv = tolower(qset_subset$`Surv_RO`),
    turb_surv = tolower(qset_subset$`Surv_Turb`),
    spill_surv = tolower(qset_subset$`Surv_Spill`),
    fps_surv = tolower(qset_subset$`Surv_FP`),
    #!# Gotta do some weird date manipulation?
    weir_start_date = qset_subset$startWeirDate,
    weir_end_date = qset_subset$startEndDate
  )
  route_specs <- data.frame(
    # Add NA here, there is none for FPS YET!!! In quickset
    max_flow = c(as.numeric(unlist(resvsheet[8:10,
      which(colnames(resvsheet) == reservoir)])),
      qset_subset$`Fish Passage Q`),
    # Add NA here, there is none for FPS YET!!! In quickset
    bottom_elev = c(as.numeric(unlist(resvsheet[13:15,
      which(colnames(resvsheet) == reservoir)])), qset_subset$`Fish Pass bottom`),
    n_gates = as.numeric(unlist(resvsheet[17:20,
      which(colnames(resvsheet) == reservoir)])),
    min_flow = as.numeric(unlist(resvsheet[22:25,
      which(colnames(resvsheet) == reservoir)])),
    target_flow = as.numeric(unlist(resvsheet[27:30,
      which(colnames(resvsheet) == reservoir)])),
    normally_used = c(unlist(resvsheet[32:34,
      which(colnames(resvsheet) == reservoir)]), NA),
    gate_method = c(
      qset_subset$`Gate_Method_RO`,
      qset_subset$`Gate_Method_Turb`,
      qset_subset$`Gate_Method_Spill`,
      qset_subset$`Gate_Method_FP`
  ))
  rownames(route_specs) <- c("Spill", "RO", "Turb", "FPS")

  route_eff <- data.frame(
    q_ratio = seq(0, 1, by = 0.1),
    Spill = as.numeric(unlist(resvsheet[48:58,
      which(colnames(resvsheet) == reservoir)])),
    RO = as.numeric(unlist(resvsheet[60:70,
      which(colnames(resvsheet) == reservoir)])),
    Turb = as.numeric(unlist(resvsheet[72:82,
      which(colnames(resvsheet) == reservoir)]))
  )
  # DPE range includes some number of elevation inputs
  resvnames <- which(!is.na(resvsheet$`Raw Data Sheet Names`))
  names(resvnames) <- resvsheet$`Raw Data Sheet Names`[resvnames]
  dpe_idx <- list(
    elev = c(
      resvnames[which(names(resvnames) == "Resv. Elevs.")] + 1,
      resvnames[which(names(resvnames) == "Elev. Description")] - 1),
    elev_desc = c(
      resvnames[which(names(resvnames) == "Elev. Description")] + 1,
      resvnames[which(names(resvnames) == "Baseline Dam Passage")] - 1),
    baseline_dpe = c(
      resvnames[which(names(resvnames) == "Baseline Dam Passage")] + 1,
      resvnames[which(names(resvnames) == "FSS Dam Passage")] - 1),
    fss_dpe = c(
      resvnames[which(names(resvnames) == "FSS Dam Passage")] + 1,
      resvnames[which(names(resvnames) == "FSC Dam Passage")] - 1),
    fsc_dpe = c(
      resvnames[which(names(resvnames) == "FSC Dam Passage")] + 1,
      resvnames[which(names(resvnames) == "Weir Box Dam Passage")] - 1),
    weir_dpe = c(
      resvnames[which(names(resvnames) == "Weir Box Dam Passage")] + 1,
      resvnames[which(names(resvnames) == "Baseline RE")] - 2)
    )
  route_dpe <- data.frame(
    # Index the sheet according to the indices generated above
    elev = as.numeric(unlist(resvsheet[
      dpe_idx$elev[1]:dpe_idx$elev[2],
      which(colnames(resvsheet) == reservoir)])),
    elev_description = unlist(resvsheet[
      dpe_idx$elev_desc[1]:dpe_idx$elev_desc[2],
      which(colnames(resvsheet) == reservoir)]),
    baseline_dpe = as.numeric(unlist(resvsheet[
      dpe_idx$baseline_dpe[1]:dpe_idx$baseline_dpe[2],
      which(colnames(resvsheet) == reservoir)])),
    fss_dpe = as.numeric(unlist(resvsheet[
      dpe_idx$fss_dpe[1]:dpe_idx$fss_dpe[2],
      which(colnames(resvsheet) == reservoir)])),
    fsc_dpe = as.numeric(unlist(resvsheet[
      dpe_idx$fsc_dpe[1]:dpe_idx$fsc_dpe[2],
      which(colnames(resvsheet) == reservoir)])),
    weir_dpe = as.numeric(unlist(resvsheet[
      dpe_idx$weir_dpe[1]:dpe_idx$weir_dpe[2],
      which(colnames(resvsheet) == reservoir)]))
  )
  ### Fish approaching
  fishapproach_idx <- c(
    resvnames[which(names(resvnames) == "Baseline Fish approaching")] + 1,
    resvnames[which(names(resvnames) == "Route Effectiveness Spillway")] - 1)
  monthly_runtiming <- data.frame(
    date = as.Date(as.numeric(resvsheet[
      fishapproach_idx[1]:fishapproach_idx[2],]$`Raw Data Sheet Names`),
      origin = "1899-12-30"),
    approaching_baseline = as.numeric(
      unlist(resvsheet[fishapproach_idx[1]:fishapproach_idx[2],
      which(colnames(resvsheet) == reservoir)])),
    # The alternative specific approaching comes from the quickset
    approaching_alternative = as.numeric(
      qset_subset[, which(colnames(qset_subset) %in% c(
          "% Fish approaching", paste0("...", seq(24, 34, by = 1)))
      )]
    ))
  # Survival
  ro_surv <- na.omit(data.frame(
    flow = as.numeric(unlist(resvsheet[
      (resvnames["RO Surv Q"] + 1):
      (resvnames["RO Low Pool Surv"] - 1),
      which(colnames(resvsheet) == reservoir)])),
    ro_surv_low = as.numeric(unlist(resvsheet[
      (resvnames["RO Low Pool Surv"] + 1):
      (resvnames["RO High Pool Surv"] - 1),
      which(colnames(resvsheet) == reservoir)])),
    ro_surv_high = as.numeric(unlist(resvsheet[
      (resvnames["RO High Pool Surv"] + 1):
      (resvnames["Turb Surv Q"] - 1),
      which(colnames(resvsheet) == reservoir)]))
  ))
  ro_elevs <- data.frame(
    param = c("ro_lower_elev", "ro_upper_elev"),
    value = as.numeric(c(
      resvsheet[resvnames["RO Low Pool"],
        which(colnames(resvsheet) == reservoir)],
      resvsheet[resvnames["RO High Pool"],
        which(colnames(resvsheet) == reservoir)]
      ))
  )

  turb_surv <- na.omit(data.frame(
    flow = as.numeric(unlist(resvsheet[
      (resvnames["Turb Surv Q"] + 1):
      (resvnames["Turb Surv"] - 1),
      which(colnames(resvsheet) == reservoir)])),
    turb_surv = as.numeric(unlist(resvsheet[
      (resvnames["Turb Surv"] + 1):
      (resvnames["Spill Surv Q"] - 1),
      which(colnames(resvsheet) == reservoir)]))
  ))
  spill_surv <- na.omit(data.frame(
    flow = as.numeric(unlist(resvsheet[
      (resvnames["Spill Surv Q"] + 1):
      (resvnames["Spill Surv"] - 1),
      which(colnames(resvsheet) == reservoir)])),
    spill_surv = as.numeric(unlist(resvsheet[
      (resvnames["Spill Surv"] + 1):
      (resvnames["FP Surv Q"] - 1),
      which(colnames(resvsheet) == reservoir)]))
  ))
  fps_surv <- na.omit(data.frame(
    flow = as.numeric(unlist(resvsheet[
      (resvnames["FP Surv Q"] + 1):
      (resvnames["FP Surv"] - 1),
      which(colnames(resvsheet) == reservoir)])),
    fps_surv = as.numeric(unlist(resvsheet[
      (resvnames["FP Surv"] + 1):
      (resvnames["Baseline Survival Percentages (for graph)"] - 2),
      which(colnames(resvsheet) == reservoir)]))
  ))
  ## Temperature data and water year types
  tempsplit <- readxl::read_excel(fbw_excel, sheet = "TEMP-SPLIT")
  # wow this is a lot of work to modify column names
  year_types <- sapply(colnames(tempsplit)[-1], function(X) {
    substring(X, first = 1,
      last = regexpr(X, pattern = "...", fixed = TRUE)[1] - 1)})
  # There will be some extra columns which begin with ".." now, remove these
  #   after searching with regular expressions via grep()
  year_types <- year_types[-which(year_types == "")]
  names(year_types) <- as.numeric(tempsplit[6, 2:(1 + length(year_types))])
  water_year_types <- data.frame(
    year = names(year_types),
    type = unlist(year_types)
  )
  # Temperature distributions need to be looked up from
  # a series of columns
  which(tempsplit[2,] == "Active Reservoir:") + 2
  available_temps <- na.omit(unique(unlist(tempsplit[2, 
    (which(tempsplit[2,] == "Active Reservoir:") + 2):
    (which(tempsplit[2,] == "Green Peter"))
    ])))
  if (!(reservoir %in% available_temps)) {
    warning("No temperature data provided for reservoir ", reservoir)
    temp_dist <- data.frame(
      Date = NA,
      ABUNDANT = NA,
      ADEQUATE = NA,
      DEFICIT = NA,
      INSUFFICIENT = NA
    )
  } else {
    idx <- which(tempsplit[2, ] == reservoir)
    temp_dist <- data.frame(
      Date = as.Date(as.numeric(unlist(tempsplit[6:40, idx])),
        origin = "1899-12-30"),
      coolwet = as.numeric(unlist(tempsplit[6:40, idx + 1])),
      normal = as.numeric(unlist(tempsplit[6:40, idx + 2])),
      hotdry = as.numeric(unlist(tempsplit[6:40, idx + 3]))
    ) %>%
    mutate(
      ABUNDANT = coolwet,
      ADEQUATE = normal,
      DEFICIT = (normal + hotdry) / 2,
      INSUFFICIENT = hotdry
    ) %>%
    select(-c(coolwet, normal, hotdry))
  }
  # Compile into named list
  params <- list(
    "alt_desc" = alt_desc_list,
    "route_specs" = route_specs,
    "route_eff" = route_eff,
    "route_dpe" = route_dpe,
    "monthly_runtiming" = monthly_runtiming,
    "ro_surv" = ro_surv,
    "ro_elevs" = ro_elevs,
    "turb_surv" = turb_surv,
    "fps_surv" = fps_surv,
    "spill_surv" = spill_surv,

    "temp_dist" = temp_dist,
    "water_year_types" = water_year_types,
    
    "reservoir" = reservoir,
    "quickset" = quickset
  )
  ## Also have to read in ResSims
  
}

# params <- loadFromWorkbook("C:/Users/mdeith/OneDrive - UBC/Willamette/FishBenefitWorkbook/ExcelFBW_InputParams/FBW-Chinook-Fry_CGR_Alt4_8-20-2021_MDTinkeringMonthly.xlsm")
# params
# params <- loadFromWorkbook()
################################################################################
#' Load parameters required for FBW-R from a pre-filled Excel workbook using
#' two superceded functions: loadResvData_FBWworkbook and
#' loadQuickset_FBWworkbook
#' @param infile Character path to an Excel file that corresponds to a 
#' parameterized FBW workbook
#' @param resv_column Character of the column in the Excel Workbook's "ResvData"
#' sheet that corresponds to the current dam.

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
        ro_surv_lower <- read_excel(na=character(), trim_ws = F,  
            infile, sheet="ResvData", col_names=FALSE, 
            range=paste0(resv_column,"204:", resv_column,"223"))
        ro_surv_upper <- read_excel(na=character(), trim_ws = F,  
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
        turb_surv <- read_excel(na=character(), trim_ws = F,  
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
        spill_surv <- read_excel(na=character(), trim_ws = F,  
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
    ro_gatemethod <- read_excel(na=character(), trim_ws = F,  
            infile, sheet = "QuickSets", col_names = FALSE,
            range = paste0("R", quickset_row)
    )
    ro_gatemethod <- ifelse(all(dim(ro_gatemethod)==c(0,0)),
        NA, ro_gatemethod)
    turb_gatemethod <- read_excel(na=character(), trim_ws = F,  
        infile, sheet = "QuickSets", col_names = FALSE,
        range = paste0("S", quickset_row)
    )
    # Have to modify the code here to handle NAs
    turb_gatemethod <- ifelse(all(dim(turb_gatemethod)==c(0,0)),
        NA, turb_gatemethod)
    spill_gatemethod <- read_excel(na=character(), trim_ws = F,  
        infile, sheet = "QuickSets", col_names = FALSE,
        range = paste0("T", quickset_row)
    )
    spill_gatemethod <- ifelse(all(dim(spill_gatemethod)==c(0,0)),
        NA, spill_gatemethod)
    fps_gatemethod <- read_excel(na=character(), trim_ws = F,  
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
        warning("Nothing in the 'Effectiveness /"x/" columns' of the QuickSets FBW sheet./nAssuming you want to use the baseline case - if not, please fill in the values and try again (or input by hand).")
        outlist$dpe_x_position <- 1
    } else if("X" %in% dpe_x_range) {
        outlist$dpe_x_position <- which(toupper(dpe_x_range)=="X")
    } else {
        # Otherwise, use the position of the one which is not NA but throw a warning
        outlist$dpe_x_position <- which(!(is.na(dpe_x_range)))
        indicator_character <- as.character(dpe_x_range[outlist$dpe_x_position])
        warning(paste0(
            "No /"x/" or /"X/" entered in the 'Effectiveness /"x/" columns' of the QuickSets FBW sheet./nUsing ", indicator_character, " as indicator of the column you want to use (position ", outlist$dpe_x_position, ")."))
    }
    outlist
}
