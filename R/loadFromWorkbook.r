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
#' @return A list of parameters required to run the FBW model in R.
#'
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
  resvsheet <- suppressMessages(readxl::read_excel(fbw_excel,
    sheet = "ResvData"))
  qset <- suppressWarnings(suppressMessages(readxl::read_excel(fbw_excel, sheet = "QuickSets")))
  stopifnot(reservoir %in% na.omit(unique(qset$Reservoir)))
  # Subset the quickset data rows to those which correspond to the reservoirs
  # Find those which are NOT empty: These are the breaks in rows between
  #  reservoir quickset "chunks"
  qs_breaks <- which(!(is.na(qset$Reservoir)))
  names(qs_breaks) <- na.omit(unique(qset$Reservoir))
  # Now, we can use the reservoir to look up the indices
  qset_range <- qs_breaks[c(
      # Start at the row which corresponds to the reservoir
          which(names(qs_breaks) == reservoir),
      # End at the row of the next reservoir, minus 1
          (which(names(qs_breaks) == reservoir) + 1)
      )]
  # Don't read the first row of the next reservoir's quickset block
  qset_range[2] <- qset_range[2] - 1
  qset_subset <- qset[seq(from = qset_range[1], to = qset_range[2], by = 1), ]
  stopifnot(!is.na(quickset) && quickset %in% qset_subset$`Quick Set Name`)
  qset_subset <- qset_subset[which(qset_subset$`Quick Set Name` == quickset), ]
  # The column of route effectiveness to be used is indicated with an "X"
  #   Find it
  route_eff_x_column <- which(toupper(
    c(qset_subset$`...37`, qset_subset$`...38`,
        qset_subset$`...39`, qset_subset$`...40`)) == "X")
  # if(identical(route_eff_x_column, character(0)))
  ### Build the output list, following template format
  ### ALT DESCRIPTION
  alt_desc_list <- list(
    scenario_name = qset_subset$`Quick Set Name`,
    scenario_description = qset_subset$`Description`,
    fp_alternative = qset_subset$`Fish Passage Alt`,
    nets = qset_subset$`Nets`,
    collector = qset_subset$`Collector`, # Quickset
    fps_max_elev = qset_subset$`topElevFP`,
    rereg = qset_subset$`Rereg?`, # Quickset
    rereg_mortality = qset_subset$`Rereg Mort.`,
    fish_with_flow = qset_subset$`Fish w Flow`,
    # Lookup the name of the DPE column 
    # that is being used based on the position of the "X" cell,
    use_temp_dist = qset_subset$`Temp_Dist?`,
    dpe_column_name = c("baseline_dpe", "col1_dpe", "col2_dpe", "col3_dpe")[
      route_eff_x_column
    ],
    # fps_q_max = qset_subset$`Fish Passage Q`,
    # fps_bottom_elev = qset_subset$`Fish Pass bottom`,
    # ro_surv = tolower(qset_subset$`Surv_RO`),
    # turb_surv = tolower(qset_subset$`Surv_Turb`),
    # spill_surv = tolower(qset_subset$`Surv_Spill`),
    # fps_surv = tolower(qset_subset$`Surv_FP`),
    #!# Gotta do some weird date manipulation?
    weir_start_date = qset_subset$startWeirDate,
    weir_end_date = qset_subset$endWeirDate
  )
  ### There may be differences between these values and those defined in the 
  ###   Route Survival Model sheet
  qset_rsm_alt <- t(suppressMessages(readxl::read_excel(fbw_excel,
    sheet = "Route Survival Model", range = "A11:B16", col_names = F)))
  qset_rsm_surv <- t(suppressMessages(readxl::read_excel(fbw_excel,
    sheet = "Route Survival Model", range = "A17:E25", col_names = F)))
  route_eff_x_column_eff <- suppressMessages(readxl::read_excel(fbw_excel,
    sheet = "Effectiveness", range = "K2:N2", col_names = F))
  route_eff_x_idx <- which(toupper(route_eff_x_column_eff) == "X")
  weir_dates <- as.vector(suppressMessages(readxl::read_excel(fbw_excel,
    sheet = "Route Survival Model", range = "G12:G13", col_names = FALSE, 
    na = "NA")))
  if (length(weir_dates) == 0) {
    weir_dates <- list(NA, NA)
  }
  # Create parameter list
  alt_desc_list_rsm <- list(
    fp_alternative = qset_rsm_alt[2, 1],
    nets = qset_rsm_alt[2, 2],
    collector = qset_rsm_alt[2, 3], # Quickset
    rereg = qset_rsm_alt[2, 4], # Quickset
    rereg_mortality = suppressMessages(readxl::read_excel(fbw_excel,
      sheet = "Route Survival Model", range = "E14", col_names = F)),
    fish_with_flow = qset_rsm_alt[2, 5],
    # Lookup the name of the DPE column 
    # that is being used based on the position of the "X" cell,
    dpe_column_name = c("baseline_dpe", "col1_dpe", "col2_dpe", "col3_dpe")[
      route_eff_x_idx
    ],
    use_temp_dist = qset_rsm_alt[2, 6],
    # 
    # # fps_q_max = ifelse(!is.na(as.numeric(qset_rsm_surv[5, 2])),
    # #   as.numeric(qset_rsm_surv[5, 2]), qset_rsm_surv[5, 2]),
    # fps_bottom_elev = ifelse(!is.na(as.numeric(qset_rsm_surv[5, 3])),
    #   as.numeric(qset_rsm_surv[5, 3]), qset_rsm_surv[5, 3]),
    fps_max_elev = NA,
    # ro_surv = ifelse(!is.na(as.numeric(qset_rsm_surv[2, 4])),
    #   as.numeric(qset_rsm_surv[2, 4]), qset_rsm_surv[2, 4]),
    # turb_surv = ifelse(!is.na(as.numeric(qset_rsm_surv[3, 4])),
    #   as.numeric(qset_rsm_surv[3, 4]), qset_rsm_surv[3, 4]),
    # spill_surv = ifelse(!is.na(as.numeric(qset_rsm_surv[4, 4])),
    #   as.numeric(qset_rsm_surv[4, 4]), qset_rsm_surv[4, 4]),
    # fps_surv = ifelse(!is.na(as.numeric(qset_rsm_surv[5, 4])),
    #   as.numeric(qset_rsm_surv[5, 4]), qset_rsm_surv[5, 4]),
    #!# Gotta do some weird date manipulation?
    weir_start_date = weir_dates[[1]][1],
    weir_end_date = weir_dates[[1]][2]
  )
  if(
    !(alt_desc_list_rsm$fp_alternative == alt_desc_list$fp_alternative) ||
    !(alt_desc_list_rsm$nets == alt_desc_list$nets) ||
    !(alt_desc_list_rsm$collector == alt_desc_list$collector) ||
    !(alt_desc_list_rsm$rereg == alt_desc_list$rereg) || 
    !(alt_desc_list_rsm$rereg_mortality == alt_desc_list$rereg_mortality) || 
    !(alt_desc_list_rsm$fish_with_flow == alt_desc_list$fish_with_flow) ||
    !(alt_desc_list_rsm$dpe_column_name == alt_desc_list$dpe_column_name) ||
    !(alt_desc_list_rsm$use_temp_dist == alt_desc_list$use_temp_dist) 
    # !(alt_desc_list_rsm$weir_start_date == alt_desc_list$weir_start_date) || 
    # !(alt_desc_list_rsm$weir_end_date == alt_desc_list$weir_end_date)
  ) {
    warning("Route specifications are mismatched between ResvData, QuickSets, and Route Survival Model sheets! Using values defined in the Route Survival Model.")
    alt_desc_list <- alt_desc_list_rsm
  }
  resv_normallyused <- c(unlist(resvsheet[31:34,
    which(colnames(resvsheet) == reservoir)]))
  rsm_normallyused <- qset_rsm_surv[2:5, 9]
  if(!all(resv_normallyused == rsm_normallyused, na.rm = T)) {
    warning("'Normally used' specifications are mismatched between ResvData and Route Survival Model sheets! Using values defined in the Route Survival Model.")
    normally_used_check <- rsm_normallyused
  } else {
    normally_used_check <- resv_normallyused
  }
  route_specs <- data.frame(
    # Add NA here, there is none for FPS YET!!! In quickset
    max_flow = c(as.numeric(unlist(resvsheet[8:10,
      which(colnames(resvsheet) == reservoir)])),
      qset_subset$`Fish Passage Q`),
    # Add NA here, there is none for FPS YET!!! In quickset
    bottom_elev = c(as.numeric(unlist(resvsheet[12:14,
      which(colnames(resvsheet) == reservoir)])),
      qset_subset$`Fish Pass bottom`),
    passage_surv_rate = c(
      qset_subset$`Surv_RO`,
      qset_subset$`Surv_Turb`,
      qset_subset$`Surv_Spill`,
      qset_subset$`Surv_FP`
    ),
    gate_method = c(
      qset_subset$`Gate_Method_RO`,
      qset_subset$`Gate_Method_Turb`,
      qset_subset$`Gate_Method_Spill`,
      qset_subset$`Gate_Method_FP`
    ),
    n_gates = as.numeric(unlist(resvsheet[16:19,
      which(colnames(resvsheet) == reservoir)])),
    min_flow = as.numeric(unlist(resvsheet[21:24,
      which(colnames(resvsheet) == reservoir)])),
    target_flow = as.numeric(unlist(resvsheet[26:29,
      which(colnames(resvsheet) == reservoir)])),
    normally_used = normally_used_check
    # c(unlist(resvsheet[31:33,
    #   which(colnames(resvsheet) == reservoir)]), NA),
  )
  rownames(route_specs) <- c("RO", "Turb", "Spill", "FPS")

  route_eff <- data.frame(
    q_ratio = seq(0, 1, by = 0.1),
    Spill = as.numeric(unlist(resvsheet[48:58,
      which(colnames(resvsheet) == reservoir)])),
    FPS = as.numeric(unlist(
      qset_subset[which(colnames(qset_subset) == "Route Effectiveness nums"):
        (which(colnames(qset_subset) == "Route Effectiveness nums") + 10)]
    )),
    RO = as.numeric(unlist(resvsheet[60:70,
      which(colnames(resvsheet) == reservoir)])),
    Turb = as.numeric(unlist(resvsheet[72:82,
      which(colnames(resvsheet) == reservoir)]))
  )
  # DPE range includes some number of elevation inputs
  resvnames <- which(!is.na(resvsheet$`Raw Data Sheet Names`))
  names(resvnames) <- resvsheet$`Raw Data Sheet Names`[resvnames]
  # Index of the rows where dpe data "live" in the ResV sheet
  dpe_idx_resv <- list(
    elev = c(
      resvnames[which(names(resvnames) == "Resv. Elevs.")] + 1,
      resvnames[which(names(resvnames) == "Elev. Description")] - 1),
    elev_desc = c(
      resvnames[which(names(resvnames) == "Elev. Description")] + 1,
      resvnames[which(names(resvnames) == "Baseline Dam Passage")] - 1),
    baseline_dpe = c(
      resvnames[which(names(resvnames) == "Baseline Dam Passage")] + 1,
      resvnames[which(names(resvnames) == "FSS Dam Passage")] - 1),
    col1_dpe = c(
      resvnames[which(names(resvnames) == "FSS Dam Passage")] + 1,
      resvnames[which(names(resvnames) == "FSC Dam Passage")] - 1),
    col2_dpe = c(
      resvnames[which(names(resvnames) == "FSC Dam Passage")] + 1,
      resvnames[which(names(resvnames) == "Weir Box Dam Passage")] - 1),
    col3_dpe = c(
      resvnames[which(names(resvnames) == "Weir Box Dam Passage")] + 1,
      resvnames[which(names(resvnames) == "Baseline RE")] - 2)
    )
  route_dpe_resv <- data.frame(
    # Index the sheet according to the indices generated above
    elev = as.numeric(unlist(resvsheet[
      dpe_idx_resv$elev[1]:dpe_idx_resv$elev[2],
      which(colnames(resvsheet) == reservoir)])),
    elev_description = unlist(resvsheet[
      dpe_idx_resv$elev_desc[1]:dpe_idx_resv$elev_desc[2],
      which(colnames(resvsheet) == reservoir)]),
    baseline_dpe = as.numeric(unlist(resvsheet[
      dpe_idx_resv$baseline_dpe[1]:dpe_idx_resv$baseline_dpe[2],
      which(colnames(resvsheet) == reservoir)])),
    col1_dpe = as.numeric(unlist(resvsheet[
      dpe_idx_resv$col1_dpe[1]:dpe_idx_resv$col1_dpe[2],
      which(colnames(resvsheet) == reservoir)])),
    col2_dpe = as.numeric(unlist(resvsheet[
      dpe_idx_resv$col2_dpe[1]:dpe_idx_resv$col2_dpe[2],
      which(colnames(resvsheet) == reservoir)])),
    col3_dpe = as.numeric(unlist(resvsheet[
      dpe_idx_resv$col3_dpe[1]:dpe_idx_resv$col3_dpe[2],
      which(colnames(resvsheet) == reservoir)]))
  )
  ### DPE from the Effectiveness tab
  effsheet <- suppressMessages(readxl::read_excel(fbw_excel,
    sheet = "Effectiveness", range = "G2:N16"))
  route_dpe_eff <- data.frame(
    elev = effsheet$`Elevs.`,
    elev_description = effsheet$Descriptions,
    baseline_dpe = effsheet[, 5],
    col1_dpe = effsheet[, 6],
    col2_dpe = effsheet[, 7],
    col3_dpe = effsheet[, 8]
  )
  colnames(route_dpe_eff) <- c("elev", "elev_description", "baseline_dpe",
    "col1_dpe", "col2_dpe", "col3_dpe")
  if( !identical(route_dpe_eff$baseline_dpe, route_dpe_resv$baseline_dpe) ||
    !identical(route_dpe_eff$col1_dpe, route_dpe_resv$col1_dpe) ||
    !identical(route_dpe_eff$col2_dpe, route_dpe_resv$col2_dpe) ||
    !identical(route_dpe_eff$col3_dpe, route_dpe_resv$col3_dpe)
  ) {
    warning("Route DPE mismatches between ResvData and Effectiveness tabs! Using values defined in the Route Effectiveness sheet.")
  }
  ### Fish approaching
  fishapproach_idx <- c(
    resvnames[which(names(resvnames) == "Baseline Fish approaching")] + 1,
    resvnames[which(names(resvnames) == "Route Effectiveness Spillway")] - 1)
  monthly_runtiming_resv <- data.frame(
    Date = as.Date(as.numeric(resvsheet[
      fishapproach_idx[1]:fishapproach_idx[2], ]$`Raw Data Sheet Names`),
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
  ### Fish approaching FROM the Route Survival Model tab
  rsm_timing <-  suppressMessages(readxl::read_excel(fbw_excel,
    sheet = "Route Survival Model", range = "A36:E48", col_names = T))
  monthly_runtiming_rsm <- data.frame(rsm_timing %>%
    rename(
      Date = Month,
      approaching_baseline = Baseline1,
      approaching_alternative = `Estimated with Downstream Passage2`
    ) %>%
    select(Date, approaching_baseline, approaching_alternative))
  if( !identical(monthly_runtiming_rsm$approaching_baseline, monthly_runtiming_resv$approaching_baseline) ||
    !identical(monthly_runtiming_rsm$approaching_alternative, monthly_runtiming_resv$approaching_alternative)
  ) {
    warning("Monthly run timing mismatches between ResvData and Route Survival Model sheets! Using values defined in the Route Survival Model sheet.")
  }
  # Survival tables
  ro_surv_table <- na.omit(data.frame(
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
  turb_surv_table <- na.omit(data.frame(
    flow = as.numeric(unlist(resvsheet[
      (resvnames["Turb Surv Q"] + 1):
      (resvnames["Turb Surv"] - 1),
      which(colnames(resvsheet) == reservoir)])),
    turb_surv = as.numeric(unlist(resvsheet[
      (resvnames["Turb Surv"] + 1):
      (resvnames["Spill Surv Q"] - 1),
      which(colnames(resvsheet) == reservoir)]))
  ))
  spill_surv_table <- na.omit(data.frame(
    flow = as.numeric(unlist(resvsheet[
      (resvnames["Spill Surv Q"] + 1):
      (resvnames["Spill Surv"] - 1),
      which(colnames(resvsheet) == reservoir)])),
    spill_surv = as.numeric(unlist(resvsheet[
      (resvnames["Spill Surv"] + 1):
      (resvnames["FP Surv Q"] - 1),
      which(colnames(resvsheet) == reservoir)]))
  ))
  fps_surv_table <- na.omit(data.frame(
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
  tempsplit <- suppressMessages(
    readxl::read_excel(fbw_excel, sheet = "TEMP-SPLIT"))
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
  which(tempsplit[2, ] == "Active Reservoir:") + 2
  available_temps <- na.omit(unique(unlist(tempsplit[2,
    (which(tempsplit[2, ] == "Active Reservoir:") + 2):
    (which(tempsplit[2, ] == "Green Peter"))
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
      DEFICIT = hotdry,
      INSUFFICIENT = (normal + hotdry) / 2
    ) %>%
    select(-c(coolwet, normal, hotdry))
  }
  # Compile into named list
  list(
    "alt_desc" = alt_desc_list,
    "route_specs" = route_specs,
    "route_eff" = route_eff,
    "route_dpe" = route_dpe_eff,
    "monthly_runtiming" = monthly_runtiming_rsm,
    "ro_surv_table" = ro_surv_table,
    "ro_elevs" = ro_elevs,
    "turb_surv_table" = turb_surv_table,
    "fps_surv_table" = fps_surv_table,
    "spill_surv_table" = spill_surv_table,
    "temp_dist" = temp_dist,
    "water_year_types" = water_year_types,
    "reservoir" = reservoir,
    "quickset" = quickset
  )
}
