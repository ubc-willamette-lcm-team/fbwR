#' Load data from an Excel ResSim file. 
#' @param infile A character string indicating the full file path to an
#' Excel workbook of HEC-ResSim flow outputs. Each of the following worksheets
#' must be present (`*` indicates a prefix that will be ignored when the file
#' is read in): `*_ELEV`: elevation of the pool in feet; `*_OUT`: 
#' total outflow in cubic feet per second (CFS); `*_PH`: flow through the 
#' powerhouse turbines (total) in CFS; `*_RO`: flow through the regulating
#' outlet in CFS; `*_SPILL`: flow through the spillway in CFS. In each sheet,
#' there should be one column for each year in the period of record and one
#' row for each day in the year. 
#' @param wide Are data in wide format (e.g., year across columns, one row for each 
#' day in the 365-day year, Feb 29 in leap years are ignored)? Defaults to TRUE,
#' following standard input format of Res-Sim spreadsheets.
#' @param elevsheet  Name of Excel sheet containing ResSim results for pool
#' elevation. If not provided, defaults to `NULL` and the function will use th
#' first sheet ending in "-"
#' @param outflowsheet Name of Excel sheet containing ResSim results for flow
#' through all outlets for each day in the period of record. If not provided,
#' defaults to `NULL` and the function will use the first sheet ending in "-out"
#' @param powerhousesheet Name of Excel sheet containing ResSim results for flow
#' through the powerhouse route. If not provided, defaults to `NULL` and the
#' function will use the first sheet ending in "-ph"
#' @param rosheet Name of Excel sheet containing ResSim results for flow
#' through the regulating outlet route. If not provided, defaults to `NULL` and
#' the function will use the first sheet ending in "-ro"
#' @param spillsheet Name of Excel sheet containing ResSim results for flow
#' through the spillway route. If not provided, defaults to `NULL` and
#' the function will use the first sheet ending in "-spill"
#' @param year_override Useful for debugging; should mismatches in years be 
#' overridden by the script? This forces the years to match the defined period
#' of record defined in `forced_year_range`. Defaults to FALSE, such that errors are returned 
#' when there are year mismatches.
#' @param forced_year_range Useful for debugging; forcibly set the years in the
#' period of record; all ResSim and other FBW inputs that do not conform to the 
#' period of record are forced to have this span.
#' 
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#' @importFrom lubridate year
#' 
#' @export

loadResSim <- function(infile, wide = TRUE,
  elevsheet = NULL, outflowsheet = NULL, powerhousesheet = NULL,
  rosheet = NULL, spillsheet = NULL, year_override = FALSE, 
  forced_year_range = NULL) {
    sheetnames <- readxl::excel_sheets(path = infile)
    elev <- data.frame(readxl::read_excel(na = character(), trim_ws = F,  infile,
        sheet = ifelse(
            is.null(elevsheet),
            sheetnames[grep(pattern = "-elev$", x = tolower(sheetnames))],
            elevsheet),
      col_names = TRUE, 
      col_types = c("date", rep("numeric", 74)),
      range = "A7:BW372"))
    colnames(elev)[1] <- "Date"
    
    outflow <- data.frame(readxl::read_excel(na = character(), trim_ws = F, 
      infile,
      sheet = ifelse(is.null(outflowsheet),
        sheetnames[grep(pattern = "-out$", x = tolower(sheetnames))],
        outflowsheet), 
      col_names = TRUE, 
      col_types = c("date", rep("numeric", 74)),
      range = "A7:BW372"))
    colnames(outflow)[1] <- "Date"
    
    turbine <- data.frame(readxl::read_excel(na = character(), trim_ws = F,
      infile,
      sheet = ifelse(is.null(powerhousesheet),
        sheetnames[grep(pattern = "-ph$", x = tolower(sheetnames))],
        powerhousesheet),
      col_names = TRUE, 
      col_types = c("date", rep("numeric", 74)),
      range = "A7:BW372"))
    colnames(turbine)[1] <- "Date"
    
    RO <- data.frame(readxl::read_excel(na = character(), trim_ws = F,
      infile,
      sheet = ifelse(is.null(rosheet),
        sheetnames[grep(pattern = "-ro$", x = tolower(sheetnames))],
        rosheet),
      col_names = TRUE, 
      col_types = c("date", rep("numeric", 74)),
      range = "A7:BW372"))
    colnames(RO)[1] <- "Date"
    spill <- data.frame(readxl::read_excel(na = character(), trim_ws = F,  infile,
      sheet = ifelse(is.null(spillsheet),
        sheetnames[grep(pattern = "-spill$", x = tolower(sheetnames))],
        spillsheet),
      col_names = TRUE, 
      col_types = c("date", rep("numeric", 74)),
      range = "A7:BW372"))
    colnames(spill)[1] <- "Date"
    # Set up this list for quick formatting/comprehension later
    ressim_list <- list(elev, outflow, turbine, RO, spill)
    names(ressim_list) <- c("elev", "outflow", "turb", "RO", "spill")    
    # 
    # All of these are in wide format
    # Convert all from wide (columns = years) to long (column=flow rates,
    #   rows=Dates with specified years)
    if (wide) {
      for (i in 2:length(ressim_list)) {
        # If there is mismatch in the wide-form column names (i.e., years),
        #   enforce year range or stop loading ResSim
          if (!is.null(forced_year_range) && year_override) {
            if (!identical(colnames(ressim_list[[i]]), colnames(ressim_list[[i - 1]]))) {
            warning("Date range mismatch between ",
              names(ressim_list)[i], " and ", names(ressim_list)[i - 1], 
              "; using year range supplied in forced_year_range (", 
              min(forced_year_range), ":", max(forced_year_range), ")")
            }
            colnames(ressim_list[[i]])[2:75] <- paste0("X", forced_year_range)
            colnames(ressim_list[[i - 1]])[2:75] <- paste0("X", forced_year_range)
          # } else {
          } else {
            if (!identical(colnames(ressim_list[[i]]), colnames(ressim_list[[i - 1]]))) {
              stop("Please fix the date range in the input ResSim file, or run `loadResSim` again with `year_override = TRUE` and `forced_year_range` set to the 74-year range represented in the period of record (e.g., `forced_year_range = 1946:2019`)")
          }
        }
      }
      # Iterate across the items in ressim_list and pivot to long format
      for (l in seq_along(ressim_list)) {
          if (names(ressim_list)[l] == "elev") {
              current_colname <- "elev"
          } else {
              current_colname <- paste0(names(ressim_list)[[l]], "_flow")
          }
          df_tmp <- ressim_list[[l]]
          df_out <- data.frame(tidyr::pivot_longer(df_tmp,
              cols = dplyr::starts_with("X"),
              names_to = "year",
              names_prefix = "X",
              values_to = current_colname,
              values_drop_na = FALSE))
          lubridate::year(df_out$Date) <- as.numeric(df_out$year)
          ressim_list[[l]] <-  dplyr::select(df_out, -year)
      }
    }
    ### DATA PROCESSING - merge ressim flow data:
    # Merge all of Elev, Outflow, PH, Spill, and URO by Date
    # Replaces column stacking, instead using Date as a key for pairing data
    ressim_out <- Reduce(function(x, y)
        merge(x, y, by = "Date"), ressim_list)
    ## Check outputs before return    
    # Flow columns should approximately sum to outflow_flow
    ressim_outcheck <- ressim_out %>%
      dplyr::mutate(summed_outflow = turb_flow + RO_flow + spill_flow) %>%
      dplyr::mutate(rounded_matching = dplyr::case_when(
        # Return an error if the difference is more than 0.5 CFS
        abs(summed_outflow - outflow_flow) > 0.5 ~ FALSE,
        TRUE ~ TRUE
      ))
    # If not all matching:
    if(!all(ressim_outcheck$rounded_matching)) {
      nonmatching <- ressim_outcheck[which(ressim_outcheck$rounded_matching == FALSE),] %>%
        dplyr::select(-c(rounded_matching, elev))
      warning("Sum of turbine, RO, and spillway flow do not match outflow flow on the following dates (n = ", nrow(nonmatching), "). Check for year mismatches in the ResSim data. Head of nonmatching shown below:\n",
        paste(capture.output(print(head(nonmatching))), collapse = "\n"))
    }
    attr(ressim_out, "ressim_infile") <- infile
    attr(ressim_out, "year_override") <- year_override
    attr(ressim_out, "forced_year_range") <- ifelse(is.null(forced_year_range), NA, 
      paste0(min(forced_year_range), ":", max(forced_year_range)))
    return(ressim_out)
}
