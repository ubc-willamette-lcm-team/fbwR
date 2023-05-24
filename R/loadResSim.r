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
#' 
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @import lubridate
#' 
#' @export

loadResSim <- function(infile, wide = TRUE,
  elevsheet = NULL, outflowsheet = NULL, powerhousesheet = NULL,
  rosheet = NULL, spillsheet = NULL) {
    sheetnames <- readxl::excel_sheets(path = infile)
    elev <- data.frame(readxl::read_excel(na = character(), trim_ws = F,  infile,
        sheet = ifelse(
            is.null(elevsheet),
            sheetnames[grep(pattern = "-elev$", x = tolower(sheetnames))],
            elevsheet),
        col_names = TRUE, range = "A7:BW372"))
    colnames(elev)[1] <- "Date"
    
    outflow <- data.frame(readxl::read_excel(na = character(), trim_ws = F, 
      infile,
      sheet = ifelse(is.null(outflowsheet),
        sheetnames[grep(pattern = "-out$", x = tolower(sheetnames))],
        outflowsheet), 
      col_names = TRUE, range = "A7:BW372"))
    colnames(outflow)[1] <- "Date"
    
    turbine <- data.frame(readxl::read_excel(na = character(), trim_ws = F,
      infile,
      sheet = ifelse(is.null(powerhousesheet),
        sheetnames[grep(pattern = "-ph$", x = tolower(sheetnames))],
        powerhousesheet),
      col_names = TRUE, range = "A7:BW372"))
    colnames(turbine)[1] <- "Date"
    
    RO <- data.frame(readxl::read_excel(na = character(), trim_ws = F,
      infile,
      sheet = ifelse(is.null(rosheet),
        sheetnames[grep(pattern = "-ro$", x = tolower(sheetnames))],
        rosheet),
      col_names = TRUE, range = "A7:BW372"))
    colnames(RO)[1] <- "Date"
    spill <- data.frame(readxl::read_excel(na = character(), trim_ws = F,  infile,
      sheet = ifelse(is.null(spillsheet),
        sheetnames[grep(pattern = "-spill$", x = tolower(sheetnames))],
        spillsheet),
      col_names = TRUE, range = "A7:BW372"))
    colnames(spill)[1] <- "Date"
    # Set up this list for quick formatting/comprehension later
    ressim_list <- list(elev, outflow, turbine, RO, spill)
    names(ressim_list) <- c("elev", "outflow", "turb", "RO", "spill")

    # All of these are in wide format
    # Convert all from wide (columns = years) to long (column=flow rates,
    #   rows=Dates with specified years)
    if (wide) {
        # Iterate across the items in ressim_list and pivot to long format
        for (l in seq_along(ressim_list)) {
            if (names(ressim_list)[l] == "elev") {
                current_colname <- "elev"
            } else {
                current_colname <- paste0(names(ressim_list)[[l]], "_flow")
            }
            df_tmp <- ressim_list[[l]]
            df_out <- data.frame(
                tidyr::pivot_longer(df_tmp,
                cols = starts_with("X"),
                names_to = "year",
                names_prefix = "X",
                values_to = current_colname,
                values_drop_na = FALSE))
            lubridate::year(df_out$Date) <- as.numeric(df_out$year)
            ressim_list[[l]] <- df_out %>%
                select(-"year")
        }
    }
    ### DATA PROCESSING - merge ressim flow data:
    # Merge all of Elev, Outflow, PH, Spill, and URO by Date
    # Replaces column stacking, instead using Date as a key for pairing data
    ressim_data <- Reduce(function(x, y)
        merge(x, y),
        ressim_list)
    ressim_data
}
