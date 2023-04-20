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
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @import lubridate
#' 
#' @export

loadResSim <- function(infile, wide = TRUE) {
    
    sheetnames <- readxl::excel_sheets(path = infile)
    
    elev <- data.frame(readxl::read_excel(na=character(), trim_ws = F,  infile,
        sheet = sheetnames[grep(pattern = "-elev$", x = tolower(sheetnames))],
        col_names = TRUE, range = "A7:BW372"))
    colnames(elev)[1] <- "Date"
    
    outflow <- data.frame(readxl::read_excel(na=character(), trim_ws = F,  infile,
        sheet = sheetnames[grep(pattern = "-out$", x = tolower(sheetnames))],
        col_names = TRUE, range = "A7:BW372"))
    colnames(outflow)[1] <- "Date"
    
    turbine <- data.frame(readxl::read_excel(na=character(), trim_ws = F,  infile,
        sheet = sheetnames[grep(pattern = "-ph$", x = tolower(sheetnames))],
        col_names = TRUE, range = "A7:BW372"))
    colnames(turbine)[1] <- "Date"
    
    RO <- data.frame(readxl::read_excel(na=character(), trim_ws = F,  infile,
        sheet = sheetnames[grep(pattern = "-ro$", x = tolower(sheetnames))],
        col_names = TRUE, range = "A7:BW372"))
    colnames(RO)[1] <- "Date"
    spill <- data.frame(readxl::read_excel(na=character(), trim_ws = F,  infile,
        sheet = sheetnames[grep(pattern = "-spill$", x = tolower(sheetnames))],
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
