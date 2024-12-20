#' Loads FBW data from a defined template spreadsheet in XLSX format. 
#' @param file File path to an Excel spreadsheet with standardized 
#' inputs
#' @param year_override Boolean; should the years in the period of record be
#' overwritten by the years in `forced_year_range`? If TRUE, `forced_year_range`
#' should also be provided.
#' @param forced_year_range If `year_override` is TRUE, what years should be 
#' imposed on the ResSim data?
#' 
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export

loadFromTemplate <- function(file, year_override = FALSE, 
  forced_year_range = NULL) {
  sheetnames <- readxl::excel_sheets(path = file)
  # Check if all of the required sheets are present
  #   If not, stops running the program
  stopifnot({
      all(c(
        "alt_description", "route_specifications",
        "route_effectiveness", "route_dpe",
        "monthly_runtiming", "ro_surv_table",
        "turb_surv_table", "spill_surv_table", "fps_surv_table",
        "temp_dist", "water_year_types") %in%
        sheetnames)
  })
  # Includes information about the alternative being modelled
  alt_desc <- readxl::read_excel(path = file,
    sheet = "alt_description", skip = 6,
    na = character(), trim_ws = FALSE, col_names = TRUE, col_types = "text") %>%
    # Remove the definition of the parameter
    dplyr::select(-c(.data$definition))
  alt_desc_list <- list(alt_desc$value)[[1]]
  names(alt_desc_list) <- alt_desc$parameter_name
  # Information about the routes in the dam
  route_specs <- data.frame(t(readxl::read_excel(path = file,
    sheet = "route_specifications", skip = 6,
    # Read in as text, change later
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    # Remove the definition of the parameter
    dplyr::select(-.data$definition)))
  # The first row can be set as column names, then removed
  colnames(route_specs) <- route_specs[1, ]
  route_specs <- route_specs[-1, ]
  #!# Why did I have suppressWarnings() on here?
  # suppressWarnings(
    route_specs[, c(1, 2, 5, 6, 7)] <- apply(route_specs[, c(1, 2, 5, 6, 7)], 
      2, as.numeric)
  # )
  # Route effectiveness data
  route_eff <- readxl::read_excel(path = file,
    sheet = "route_effectiveness", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "numeric")
  # Dam passage efficiency data
  route_dpe <- readxl::read_excel(path = file,
    sheet = "route_dpe", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = c(
      "numeric", "text", "numeric", "numeric", "numeric", "numeric"))
  monthly_runtiming <- readxl::read_excel(path = file,
    sheet = "monthly_runtiming", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = c("date",
      "numeric", "numeric"))
  ro_surv_table <- readxl::read_excel(path = file,
    sheet = "ro_surv_table", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "numeric")
  ro_elevs <- readxl::read_excel(path = file,
    sheet = "ro_elevs", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = c("text", 
    "numeric"))
  turb_surv_table <- readxl::read_excel(path = file,
    sheet = "turb_surv_table", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "numeric")
  spill_surv_table <- readxl::read_excel(path = file,
    sheet = "spill_surv_table", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "numeric")
  fps_surv_table <- readxl::read_excel(path = file,
    sheet = "fps_surv_table", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "numeric")
  ## Still have to read in the temperature distribution data
  # Just include in the Excel file like FBW Excel heh
  temp_dist <- readxl::read_excel(path = file,
    sheet = "temp_dist", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, 
    col_types = c("date", "numeric", "numeric", "numeric", "numeric"))
  water_year_types <- data.frame(readxl::read_excel(path = file,
    sheet = "water_year_types", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text"))
  # Check for years in the water year type data
  if (!is.null(forced_year_range) && year_override) {
    if (!all(as.numeric(water_year_types$year) %in% forced_year_range)) {
      warning("Water year types in the period of record do not match the years in
      `forced year range`, (", min(forced_year_range), ":", max(forced_year_range), ").")
      if (nrow(water_year_types) != length(forced_year_range)) {
        stop("Number of years in `forced_year_range` does not match number of rows in the water year type table")
      } else {
        water_year_types$year <- forced_year_range
      }
    }
  }
  param_list <- return(list(
    "alt_desc" = alt_desc_list,
    "route_specs" = route_specs,
    "route_eff" = route_eff,
    "route_dpe" = route_dpe,
    "monthly_runtiming" = monthly_runtiming,
    "ro_surv_table" = ro_surv_table,
    "ro_elevs" = ro_elevs,
    "turb_surv_table" = turb_surv_table,
    "spill_surv_table" = spill_surv_table,
    "fps_surv_table" = fps_surv_table,
    "temp_dist" = temp_dist,
    "water_year_types" = water_year_types
  ))
  attr(param_list, "template_infile") <- file
  attr(param_list, "year_override") <- year_override
  attr(param_list, "forced_year_range") <- ifelse(is.null(forced_year_range), NA, 
    paste0(min(forced_year_range), ":", max(forced_year_range)))
  return(param_list)
}
