#' Loads FBW data from a defined template spreadsheet in XLSX format. 
#' @param template_file File path to an Excel spreadsheet with standardized 
#' inputs
#' @import readxl
#' @import dplyr
#' @import lubridate
#' 
#' @export

loadFromTemplate <- function(template_file) {
  sheetnames <- readxl::excel_sheets(path = template_file)
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
  alt_desc <- readxl::read_excel(path = template_file,
    sheet = "alt_description", skip = 6,
    na = character(), trim_ws = FALSE, col_names = TRUE, col_types = "text") %>%
    # Remove the definition of the parameter
    select(-c(definition))
  alt_desc_list <- list(alt_desc$value)[[1]]
  names(alt_desc_list) <- alt_desc$parameter_name

  # Information about the routes in the dam
  route_specs <- data.frame(t(readxl::read_excel(path = template_file,
    sheet = "route_specifications", skip = 6,
    # Read in as text, change later
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    # Remove the definition of the parameter
    dplyr::select(-definition)))
  # The first row can be set as column names, then removed
  colnames(route_specs) <- route_specs[1, ]
  route_specs <- route_specs[-1, ]
  suppressWarnings(
    route_specs[, c(1, 2, 5, 6, 7)] <- apply(route_specs[, c(1, 2, 5, 6, 7)], 
      2, as.numeric))
  # Route effectiveness data
  route_eff <- readxl::read_excel(path = template_file,
    sheet = "route_effectiveness", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "numeric")
  # Dam passage efficiency data
  route_dpe <- readxl::read_excel(path = template_file,
    sheet = "route_dpe", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = c(
      "numeric", "text", "numeric", "numeric", "numeric", "numeric"))
  monthly_runtiming <- readxl::read_excel(path = template_file,
    sheet = "monthly_runtiming", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = c("date",
      "numeric", "numeric"))
  ro_surv_table <- readxl::read_excel(path = template_file,
    sheet = "ro_surv_table", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  ro_elevs <- readxl::read_excel(path = template_file,
    sheet = "ro_elevs", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  turb_surv_table <- readxl::read_excel(path = template_file,
    sheet = "turb_surv_table", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  spill_surv_table <- readxl::read_excel(path = template_file,
    sheet = "spill_surv_table", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  fps_surv_table <- readxl::read_excel(path = template_file,
    sheet = "fps_surv_table", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  ## Still have to read in the temperature distribution data
  # Just include in the Excel file like FBW Excel heh
  temp_dist <- readxl::read_excel(path = template_file,
    sheet = "temp_dist", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, 
    col_types = c("date", "numeric", "numeric", "numeric", "numeric"))
  water_year_types <- data.frame(readxl::read_excel(path = template_file,
    sheet = "water_year_types", skip = 6,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text"))
  
  return(list(
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
}
