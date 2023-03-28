#' Loads FBW data from a defined template spreadsheet in XLSX format. 
#' @param template_file File path to an Excel spreadsheet with standardized 
#' inputs
#' @import read_excel
#' @import dplyr
#' @import lubridate

#' @export
#' 
#' 
library(here)
loadFromTemplate <- function(template_file) {
  sheetnames <- readxl::excel_sheets(path = template_file)
  
  # Check if all of the sheets are there
  stopifnot({
      # Check for sheet names
      all(c(
        "alt_description", "route_specifications",
        "route_effectiveness", "route_dpe",
        "monthly_runtiming", "ro_surv", "ro_elevs",
        "turb_surv", "spill_surv", "temp_dist", "water_year_types") %in% 
        sheetnames)
  })

  # Includes information about the alternative being modelled
  alt_desc <- readxl::read_excel(path = template_file,
    sheet = "alt_description", skip = 5,
    na = character(), trim_ws = FALSE, col_names = TRUE, col_types = "text") %>%
    # Remove the definition of the parameter
    select(-c(definition, `R class`))
  alt_desc_list <- list(alt_desc$value)[[1]]
  names(alt_desc_list) <- alt_desc$parameter_name

  # Information about the routes in the dam
  route_specs <- data.frame(t(readxl::read_excel(path = template_file,
    sheet = "route_specifications", skip = 5,
    # Read in as text, change later
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    # Remove the definition of the parameter
    dplyr::select(-definition)) %>%
    janitor::row_to_names(., row_number = 1))
  suppressWarnings(route_specs[,1:5] <- apply(route_specs[,1:5], 2, as.numeric))

  # Route effectiveness data
  route_eff <- readxl::read_excel(path = template_file,
    sheet = "route_effectiveness", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "numeric")
  # Dam passage efficiency data
  route_dpe <- readxl::read_excel(path = template_file,
    sheet = "route_dpe", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = c(
      "numeric", "text", "numeric", "numeric", "numeric", "numeric"))
  monthly_runtiming <- readxl::read_excel(path = template_file,
    sheet = "monthly_runtiming", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = c("date", 
      "numeric", "numeric"))
  ro_surv <- readxl::read_excel(path = template_file,
    sheet = "ro_surv", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  ro_elevs <- readxl::read_excel(path = template_file,
    sheet = "ro_elevs", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  turb_surv <- readxl::read_excel(path = template_file,
    sheet = "turb_surv", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  spill_surv <- readxl::read_excel(path = template_file,
    sheet = "spill_surv", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  ## Still have to read in the temperature distribution data
  # Just include in the Excel file like FBW Excel heh
  temp_dist <- readxl::read_excel(path = template_file,
    sheet = "temp_dist", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, 
    col_types = c("date", "numeric", "numeric", "numeric", "numeric"))
  water_year_types <- data.frame(readxl::read_excel(path = template_file,
    sheet = "water_year_types", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text"))
  
  return(list(
    "alt_desc" = alt_desc_list,
    "route_specs" = route_specs,
    "route_eff" = route_eff,
    "route_dpe" = route_dpe,
    "monthly_runtiming" = monthly_runtiming,
    "ro_surv" = ro_surv,
    "ro_elevs" = ro_elevs,
    "turb_surv" = turb_surv,
    "spill_surv" = spill_surv,
    "temp_dist" = temp_dist,
    "water_year_types" = water_year_types
  ))
}
