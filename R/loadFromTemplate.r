#' Loads FBW data from a defined template spreadsheet in XLSX format. 
#' @param template_file File path to an Excel spreadsheet with standardized 
#' inputs
#' @import read_excel
#' @import dplyr
#' @import lubridate

#' @export

template_file <- here("17_01_2023_TemplateLoader", 
  "template_data_entry_06012023.xlsx")

loadFromTemplate <- function(template_file) {
  sheetnames <- readxl::excel_sheets(path = template_file)
  stopifnot({
      # Check for sheet names
      all(c(
        "alt_description", "route_specifications",
        "route_effectiveness", "route_dpe",
        "monthly_runtiming", "ro_surv", "ro_elevs",
        "turb_surv", "spill_surv", "temp_dist", "water_year_types") %in% sheetnames)
  })
  outlist <- list()
  alt_desc <- readxl::read_excel(path = template_file,
    sheet = "alt_description", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    # Remove the definition of the parameter
    select(-definition, `R class`)
  route_specs <- readxl::read_excel(path = template_file,
    sheet = "route_specifications", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    # Remove the definition of the parameter
    select(-definition)
  route_eff <- readxl::read_excel(path = template_file,
    sheet = "route_effectiveness", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  route_dpe <- readxl::read_excel(path = template_file,
    sheet = "route_dpe", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")
  monthly_runtiming <- readxl::read_excel(path = template_file,
    sheet = "monthly_runtiming", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    janitor::clean_names()
  ro_surv <- readxl::read_excel(path = template_file,
    sheet = "ro_surv", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    janitor::row_to_names(5)
  ro_elevs <- readxl::read_excel(path = template_file,
    sheet = "ro_elevs", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    janitor::row_to_names(5)
   turb_surv <- readxl::read_excel(path = template_file,
    sheet = "turb_surv", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    janitor::row_to_names(5)
  spill_surv <- readxl::read_excel(path = template_file,
    sheet = "spill_surv", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text") %>%
    janitor::row_to_names(5)
  ## Still have to read in the temperature distribution data
  # Just include in the Excel file like FBW Excel heh
  temp_dist <- readxl::read_excel(path = template_file,
    sheet = "temp_dist", skip = 5,
    na = character(), trim_ws = F, col_names = TRUE, 
    col_types = c("date", "numeric", "numeric", "numeric")) %>%
    janitor::row_to_names(5)
  water_year_types <- data.frame(readxl::read_excel(path = template_file,
    sheet = "water_year_types",
    na = character(), trim_ws = F, col_names = TRUE, col_types = "text")) %>%
    dplyr::mutate(
      split_abundant = split_CW,
      split_adequate = split_N,
      split_deficit = split_HD,
      split_insufficient = mean(split_N, split_HD)
    )
}
