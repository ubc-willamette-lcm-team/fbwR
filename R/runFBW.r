#' Loads FBW data from a defined template spreadsheet in XLSX format. 
#' @param template_file File path to an Excel spreadsheet with standardized 
#' inputs
#' @param ressim_file File path to an Excel spreadsheet with ResSim inputs
#' @param ressim_wide Are ResSim data in typical wide format? Defaults to TRUE,
#' which assumes that each column in the dataframe contains data for a single
#' year, with each row representing a single day in the 365-day year. 
#' 
#' @export

runFBW <- function(ressim_file, template_file, ressim_wide = TRUE) {
  ressim_data <- loadResSim(infile = ressim_file, wide = ressim_wide)
  param_list <- loadFromTemplate(template_file = template_file)
  # Distribute fish population into daily passing populations
  fish_dailydist_flow <- distributeFishDaily(ressim_df, param_list = param_list)
  # Calculate DPE
  dpe <- fetchDPE(fish_dailydist, resv_data = resvdata, quickset_data = qsdata)
  # Calculate 
  # Can do a quick multiplication now to multiply DPE by approaching
  
  
  fish_dailydist$approaching_daily_postDPE <- dpe$dam_passage*fish_dailydist$approaching_dailyDistribution
  
  
  fish_distributed <- distributeFish_outlets(fish_dailydist, resvdata, qsdata)
# Additional parameter options are:
#   water_year_types - a data frame of water year types in the period of record (PoR) 
#   temp_splits - a dataframe of temperature splits in each day of the PoR
# See example 2, Dexter, for temperature regulation processes

# Calculate survival rates from flow data, including distribution of fish 
#   through gates in multi-gate outlets
route_survival_rates <- distributeFlow_Survival_gates(
  fish_dailydist, qsdata, resvdata
)

fish_passage_survival <- route_survival_rates %>% 
  mutate(
    passage_survRO = ro_survival * F.RO,
    passage_survTurb = turb_survival * F.turb_flow,
    passage_survSpill = spill_survival * F.spill_flow,
    passage_survFPS = fps_survival * F.FPS,
    passage_survAllRoutes = passage_survRO + passage_survTurb +
      passage_survSpill + passage_survFPS
  )

summary <- fish_passage_survival %>% 
    # R cannot handle year-less dates, so create a dummy column
    # for the sake of aggregation and summary
    mutate(groupingDate=Date)
}