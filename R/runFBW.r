#' Loads FBW data from a defined template spreadsheet in XLSX format. 
#' @param template_file File path to an Excel spreadsheet with standardized 
#' inputs
#' @param ressim_file File path to an Excel spreadsheet with ResSim inputs
#' @param ressim_wide Are ResSim data in typical wide format? Defaults to TRUE,
#' which assumes that each column in the dataframe contains data for a single
#' year, with each row representing a single day in the 365-day year. 
#' 
#' @export

runFBW <- function(ressim_file, template_file, ressim_wide = TRUE, summarize = FALSE) {

  ressim_data <- loadResSim(infile = ressim_file, 
    wide = ressim_wide)  
  param_list <- loadFromTemplate(template_file = template_file)
  
  # Distribute fish population into daily passing populations
  fish_daily_distribution <- distributeFishDaily(ressim_df,
    param_list = param_list)

  # Calculate DPE
  dpe <- fetchDPE(fish_daily_distribution, param_list = param_list)
  # Multiply approaching population by dam passage efficiency
  fish_dailydist$approaching_daily_postDPE <- dpe$dam_passage*fish_dailydist$approaching_dailyDistribution
  fish_distributed <- distributeFish_outlets(fish_postDPE = fish_dailydist, param_list = param_list)
  # Additional parameter options are:
  #   water_year_types - a data frame of water year types in the period of record (PoR) 
  #   temp_splits - a dataframe of temperature splits in each day of the PoR
  # See example 2, Dexter, for temperature regulation processes

  # Calculate survival rates from flow data, including distribution of fish 
  #   through gates in multi-gate outlets
  route_survival_rates <- distributeFlow_Survival_gates(
    fish_distributed_outlets = fish_distributed, 
    param_list = param_list)

  # Perform final calculations, multiplying survival by the proportion of fish 
  #   in outlet X (F.X)
  fish_passage_survival <- route_survival_rates %>% 
    mutate(
      passage_survRO = ro_survival * F.RO,
      passage_survTurb = turb_survival * F.turb_flow,
      passage_survSpill = spill_survival * F.spill_flow,
      passage_survFPS = fps_survival * F.FPS,
      passage_survAllRoutes = passage_survRO + passage_survTurb +
        passage_survSpill + passage_survFPS
    )
    if (summarize == FALSE) {
      return(fish_passage_survival)
    } else {
      summary <- fish_passage_survival %>% 
          # R cannot handle year-less dates, so create a dummy column
          # for the sake of aggregation and summary
          mutate(groupingDate=Date)
        # Force all dates to have the same (arbitrary) year
        lubridate::year(summary$groupingDate) <- 2000
    
      summary <- summary %>% 
        # dplyr lets you summarize by grouping variables:
        #   Here, group by month and "dummy" date
        group_by(groupingDate, Month) %>% 
        summarize( 
          # Calculate the mean survival through each passage
          dailyMeanRO=mean(passage_survRO*100, na.rm=T),
          dailyMeanTurb=mean(passage_survTurb*100, na.rm=T),
          dailyMeanSpill=mean(passage_survSpill*100, na.rm=T),
          dailyMeanFPS=mean(passage_survFPS*100, na.rm=T)
        ) %>% 
        ungroup() %>% # Remove grouping, re-group to only Month
        group_by(Month) %>% 
        summarise( 
          # Now add together daily means into monthly sums
          meanFPS=sum(dailyMeanFPS, na.rm=TRUE),
          meanTurb=sum(dailyMeanTurb, na.rm=TRUE),
          meanRO=sum(dailyMeanRO, na.rm=TRUE),
          meanSpill=sum(dailyMeanSpill, na.rm=TRUE)
        )
      return(summary)
    }
}
