# Quick start guide example to using the FBW R package
# 	Created by Mairin Deith (m.deith@oceans.ubc.ca)
#	Last modified March 29, 2022
#
# Note that this script reads information from a filled-in FBW 
#	  Excel workbook and references sheets within the workbook
# If a workbook is not already filled in, parameters will
#	  have to be entered by hand according to the format 
#	  expected by the script.
#
# See the FBW-R-User Guide for more details

### First, read libraries (these are also used by the FBW R script)
#   dplyr for data manipulation and lubridate for time-date objects
library(dplyr)
library(lubridate)
library(here)

# Read in the FBW functions contained in the R script - this makes the functions 
# 	available to the R session, much like calling "library(...)" on a CRAN package
# Replace PATH/TO/FUNCTIONS with a real file path on your computer

# source("PATH/TO/FUNCTIONS/FBWFunctions.r")
lapply(
  list.files(here("Nov2022_allsubbasin_hatcherymitigation", "src"),
    pattern = "\\.r$|\\.R$"),
    function(X) {
      # cat(X)
      source(file = here("Nov2022_allsubbasin_hatcherymitigation", "src", X),
        echo = FALSE)
    }
  )

############################################################################
# First example: Cougar dam, NAA, no temperature operations
############################################################################

# Original workbook file
workbook_file <- here( # "PATH/TO/FBW/WORKBOOKS/FBW-Chinook-Fry_CGR_Alt1.xlsm"
  "..", "InputParams", "Jan11_2022_UpdatedFBW_mod", "FBW_01_10_2022",
  "Chinook_RawForProcessing", "No action", "CGR",
  "FBW-Chinook-Fry_CGR_NAA.xlsm")

# New addition: load ResSim data directly from template
ressim_file <- here("17_01_2023_TemplateLoader",
  "CGR_ResSim_NAA.xlsm")
template_file <- here("17_01_2023_TemplateLoader", 
  "template_data_entry_06012023.xlsx")

# workbook_file now points to the location of a single FBW workbook file, and will
#	read necessary parameters from that workbook
# Load ResSim data; they are in wide format so set wide = T
ressim_df_in <- loadResSim_FBWworkbook(infile = ressim_file, wide = TRUE)

# Remove "excess" rows after the date - these rows will have no values
ressim_df <- ressim_df_in[which(!(is.na(rowSums(ressim_df_in[, 2:6])))), ]

# Load in template-version of data (instead of QuicksetData/ResvData)
alt_params <- loadTemplate()


### OLD VERSION
## To read from QuickSet and ResvData sheets, need cell positions:
# Which column is reservoir data stored in?
resv_col <- "I"
# Which row is quickset data stored in?
quickset_row <- "154" # NAA

# Provided with a workbook file location and a column, loadResvData_FBWworkbook
#   reads in reservoir-specific operation data
resvdata <- loadResvData_FBWworkbook(infile = workbook_file, resv_col)
# loadQuicksetData_FBWworkbook does the same as loadResvData for data in the
#   quicksets workbook
qsdata <- loadQuicksetData_FBWworkbook(infile = workbook_file, quickset_row)

# Note: There may be errors above, as many Excel workbooks have dates instead of
#   numbers in some cells, these will be converted to numbers


############################################################################
# Begin FBW steps
############################################################################

# Starting here, the dataframe will contain one row per date in the period of
# record, with new columns added as fish are routed and survive passage
fish_dailydist_flow <- distributeFish_daily(ressim_df,
  resv_data = resvdata,
  quickset_data = qsdata,
  # Determine distribution type based on quickset data
  type = "flow") %>% 
  mutate(type = "flow")

fish_dailydist_flat <- distributeFish_daily(ressim_df,
  resv_data = resvdata,
  quickset_data = qsdata,
  # Determine distribution type based on quickset data
  type = "flat") %>%
  mutate(type = "flat")

fish_dailydist_both <- rbind(
    fish_dailydist_flat, fish_dailydist_flow
)

library(ggplot2)

ggplot(fish_dailydist_both %>% filter(year(Date) == 2019) %>%
  filter(Month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")),
  aes(colour = type)) + 
  geom_line(aes(x = Date, y = approaching_dailyDistribution), lwd = 2, alpha = 0.5) + 
  theme_minimal()

# DPE has to be determined from reservoir elevation data, using the "fetchDPE" 
# function
dpe <- fetchDPE(fish_dailydist,
  resv_data = resvdata,
  quickset_data = qsdata)

ggplot(dpe %>% filter(year(Date) == 2019) %>%
  filter(Month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))) + 
  geom_line(aes(x = Date, y = scale(dam_passage)), color = "darkorange", lwd = 2) + 
  geom_line(aes(x = Date, y = scale(elev)), color = "grey15", lwd = 2) + 
  theme_minimal()

ggplot(dpe %>% filter(year(Date) == 2019)) + 
  geom_line(aes(x = elev, y = dam_passage), lwd = 2) + 
  theme_minimal()

dpe$dam_passage

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

# View the summary by calling the data frame
summary

############################################################################
# Second example: Dexter dam, WITH temperature operations
############################################################################
# Many of these steps mimic those from the first example, Cougar, with the
#   addition of temperature-influenced routing of fish

workbook_file <- "PATH/TO/FBW/WORKBOOKS/FBW-Chinook-Subs_DET_Alt4.xlsm"

ressim_df_in <- suppressMessages(
  loadResSim_FBWworkbook(infile = workbook_file, wide = TRUE)
)
# Remove "excess" rows after the date
ressim_df <- ressim_df_in[which(!(is.na(rowSums(ressim_df_in[,2:6])))),]

resv_col <- "M"
quickset_row <- "240" # Alt 4

resvdata <- suppressMessages(
  loadResvData_FBWworkbook(infile = workbook_file, resv_col)
)
qsdata <- suppressMessages(
  loadQuicksetData_FBWworkbook(infile = workbook_file, quickset_row)
)

fish_dailydist <- distributeFish_daily(ressim_df,
  resv_data = resvdata,quickset_data = qsdata,
# Determine distribution type based on quickset data
    type = ifelse(qsdata$fish_with_flow == "Y", "flow", "flat"))

# fish_dailydist$dpe <- fetchDPE(fish_dailydist, resv_data = resvdata, quickset_data = qsdata)

fish_dailydist <- fish_dailydist %>%
    mutate(
      # Look up the DPE, supplying reservoir and quickset_data
        dpe = fetchDPE(fish_dailydist, resv_data = resvdata, quickset_data = qsdata)$dam_passage,
        # 
        approaching_daily_postDPE = approaching_dailyDistribution * dpe)

### LOAD IN DATA FOR TEMPERATURE CONTROLS

# First, check that temp_dist in the quickset data is turned on (== 'y'), 
# else don't bother and set water_year_types and temp_dist_df to NULL
# (the FBW-R functions know to ignore temperature distribution when these 
#  data frames == NULL)

# Use the reservoir name for lookups
resv <- "DETROIT"

if(tolower(qsdata$temp_dist) == "y"){
  # Supplying the reservoir name to a "switch" function will provide the range
  #   in the Excel workbook where the temperature distribution data is stored  
  temp_dist_range <- switch(
      resv,
      "HILLS CREEK" = "CL4:CO17",
      "LOOKOUT POINT" = "CQ4:CT17",
      "DETROIT" = "CV4:CY22",
      "GREEN PETER" = "DA4:DD15")
    # Make a data frame of water year types 
    water_year_types_raw <- data.frame(
        data.table::transpose(
            read_excel(workbook_file, sheet = "TEMP-SPLIT",
            range = "B1:BW7", col_names = FALSE)))
    water_year_types <- data.frame(
        type = as.character(water_year_types_raw[, 1]), 
        year = as.numeric(water_year_types_raw[,7]))
    temp_dist_df_raw <- data.frame(read_excel(workbook_file,
        # Data are in the "TEMP-SPLIT" sheet
        sheet ='TEMP-SPLIT',
        # Indicate the column data types
        col_types=c("date", rep("numeric", 3)),
        # Range indicates cell ranges, keep column names
        range=temp_dist_range, col_names=TRUE))[-1,] # this negative index removes row 1, a "second header" of sorts
    temp_dist_df <- temp_dist_df_raw %>%
        # rename(NewName = OldName)
        rename(Date ='...1',ABUNDANT = Cool.Wet,ADEQUATE = Normal,DEFICIT = Hot.Dry) %>%
        # Create INSUFFICIENT category
        mutate(INSUFFICIENT = (ADEQUATE + DEFICIT)/2) %>%
        # re-arrange with relocate() so they are in increasing order
        relocate(INSUFFICIENT, .before=DEFICIT)
} else if(tolower(qsdata$temp_dist) == "n") {
    water_year_types <- NULL
    temp_dist_df <- NULL
} else {
    warning("Temperature distribution must be either 'N' or 'Y'")
}

fish_distributed <- distributeFish_outlets(fish_dailydist, resvdata, qsdata, 
    water_year_types = water_year_types, temp_splits = temp_dist_df)

raw_survival_rates <- distributeFlow_Survival_gates(
  fish_distributed, 
  qsdata,
  resvdata)

# Apply multipliers
fish_passage_survival <- raw_survival_rates %>% 
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

# View the summary by calling the data frame
summary
