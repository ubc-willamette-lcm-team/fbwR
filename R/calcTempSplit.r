#' Calculate the distribution of flow between the temperature control tower and 
#' the fish passage structure in cases where there is temperature control (FSS)
#' @param ressim_data A dataframe that includes at least Date (in datetime
#' format) and information on daily Outflow (cfs), temperature splits
#' @param water_year_types A dataframe containing "year" and "watertype", one 
#' of "abundant", "adequate", "insufficient", and "deficient"
#' @param temp_dist_df a dataframe containing dated temperature splits (often 
#' month-by-month) based on water year types in the water_year_types dataframe.
#' @return Dataframe of temperature splits in each day (a proportion, 0-1, 
#' reflecting which dates have what percent of the outflow will be redirected
#' for temperature control operations). Flow through the fish passage structure
#' is calculated as outflow * (1 - proportion redirected to temperature control)
#' 
#' @importFrom lubridate %within%
#' @importFrom lubridate year
#' @importFrom lubridate interval
#' @export

calcTempSplit <- function(ressim_data, water_year_types, temp_dist_df) {
    # Create an output data frame
    tempSplitOut <- data.frame(
        Date = as.Date(x = ressim_data$Date, format = "%Y-%m-%d"),
        split = NA
    )
    if (any(is.na(temp_dist_df))) warning("Some rows in the `temp_dist` table include NAs; rows with any NA value are omitted.")
    temp_dist_df <- na.omit(temp_dist_df)
    # lubridate::year(temp_dist_df$Date) <- 2000
    temp_dist_sorted <- temp_dist_df %>%
      dplyr::arrange(Date)
    yr_summary <- table(lubridate::year(temp_dist_sorted$Date))
    if (length(yr_summary) > 1) {
      keepyr <- names(yr_summary)[which(yr_summary == max(yr_summary))]
      temp_dist_sorted <- temp_dist_sorted %>%
        dplyr::filter(lubridate::year(Date) == as.numeric(keepyr))
      warning(paste0(
        "There is more than one year of data in the `temp_dist` parameter table.",
        " Keeping only dates in the most prevalent year (", 
        names(yr_summary)[which(yr_summary == max(yr_summary))], "):\n",
        paste(capture.output(print(temp_dist_sorted)), collapse = "\n")
        ))
    }
    baseline_year <- lubridate::year(temp_dist_sorted$Date[1])
    # Iterate over years
    for (i in 1:length(water_year_types$year)) {
        yr <- as.numeric(water_year_types$year[i])
        yr_diff <- yr - baseline_year
        # Identify year type and corresponding column in temp_dist_sorted
        type <- water_year_types$type[i]
        typeCol <- which(colnames(temp_dist_sorted) == type)
        if (identical(typeCol, integer(0))) {
            stop(paste0("Water year type ", type, " (year ", yr, ") not found in temperature split table"))
        }
        # Find the lower bound for the temperature split data
        lower_date <- temp_dist_sorted$Date[1]
        # Coerce the year to be current (cannot be year-agnostic)
        lubridate::year(lower_date) <- lubridate::year(lower_date) + yr_diff
        # For all days before the lower_date in the first year, 
        # set to the last row of the table
        if (temp_dist_sorted$Date[1] != "2000-01-01") {
          date_interval <- lubridate::interval(paste0(lubridate::year(lower_date), "-01-01"),
            lower_date)
          tempSplitOut$split[which(tempSplitOut$Date %within% 
                date_interval)] <- temp_dist_sorted[nrow(temp_dist_sorted),typeCol]
        }
        origin_date <- lower_date # Make a copy for later
        # Iterate over these rows
        for (r in 2:nrow(temp_dist_sorted)) {
            upper_date <- temp_dist_sorted$Date[r] - 1
            lubridate::year(upper_date) <- lubridate::year(upper_date) + yr_diff
            date_interval <- lubridate::interval(lower_date, upper_date)
            tempSplitOut$split[which(tempSplitOut$Date %within% 
              date_interval)] <- temp_dist_sorted[r-1,typeCol]
            lower_date <- upper_date + 1
        }
        # Now, finish up with the final interval (lower_date to the original date of the next year)
        lubridate::year(origin_date) <- lubridate::year(origin_date) + 1
        # New interval
        date_interval <- lubridate::interval(lower_date, origin_date)
        # Here, 'r' is saved from the iterator above and can be used at its highest value
        tempSplitOut$split[
            which(tempSplitOut$Date %within% date_interval)] <- 
            temp_dist_sorted[r, typeCol]
    }
    if (all(is.na(tempSplitOut$split))) {
      warning("Warning! All temperature splits NA - this implies empty temp_split table. Assuming all 0's.")
      tempSplitOut$split <- 0
    }
    return(tempSplitOut)
    # # For testing
    # water_year_types$year <- as.numeric(water_year_types$year)
    # yrs <- water_year_types %>% group_by(type) %>% slice(1:5)
    # tempSplitOut_plot <- tempSplitOut %>%
    #   mutate(year = lubridate::year(Date)) %>%
    #   left_join(water_year_types, by = "year") %>%
    #   # Get the first 3 from each type
    #   filter(year %in% yrs$year)
    # lubridate::year(tempSplitOut_plot$Date) <- 2000
    # ggplot2::ggplot(tempSplitOut_plot) + 
    #   ggplot2::geom_line(aes(x = Date, y = split, color = type),
    #     linewidth = 2, alpha = 0.4) + 
    #   theme_classic()
}
