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
#' @import lubridate

calcTempSplit <- function(ressim_data, water_year_types, temp_dist_df){
    # Create an output data frame
    tempSplitOut <- data.frame(
        Date = as.Date(x = ressim_data$Date, format = "%Y-%m-%d"),
        split = NaN
    )
    # Iterate over years
    for (i in 1:length(water_year_types$year)) {
        yr <- as.numeric(water_year_types$year[i])
        # Identify year type and corresponding column in temp_dist_df
        type <- water_year_types$type[i]
        typeCol <- which(colnames(temp_dist_df) == type)
        # Find the lower bound for the temperature split data
        lower_date <- temp_dist_df$Date[1]
        # Coerce the year to be current (cannot be year-agnostic)
        lubridate::year(lower_date) <- yr
        origin_date <- lower_date # Make a copy for later
        # Iterate over these rows
        for (r in 2:nrow(temp_dist_df)) {
            upper_date <- temp_dist_df$Date[r]-1
            lubridate::year(upper_date) <- yr
            date_interval <- lubridate::interval(lower_date, upper_date)
            tempSplitOut$split[which(tempSplitOut$Date %within% date_interval)] <- temp_dist_df[r-1,typeCol]
            lower_date <- upper_date + 1
        }
        # Now, finish up with the final interval (lower_date to the original date of the next year)
        lubridate::year(origin_date) <- lubridate::year(origin_date)+1
        # New interval
        date_interval <- lubridate::interval(lower_date, origin_date)
        # Here, 'r' is saved from the iterator above and can be used at its highest value
        tempSplitOut$split[
            which(tempSplitOut$Date %within% date_interval)] <- 
            temp_dist_df[r, typeCol]
    }
    tempSplitOut
}
