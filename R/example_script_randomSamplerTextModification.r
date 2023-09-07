# Example script for working with text information
# Created by Mairin Deith on Sept 7 2023

### Step 1: Create an example dataframe, e.g., an intermediate FBW result
###   that has one row for each day, let's populate with random abundances

fbw_df <- data.frame(
    # All 1's to show as an example
  fish_in_dam = rep(1, 12))

### Step 2: Example entry from the template; 
###   the string here is what would entered in the Excel cell
### fbwR reads this string, it does not create it.

text_input <- "random[rnorm(mean = 0, sd = 1)]"

### Step 3: Modify the string in fbwR
###   a: First, remove the "random[]" part using `gsub`

# gsub replaces text matching the `pattern` in `x` with the `replace` text, 
#   fixed = TRUE forces the pattern to be read exactly as written (instead of 
#   looking for regular expressions)

text_input <- gsub(x = text_input, pattern = "random[", replace = "", fixed = T)
text_input <- gsub(x = text_input, pattern = "]", replace = "", fixed = T)

### This returns an error; we do not have n =  ... in the call yet
eval(parse(text = text_input)) # Error: argument "n" is missing

###   b: Next, add the number of samples to be drawn from the 
###     random sampler

text_final <- gsub(text_input, pattern = ")",
  # the replacement text can created using `paste0()` and `nrow()` 
  replace = paste0(", n = ", nrow(fbw_df), ")"), 
  fixed = TRUE)

text_final # "rnorm(mean = 0, sd = 1, n = 12)"

### Step 4: Check that it works and add it to the fbw_df 
survival_rates <- eval(parse(text = text_final)) 

library(dplyr)
fbw_df <- fbw_df %>%
  mutate(
    ### add the survival rate as a column
    survival_rate = survival_rates,
    ### multiply by the fish abundance in the dam
    surviving = fish_in_dam * survival_rate)
