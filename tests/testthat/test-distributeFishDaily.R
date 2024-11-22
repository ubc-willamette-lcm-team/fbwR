# Create a simple ResSim dataframe and param_list for testing 

ressim <- data.frame(
  Date = c()
  # ....
)

param_list <- loadFromTemplate(template_file = file.path(
  "inst", "extdata", "NONFINAL_template_data_entry_06012023.xlsx"))
# Errors here; need to test this one
# Tests
test_that("errors if inputs not provided", {
  expect_error(distributeFishDaily())
  #!# Need to create these inputs...
  expect_error(distributeFishDaily(ressim = data.frame()))
  expect_error(distributeFishDaily(param_list = list()))
})

# Test characteristics of inputs provided to the distribution function
test_that("ressim_tmp created properly", {
  expect_equal(nrow(data.frame()) == nrow(distributeFishDaily(
    param_list = list(), ressim = data.frame())))
})

# Test that output dataframe includes the 