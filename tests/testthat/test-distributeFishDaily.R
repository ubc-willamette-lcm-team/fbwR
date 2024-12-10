# Create a simple ResSim dataframe and param_list for testing 



test_that("errors if inputs not provided", {
  expect_error(distributeFishDaily())
  #!# Need to create these inputs...
  expect_error(distributeFishDaily(ressim = data.frame()))
  expect_error(distributeFishDaily(param_list = list()))
})

# param_list <- loadFromTemplate(
#   test_path("testdata", "NONFINAL_template_data_entry_06012023.xlsx"))
# Test characteristics of inputs provided to the distribution function
test_that("ressim_tmp created properly", {
  ressim <- tibble::tibble(
    Date = date(),
    outflow_flow = numeric()
  )
  expect_equal(nrow(data.frame()) == nrow(distributeFishDaily(
    param_list = list(), ressim = ressim)))
})

# Test that output dataframe includes the 
