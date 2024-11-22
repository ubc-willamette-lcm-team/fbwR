test_that("error if no file path provided", {
  expect_error(loadFromTemplate())
})

### Test data are to be stored in tests/testthat/testdata
test_that("read in test template file", {
  ### Test 1: errors if a sheet is missing
  expect_error(loadFromTemplate(
    template_file = test_path("testdata", "template_test1.xlsx")))
  ### Check the name of the param_list() created
  expect_equal(names(loadFromTemplate(
    template_file = test_path("testdata", "template_test2.xlsx"))),
    c("alt_desc", "route_specs", "route_eff", "route_dpe", "monthly_runtiming", 
      "ro_surv_table", "ro_elevs", "turb_surv_table", "spill_surv_table", "fps_surv_table", 
      "temp_dist", "water_year_types"))
})