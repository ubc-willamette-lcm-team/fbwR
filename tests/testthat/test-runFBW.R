test_that("when runFBW is run without inputs, return an error", {
  expect_error(runFBW(), "One of param_list or template_file must be provided to runFBW")
})