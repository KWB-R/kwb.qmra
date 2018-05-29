test_that("simulate_treatment() works", {

  expect_error(simulate_treatment())
  
  confDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")
  
  config <- config_read(confDir)
  
  simulate_treatment(config, wide = FALSE)
})
