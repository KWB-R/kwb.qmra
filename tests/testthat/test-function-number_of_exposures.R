test_that("number_of_exposures() works", {

  expect_error(kwb.qmra:::number_of_exposures())
  
  confDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")
  
  config <- config_read(confDir)
  
  expect_equal(kwb.qmra:::number_of_exposures(config), 365)
  
  config$exposure$value[config$exposure$name == "number_of_exposures"] <- NA
  
  expect_error(kwb.qmra:::number_of_exposures(config))
})
