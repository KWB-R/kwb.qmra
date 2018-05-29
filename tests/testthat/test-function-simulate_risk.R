test_that("simulate_risk() works", {

  expect_error(kwb.qmra:::simulate_risk())
  
  confDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")
  
  config <- config_read(confDir)

  simulate_risk(config, usePoisson = FALSE)
  
  config$doseresponse[, c("k", "alpha", "N50")] <- NA
  
  expect_error(simulate_risk(config))
})

