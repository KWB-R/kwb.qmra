test_that("plot_total_dalys() works", {

  expect_error(plot_total_dalys())
  
  confDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")
  
  risk <- simulate_risk(config_read(confDir))
  
  plot_total_dalys(risk)
  
  plot_total_dalys(risk, labelling = TRUE)
})
