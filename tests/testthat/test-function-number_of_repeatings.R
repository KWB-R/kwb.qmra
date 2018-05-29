test_that("number_of_repeatings() works", {

  expect_error(kwb.qmra:::number_of_repeatings())

  confDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")
  
  config <- config_read(confDir)
  
  expect_equal(kwb.qmra:::number_of_repeatings(config), 10)

  config$exposure$value[config$exposure$name == "number_of_repeatings"] <- NA
  
  expect_error(kwb.qmra:::number_of_repeatings(config))
})
