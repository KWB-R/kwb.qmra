test_that("config_write() works", {

  expect_error(config_write())
  
  confDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")
  
  config <- config_read(confDir)

  config_write(config)  
  
  config_write(config, zipFiles = FALSE)
})
