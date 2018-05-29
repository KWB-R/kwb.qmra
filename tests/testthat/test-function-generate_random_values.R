test_that("generate_random_values() works", {

  expect_error(generate_random_values())
  
  confDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")
  
  config <- config_read(confDir)

  config$type <- "norm"  
  config$min <- 1
  config$max <- 1000
  config$mean <- NA
  config$sd <- NA
  
  expect_error(generate_random_values(config))

  config$type <- "lnorm"  
  config$meanlog <- NA
  config$sdlog <- NA
  
  expect_error(generate_random_values(config))

  config$type <- "triangle"  
  config$mode <- NA
  config$min <- config$max <- 0
  
  expect_error(generate_random_values(config))
})
