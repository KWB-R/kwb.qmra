test_that("xlogscale() works", {

  expect_error(kwb.qmra:::xlogscale())
  
  kwb.qmra:::xlogscale(10:100)
  
  kwb.qmra:::xlogscale(10:100, userLimit = c(5, 1000))
})
