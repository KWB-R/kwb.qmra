test_that("dr.db_model() works", {

  dr.db <- dr.db_download()
  
  dr.db_model(dr.db)
  
  dr.db$`Best fit model*` <- rep("beta-Poisson", nrow(dr.db))
  
  dr.db_model(dr.db)
})
