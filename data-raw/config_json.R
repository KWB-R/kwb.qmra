## code to prepare `config_json` dataset goes here
config_dummy_json <- kwb.qmra::opencpu_config_read(
  confDir = system.file("extdata/configs/dummy", package = "kwb.qmra")
  )

usethis::use_data(config_dummy_json, overwrite = TRUE)

config_default_json <- kwb.qmra::opencpu_config_read(
  confDir = system.file("extdata/configs/default", package = "kwb.qmra")
)

usethis::use_data(config_default_json, overwrite = TRUE)
