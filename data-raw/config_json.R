## code to prepare `config_json` dataset goes here
config_json <- kwb.qmra::opencpu_config_read()

usethis::use_data(config_json, overwrite = TRUE)
