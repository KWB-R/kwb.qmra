## code to prepare `risk_json` dataset goes here

config_json <- kwb.qmra::opencpu_config_read()
risk_json <- kwb.qmra::opencpu_simulate_risk(config_json)

usethis::use_data(risk_json, overwrite = TRUE)
