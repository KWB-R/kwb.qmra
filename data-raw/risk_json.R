## code to prepare `risk_json` dataset goes here

config_dummy_json <- kwb.qmra::opencpu_config_read(
  confDir = system.file("extdata/configs/dummy", package = "kwb.qmra")
)
config_json <- jsonlite::fromJSON(config_dummy_json)

risk_dummy <- kwb.qmra::opencpu_simulate_risk(config_json)
risk_dummy_json <- jsonlite::toJSON(risk_dummy, pretty = TRUE)
usethis::use_data(risk_dummy_json, overwrite = TRUE)


### Unused (resulting "risk_default.json" too big for Github -> ~276MB size!!!)

# config_default_json <- kwb.qmra::opencpu_config_read(
#   confDir = system.file("extdata/configs/default", package = "kwb.qmra")
# )
# config_default <- jsonlite::fromJSON(config_default_json)
# 
# risk_default_json <- kwb.qmra::opencpu_simulate_risk(config_default)
# 
# usethis::use_data(risk_default_json, overwrite = TRUE)