# [kwb.qmra 0.3.0](https://github.com/KWB-R/kwb.qmra)

Adaptions for project [Smart-Control](https://www.kompetenz-wasser.de/en/project/smart-control/):

* Added stats summary outputs to `risk` output list (i.e. `stats_total` and 
`stats_logreductions`)

* Added wrapper functions for OpenCPU (kwb.qmra::opencpu_config_read, 
kwb.qmra::opencpu_simulate_risk) and example JSON datasets based on dummy config
(`config_json` and `risk_json)

* Fixes: fix deprecation warning for `dplyr::tbl_df()` by replacing with 
`dplyr::as_tibble()`

* Continuous integration: moved to `github-actions` (instead of `travis-ci` and 
`appveyor`)

# [kwb.qmra 0.2.0](https://github.com/KWB-R/kwb.qmra/releases/tag/v0.2.0)

Improvements of the R package (originally developed within the EU project [DEMOWARE](http://www.demoware.eu/en))
made within the EU project [AquaNES](http://www.aquanes-h2020.eu): 

* Test: add vignette [test_random_distributions_inflow.Rmd](https://github.com/KWB-R/kwb.qmra/blob/master/vignettes/test_random_distributions_inflow.Rmd) for testing random distrution functions with inflow data (using data stored in 
[qmra.db](https://github.com/kwb-r/qmra.db) R package) ([#21](https://github.com/KWB-R/kwb.qmra/issues/21), 
([#24](https://github.com/KWB-R/kwb.qmra/issues/24))) 

* Delete "docs" folder (documentation auto-build on gh-pages branch) ([#19](https://github.com/KWB-R/kwb.qmra/issues/19))

* fix bug due to dplyr update (0.7.8 -> 0.8.0.1) ([#18](https://github.com/KWB-R/kwb.qmra/issues/18)) 

* create_random_distribution: "norm/log10_norm" ("percentile under min/max as variable) 
([#17](https://github.com/KWB-R/kwb.qmra/issues/17))

* create_random_distribution: "lognorm" (fix to avoid NaN) ([#16](https://github.com/KWB-R/kwb.qmra/issues/16)))

* `create_random_distribution()` add **log10_norm** ([#14](https://github.com/KWB-R/kwb.qmra/issues/14)) distribution together with new vignette `usage_log10_norm.Rmd` and rename **loguniform** to **log10_uniform**

* `create_random_distribution()` add  **loguniform** distribution together with new vignette `usage_loguniform.Rmd` ([#23](https://github.com/KWB-R/kwb.qmra/issues/23))

* bugfix: `distribution_repeater()`, works now if number_of_events == 1 ([#22](https://github.com/KWB-R/kwb.qmra/issues/22))

For more details of the changes see also [Milestone: v0.2.0](https://github.com/KWB-R/kwb.qmra/milestone/5) 

# [kwb.qmra 0.1.1](https://github.com/KWB-R/kwb.qmra/releases/tag/v.0.1.1)

* version developed within DEMOWARE project and also released on [Zenodo](https://zenodo.org/record/154111) 
* used for the quantitative microbiological risk assessment of different wastewater reuse options in Old Ford (see: [Zenodo](https://doi.org/10.5281/zenodo.159527))