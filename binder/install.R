install.packages("devtools")
devtools::install_github("kwb-r/kwb.qmra")
writeLines("R_LIBS_USER=/srv/rlibs", "/home/jovyan/.Renviron")
setwd("/home/jovyan/binder")