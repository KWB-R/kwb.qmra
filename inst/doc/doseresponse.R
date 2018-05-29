## ------------------------------------------------------------------------
library(kwb.qmra)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggrepel)

## ------------------------------------------------------------------------
dr.db <- kwb.qmra::dr.db_download()

## ------------------------------------------------------------------------
dr.db2 <- dr.db %>% dplyr::select(PathogenGroup,
                         PathogenName, 
                        `Agent strain`,
                        `Best fit model*`,
                        `Optimized parameter(s)`, 
                        `LD50/ID50`,
                        `Host type`,
                         Route,
                        `Dose units`,
                         Response, 
                         Reference)  %>% 
                dplyr::arrange(PathogenGroup, PathogenName) 

caption <- "Table 1: Best-fit dose-response parameters ([QMRAwiki, 2016](http://qmrawiki.canr.msu.edu/index.php/?title=Table_of_Recommended_Best-Fit_Parameters))"
knitr::kable(dr.db2,caption = caption )
#DT::datatable(doseresponse, caption = caption)

## ------------------------------------------------------------------------
dr.model <- kwb.qmra::dr.db_model(dr.db = dr.db)


ggplot( dr.model, aes(x = dose, 
                      y = infectionProbability, 
                      col = PathogenGroup)) + 
  geom_point() + 
  scale_x_log10() + 
  theme_bw()


## ---- echo=FALSE, fig.width=8, fig.height=5, fig.cap="Figure 3: Dose for all microbial parameters with 50% infection probability"----
tt <- dr.model  %>%  
  filter(infectionProbability > 0.49,
         infectionProbability < 0.51) %>%  
  group_by(PathogenID, PathogenGroup, PathogenName)  %>% 
  summarise(infectionProbability = round(median(infectionProbability),2), 
            dose = median(dose)) %>%  
  ungroup() %>% 
  dplyr::arrange(dose)

ggplot(tt, aes(PathogenGroup, dose, col = PathogenGroup)) + 
  geom_point(position = position_jitter(width = 0, height = 0)) + 
  geom_text_repel(aes(label = PathogenName)) +
  scale_y_log10() + 
  theme_bw() +
  guides(fill = FALSE) +
  ylab("Dose with 50% infection probability")


