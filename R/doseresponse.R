# library(rvest)
# library(dplyr)
# library(stringr)
# library(sfsmisc)
# library(ggplot2)
# library(ggrepel)

#' Download dose-response model database from QMRAwiki 
#' @param fromInternet download from internet (default: FALSE), if FALSE import from local copy 
#' @return tbl_df for different microbial parameters
#' @source http://qmrawiki.canr.msu.edu/index.php?title=Table_of_Recommended_Best-Fit_Parameters
#' @export
dr.db_download <- function(fromInternet = FALSE) {

if (fromInternet == FALSE) {
dbfile <- system.file("extdata/doseresponse/dr_db.csv", package = "kwb.qmra")
dr.db <- readr::read_csv(file = dbfile)
} else {
  
pathogens <- c("Bacteria", "Viruses", "Protozoa")


for (i in 1:length(pathogens)) {

url <- "http://qmrawiki.canr.msu.edu/index.php"
  
tmp <- xml2::read_html(sprintf("%s?title=Table_of_Recommended_Best-Fit_Parameters#tab=%s", url, pathogens[i])) 
tmp <-  rvest::html_node(x = tmp, xpath = sprintf("/html/body/div[3]/div/div[4]/div/div/div[%d]/table",i))
tmp <- rvest::html_table(x = tmp) 
  
tmp$Link <- sprintf("%s/%s",url,gsub(tmp$Agent, pattern = " ", replacement = "_"))
tmp$PathogenGroup <- pathogens[i]


if (i == 1) {
  dr.db <- tmp 
} else {
  dr.db <- rbind(dr.db, tmp)
}
}

dr.db <- dplyr::filter_(.data = dr.db, ~Agent != "TestPage")
dr.db$PathogenName <- stringr::str_replace(string = dr.db$Agent,pattern = ":.*","")
dr.db$Agent <- sprintf("[%s](%s)", dr.db$Agent, dr.db$Link)

dr.db$k <- as.numeric(ifelse(dr.db$`Best fit model*` == "exponential", 
                  stringr::str_replace(dr.db$`Optimized parameter(s)`, 
                                       pattern = "k\\s*=", ""), 
                  NA))

dr.db$alpha  <- as.numeric(ifelse(dr.db$`Best fit model*`== "beta-Poisson", 
                        stringr::str_extract_all(dr.db$`Optimized parameter(s)`, 
                                                 pattern = "\\d+\\.\\d+E[+-]\\d{2}",
                                                 simplify = TRUE)[,1],
                                       NA))

dr.db$N50 <- as.numeric(ifelse(dr.db$`Best fit model*`== "beta-Poisson", 
                               stringr::str_extract_all(dr.db$`Optimized parameter(s)`, 
                                                     pattern = "\\d+\\.\\d+E[+-]\\d{2}",
                                                     simplify = TRUE)[,2], 
                                     NA))


dr.db$PathogenID <- 1:nrow(dr.db)
}
#write.csv(dr.db,
#"C:/Users/mrustl/Documents/WC_Server/R_Development/trunk/RPackages/kwb.qmra/inst/extdata/doseresponse/dr_db.csv")  
return(dr.db)
}

#' Dose-response model: exponential
#' @param dose vector of dose data (default: sfsmisc::lseq(from=0.1, to=10^10, length=1000))
#' @param k k-value (default: 5.72E-01)
#' @return tbl_df 
#' @export
#' @importFrom sfsmisc lseq
dr.expo <- function(dose = sfsmisc::lseq(from = 1, 
                                         to = 10^10, 
                                         length=1000), 
                    k = 5.72E-01) {

response <- 1 - exp(-k * dose)
res <- dplyr::tbl_df(data.frame(model = "exponential", 
                  dose = dose, 
                  infectionProbability = response,
                  k = k))

return(res)
}

#' Dose-response model: beta-poisson
#' @param dose vector of dose data (default: sfsmisc::lseq(from=0.1, to=10^10, length=1000))
#' @param alpha  alpha (default: 3.28E-01)
#' @param N50  N50 (default: 5.43E+03)
#' @return tbl_df 
#' @export
#' @importFrom sfsmisc lseq
dr.betapoisson <- function(dose = sfsmisc::lseq(from = 1, 
                                                to = 10^10, 
                                                length=1000), 
                    alpha =  3.28E-01,
                    N50 = 5.43E+03) {
  
  response <- 1 - (1 + dose * (2 ^ (1/alpha) - 1)/N50) ^ -alpha
  res <- dplyr::tbl_df(data.frame(model = "betapoisson",
                    dose = dose, 
                    infectionProbability = response, 
                    alpha = alpha, 
                    N50 = N50))
  
  return(res)
}


#' Generate table with different doses for dr.db_download() 
#' @param dr.db as retrieved by dr.db_download(), default: dr.db_download()
#' @param dose vector of dose data (default: sfsmisc::lseq(from=0.1, to=10^10, length=1000))
#' @return tbl_df 
#' @export
#' @importFrom plyr rbind.fill
dr.db_model <- function(dr.db = dr.db_download(),
                        dose = sfsmisc::lseq(from = 1, 
                                             to = 10 ^ 10, 
                                             length = 1000)) {

indices.expo <- which(dr.db$`Best fit model*` == "exponential")


if (length(indices.expo) > 0) {
for (ind in indices.expo) {
  sel <- dr.db[ind, ]
  tmp <- data.frame(PathogenID = sel$PathogenID, 
                    PathogenGroup = sel$PathogenGroup, 
                    PathogenName = sel$PathogenName, 
                    dr.expo(dose = dose,
                            k = sel$k))
  if (ind == indices.expo[1]) {
    dr.model <- tmp } else {
      dr.model <- rbind(dr.model, tmp)  
    }}}

indices.betapoisson <- which(dr.db$`Best fit model*` != "exponential")

if (length(indices.betapoisson) > 0) {
for (ind in indices.betapoisson) {
  sel <- dr.db[ind, ]
  tmp <- data.frame(PathogenID = sel$PathogenID, 
                    PathogenGroup = sel$PathogenGroup, 
                    PathogenName = sel$PathogenName, 
                    dr.betapoisson(dose = dose,  
                                   alpha = sel$alpha, 
                                   N50 = sel$N50))
  if (length(indices.expo) == 0 & ind == indices.betapoisson[1]) dr.model <- tmp
  dr.model <- plyr::rbind.fill(dr.model, tmp)  
}}

dr.model  <- dplyr::tbl_df(dr.model)

return(dr.model)
}


if (FALSE)
{
dr.db <- dr.db_download()
dr.model <- dr.db_model(dr.db = dr.db)


ggplot( dr.model, aes_(x = ~dose, 
               y = ~infectionProbability, col = ~PathogenGroup)) + 
  geom_point() + 
  scale_x_log10() + 
  theme_bw()

tt <- dr.model  %>%  
  dplyr::filter_(~infectionProbability > 0.49,
         ~infectionProbability < 0.51) %>%  
  dplyr::group_by_(~PathogenID, ~PathogenGroup, ~PathogenName)  %>% 
  dplyr::summarise_(infectionProbability = round(median(~infectionProbability),2), 
            dose = median(~dose)) %>%  
  dplyr::ungroup() %>% 
  dplyr::arrange_(~dose)

ggplot2::ggplot(tt, ggplot2::aes_(~PathogenGroup, ~dose, col = ~Group)) + 
  geom_point(position = position_jitter(w = 0, h = 0)) + 
  geom_text_repel(ggplot2::aes_(label = ~PathogenName)) +
scale_y_log10() + 
  theme_bw() +
  guides(fill=FALSE) +
  ylab("Dose with 50% infection probability")

}