#' Download dose-response model database from QMRAwiki 
#' 
#' @param fromInternet download from internet (default: FALSE), if FALSE import
#'   from local copy
#' @return tbl_df for different microbial parameters
#' @source
#'   http://qmrawiki.canr.msu.edu/index.php?title=Table_of_Recommended_Best-Fit_Parameters
#'
#' @export
#' 
dr.db_download <- function(fromInternet = FALSE)
{
  # Load the database from the file if requested
  if (! fromInternet) {
    
    file <- system.file("extdata/doseresponse/dr_db.csv", package = "kwb.qmra")
    
    return(readr::read_csv(file))
  } 
  
  # Load the database from the internet
  pathogens <- c("Bacteria", "Viruses", "Protozoa")
  
  url <- "http://qmrawiki.canr.msu.edu/index.php"
  url_fmt <- "%s?title=Table_of_Recommended_Best-Fit_Parameters#tab=%s"
  xpath_fmt <- "/html/body/div[3]/div/div[4]/div/div/div[%d]/div/table"
  
  dr.db <- do.call(rbind, lapply(seq_along(pathogens), function(i) {
    result <- xml2::read_html(sprintf(url_fmt, url, pathogens[i])) %>%
      rvest::html_node(xpath = sprintf(xpath_fmt, i)) %>%
      rvest::html_table()
    
    result$Link <- sprintf("%s/%s", url, gsub(" ", "_", result$Agent))
    result$PathogenGroup <- pathogens[i]
    result
  }))
  
  dr.db <- dr.db %>% 
    dplyr::filter(.data$Agent != "TestPage")
  
  dr.db$PathogenName <- stringr::str_replace(
    string = dr.db$Agent,
    pattern = ":.*",
    replacement = ""
  )
  
  dr.db$Agent <- sprintf("[%s](%s)", dr.db$Agent, dr.db$Link)
  
  best_fit_model <- dr.db$`Best fit model*`
  parameters <- dr.db$`Optimized parameter(s)`
  
  dr.db$k <- as.numeric(ifelse(
    test = (best_fit_model == "exponential"), 
    yes = stringr::str_replace(parameters, "k\\s*=", ""), 
    no = NA
  ))
  
  dr.db$alpha  <- as.numeric(ifelse(
    test = (best_fit_model == "beta-Poisson"), 
    yes = stringr::str_extract_all(
      parameters, 
      pattern = "\\d+\\.\\d+E[+-]\\d{2}",
      simplify = TRUE
    )[, 1],
    no = NA
  ))
  
  dr.db$N50 <- as.numeric(ifelse(
    test = (best_fit_model == "beta-Poisson"), 
    yes = stringr::str_extract_all(
      parameters, 
      pattern = "\\d+\\.\\d+E[+-]\\d{2}",
      simplify = TRUE
    )[, 2], 
    no = NA
  ))
  
  dr.db$PathogenID <- seq_len(nrow(dr.db))
  
  #write.csv(dr.db,
  #"C:/Users/mrustl/Documents/WC_Server/R_Development/trunk/RPackages/kwb.qmra/inst/extdata/doseresponse/dr_db.csv")  
  dr.db
}

#' Dose-response model: exponential
#' 
#' @param dose vector of dose data (default: 
#'   \code{sfsmisc::lseq(from = 0.1, to = 10^10, length = 1000)})
#' @param k k-value (default: 5.72E-01)
#' @return tbl_df
#' @export
#' @importFrom sfsmisc lseq
#' 
dr.expo <- function(
  dose = sfsmisc::lseq(from = 1, to = 10^10, length = 1000), 
  k = 5.72E-01
)
{
  dplyr::tbl_df(data.frame(
    model = "exponential", 
    dose = dose, 
    infectionProbability = 1 - exp(-k * dose),
    k = k
  ))
}

#' Dose-response model: beta-poisson
#'
#' @param dose vector of dose data (default: 
#'   \code{sfsmisc::lseq(from = 0.1, to = 10^10, length = 1000)})
#' @param alpha  alpha (default: 3.28E-01)
#' @param N50  N50 (default: 5.43E+03)
#' @return tbl_df
#' @export
#' @importFrom sfsmisc lseq
#' 
dr.betapoisson <- function(
  dose = sfsmisc::lseq(from = 1, to = 10^10, length = 1000), 
  alpha = 3.28E-01,
  N50 = 5.43E+03
)
{
  dplyr::tbl_df(data.frame(
    model = "betapoisson",
    dose = dose, 
    infectionProbability = 1 - (1 + dose * (2 ^ (1/alpha) - 1)/N50) ^ -alpha, 
    alpha = alpha, 
    N50 = N50
  ))
}

#' Generate table with different doses for dr.db_download() 
#' 
#' @param dr.db as retrieved by dr.db_download(), default: dr.db_download()
#' @param dose vector of dose data (default: 
#'   \code{sfsmisc::lseq(from=0.1, to = 10^10,length = 1000)})
#' @return tbl_df
#' @export
#' @importFrom plyr rbind.fill
#' 
dr.db_model <- function(
  dr.db = dr.db_download(),
  dose = sfsmisc::lseq(from = 1, to = 10^10, length = 1000)
)
{
  is_expo <- (dr.db$`Best fit model*` == "exponential")
  
  dr_model_expo <- if (any(is_expo)) {
    get_dr_model(dr.db[is_expo, ], dose, exponential = TRUE)
  } # else NULL

  dr_model_poisson <- if (any(! is_expo)) {
    get_dr_model(dr.db[! is_expo, ], dose, exponential = FALSE)
  } # else NULL
 
  plyr::rbind.fill(dr_model_expo, dr_model_poisson) %>% 
    dplyr::tbl_df()
}

# get_dr_model -----------------------------------------------------------------
get_dr_model <- function(dose_response_db, dose, exponential)
{
  key_columns <- c("PathogenID", "PathogenGroup", "PathogenName")
  
  result_rows <- lapply(seq_len(nrow(dose_response_db)), function(i) {
    
    pars <- dose_response_db[i, ]
    
    model <- if (exponential) {
      dr.expo(dose = dose, k = pars$k)
    } else {
      dr.betapoisson(dose = dose, alpha = pars$alpha, N50 = pars$N50)
    }
    
    cbind(pars[, key_columns], model)
  })
  
  do.call(plyr::rbind.fill, result_rows)
}

if (FALSE)
{
  dr.db <- dr.db_download()
  dr.model <- dr.db_model(dr.db = dr.db)
  
  ggplot(dr.model, ggplot2::aes_string(
    x = "dose", 
    y = "infectionProbability", col = "PathogenGroup"
  )) + 
    geom_point() + 
    scale_x_log10() + 
    theme_bw()
  
  tt <- dr.model  %>%  
    dplyr::filter(
      .data$infectionProbability > 0.49,
      .data$infectionProbability < 0.51
    ) %>%  
    dplyr::group_by(
      .data$PathogenID, 
      .data$PathogenGroup, 
      .data$PathogenName
    )  %>% 
    dplyr::summarise(
      infectionProbability = round(median(.data$infectionProbability), 2), 
      dose = median(.data$dose)
    ) %>%
    dplyr::arrange(.data$dose)
  
  ggplot2::ggplot(tt, ggplot2::aes_string(
    "PathogenGroup", "dose", col = "PathogenGroup"
  )) + 
    geom_point(position = position_jitter(w = 0, h = 0)) + 
    ggrepel::geom_text_repel(ggplot2::aes_string(label = "PathogenName")) +
    scale_y_log10() + 
    theme_bw() +
    guides(fill = FALSE) +
    ylab("Dose with 50% infection probability")
}
