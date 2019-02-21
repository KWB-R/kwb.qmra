#' Treatment: get WHO reductions 
#' @return list with $untidy and  $tidy data frames
#' @seealso \url{http://apps.who.int/iris/bitstream/10665/44584/1/9789241548151_eng.pdf#page=162}
#' @importFrom magrittr %>% 
#' @import dplyr
#' @export
who_getTreatment <- function() {

whoFile <- system.file("extdata/monitoring/WHO2011_PathogensInDrinkingWater.xlsx", 
                       package = "kwb.qmra")

who_sources <- readxl::read_excel(path = whoFile,
                          sheet = "Sources")

who_treatment <-  readxl::read_excel(path = whoFile,
                             sheet = "DrinkingWaterTreatment") %>%  
                  dplyr::left_join(who_sources) %>%  
                  dplyr::mutate(LogReduction_Minimum = gsub(pattern = "<|>",
                                                     replacement = "",
                                                     .data$LogReduction_Minimum)  %>%
                                                as.numeric(),
                         LogReduction_Maximum = gsub(pattern = "<|>",
                                                     replacement = "",
                                                     .data$LogReduction_Maximum)  %>%  
                                                     as.numeric(),
                         Reference = sprintf("[%s](%s)",
                                             .data$ReferenceName, 
                                             .data$ReferenceLink)) %>% 
                  dplyr::rowwise() %>%  
                  dplyr::mutate(LogReduction_Mean = (.data$LogReduction_Minimum + .data$LogReduction_Maximum)/2) %>% 
                  dplyr::arrange(.data$TreatmentGroup, 
                                  .data$TreatmentName, 
                                  .data$PathogenGroup ) %>% 
                  dplyr::select(-.data$ReferenceLink,
                                -.data$ReferenceID,
                                -.data$ReferenceName,
                                -dplyr::starts_with("Dose"))

treatments <- who_treatment %>% 
              dplyr::select(.data$TreatmentName) %>%  
              dplyr::distinct() %>% 
              dplyr::ungroup() %>% 
              dplyr::mutate(TreatmentID = dplyr::row_number()) %>% 
              dplyr::select(.data$TreatmentID, .data$TreatmentName)

who_treatment <- merge(who_treatment, treatments) 


who_treatment_tidy <- tidyr::gather_(who_treatment, 
                    key_col = "Key", 
                    value_col = "LogReduction", 
                    gather_cols = c("LogReduction_Minimum",
                                    "LogReduction_Maximum")) %>% 
      dplyr::mutate(Key = gsub("LogReduction_","", .data$Key))

list(untidy = who_treatment, 
       tidy = who_treatment_tidy, 
     schemes = treatments )
}

if (FALSE) {
who_treatment <- who_getTreatment()

#### Plot all treatment schemes
who_treatment$tidy  %>% 
  ggplot(ggplot2::aes(x = .data$PathogenGroup, 
             y = .data$LogReduction)) + 
        geom_point() + 
        geom_line() + 
        facet_wrap(~  TreatmentName) + 
        stat_summary(fun.y = "mean", 
                     colour = "red", 
                     size = 2, 
                     geom = "point") +
        theme_bw()


#### Plot only selected treatment schemes
treatment_selected <- c(9,8)

schemeName <- who_treatment_tidy %>% 
              dplyr::filter(.data$TreatmentID %in% treatment_selected) %>% 
              dplyr::select(.data$TreatmentName) %>% 
              unique() %>%  
              unlist() %>% 
              paste(collapse = " & ")

  who_treatment_tidy %>% 
  dplyr::filter(.data$TreatmentID %in% treatment_selected) %>% 
  ggplot(ggplot2::aes(x = .data$TreatmentName, y = .data$LogReduction)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~  PathogenGroup,
             ncol = 1) + 
  stat_summary(fun.y = "mean", 
               colour = "red", 
               size = 2, 
               geom = "point") +
  theme_bw() +
  ggtitle(sprintf("Treatment scheme: %s", .data$schemeName))

treatment_scheme <- who_treatment %>% 
                    dplyr::filter(.data$TreatmentID %in% treatment_selected)  %>%  
                    dplyr::select(.data$PathogenGroup, 
                                   .data$LogReduction_Minimum, 
                                   .data$LogReduction_Maximum, 
                                   .data$LogReduction_Mean) %>% 
                    dplyr::group_by(.data$PathogenGroup) %>% 
                    dplyr::summarise_each(funs(sum), 
                                          dplyr::matches("Log"))


### Plot treatment scheme performance

treatment_scheme_plot <- tidyr::gather(treatment_scheme , 
                                       key = .data$Key, 
                                       value = .data$LogReduction,
                                       -.data$PathogenGroup) %>% 
                         dplyr::mutate(Key = gsub("LogReduction_","", 
                                                  .data$Key))



treatment_scheme_plot  %>% 
ggplot(aes(x = .data$PathogenGroup, 
           y = .data$LogReduction)) + 
  geom_point() + 
  geom_line() + 
  stat_summary(fun.y = "mean", 
               colour = "red", 
               size = 2, 
               geom = "point") +
  theme_bw() +
  ggtitle(sprintf("Treatment scheme: %s", .data$schemeName))
}