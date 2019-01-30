#' Helper function: create default ggplot theme
#' @return default ggplot theme for kwb.qmra
#' @import ggplot2
#' @keywords internal
myggtheme <- function() {
  
  ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "top",
                   plot.title = element_text(size = 16,
                                             face = "bold"),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14,
                                             face = "bold"))
}

#' Helper function: prepare ylogscale appearence
#' @param paraVals values to be used
#' @param userLimit vector specificing user limits to be used in case that paraVals 
#' are to not included 
#' @param by by parameter for seq() (default: 1)
#' @return default ggplot theme for kwb.qmra
#' @import ggplot2
#' @keywords internal
ylogscale <-  function(paraVals, 
                       userLimit = NULL, 
                       by = 1) {
  
  paraVals <- paraVals[!is.na(paraVals) & paraVals > 0]
  
  
  if (!is.null(userLimit)) paraVals <- c(paraVals, userLimit)
  
  values <- 10 ^ seq(floor(log10(min(paraVals))),
                     ceiling(log10(max(paraVals))),by)
  
  ggplot2::scale_y_continuous(breaks = values,
                     minor_breaks = values, 
                     limits = range(values),
                     labels = values)
}


#' Helper function: prepare xlogscale appearence
#' @param paraVals values to be used
#' @param userLimit vector specificing user limits to be used in case that paraVals 
#' are to not included 
#' @param by by parameter for seq() (default: 1)
#' @return default ggplot theme for kwb.qmra
#' @import ggplot2
#' @keywords internal
xlogscale <-  function(paraVals, 
                       userLimit = NULL, 
                       by = 1) {
  
  paraVals <- paraVals[!is.na(paraVals) & paraVals > 0]
  
  
  if (!is.null(userLimit)) paraVals <- c(paraVals, userLimit)
  
  values <- 10 ^ seq(floor(log10(min(paraVals))),
                     ceiling(log10(max(paraVals))),by)
  
  ggplot2::scale_x_continuous(breaks = values,
                              minor_breaks = values, 
                              limits = range(values),
                              labels = values)
}



### Plot: inflow
#' @title plot inflow
#' @description plotting
#' @param risk list as retrieved by simulate_risk
#' @return ggplot for inflow
#' @import ggplot2
#' @export
plot_inflow <- function(risk) {

  ggplot2::ggplot(risk$input$inflow$events, 
                  ggplot2::aes_string(x = "PathogenName",
                               y = "inflow",
                               col = "PathogenGroup")) +
    ggplot2::geom_jitter() +
    ggplot2::coord_trans(y="log10") +
    ylogscale(risk$input$inflow$events$inflow) +
    myggtheme() +
    labs(x = "Pathogen",
         y = "Inflow concentration (pathogens / litre)")
}

### Plot: reduction in treatment plant
#' @title plot reduction
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @return ggplot for reduction
#' @import ggplot2
#' @export
plot_reduction <- function(risk) {

  ggplot(risk$input$treatment$events_long, aes_string(x = "TreatmentName",
                                               y = "logreduction",
                                               col = "PathogenGroup")) +
    geom_jitter() +
    facet_wrap(~ TreatmentSchemeName,ncol = 1) +
    myggtheme() +
    labs(x = "Treatment process",
         y = "Log reduction in treatment plant")
}





### Plot: effluent
#' @title plot effluent
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @return ggplot for effluent
#' @import ggplot2
#' @export
plot_effluent <- function(risk) {

  ggplot2::ggplot(risk$output$events, 
                  ggplot2::aes_string(x = "PathogenName",
                                 y = "effluent",
                                 col = "PathogenGroup")) +
    ggplot2::geom_jitter() +
    ggplot2::coord_trans(y = "log10") +
    ylogscale(risk$output$events$effluent) +
    ggplot2::facet_wrap(~ TreatmentSchemeName, ncol = 1) +
    ggplot2::geom_hline(yintercept = 1,
               col = "grey",
               lty = 2,
               lwd = 1.5) +
    myggtheme() +
    ggplot2::labs(x = "Pathogen",
         y = "Effluent concentration (pathogens / litre)")
}


### Plot: volume per event
#' @title plot volume per event
#' @description plotting volume
#' @param risk list as retrieved by simulate_risk()
#' @return ggplot for volume per event
#' @import ggplot2
#' @export
plot_event_volume <- function(risk) {
  
ggplot(risk$input$exposure$volumes$events, 
       ggplot2::aes_string(x = "volume_perEvent")) +
  geom_density(alpha = 0.1, col = "blue",fill = "blue") +
  geom_point(y = 0, col = "black") +
  myggtheme() + 
  scale_fill_discrete(guide = 'none') +
  scale_alpha_continuous(guide = 'none') +
  labs(x = "Exposed volume (litre / event)", 
       y = "Density function", 
       title = "Distribution of ingested water volumes")
}


### Plot: dose response for simulated pathogens
#' @title plot reduction
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @return ggplot for reduction
#' @import ggplot2
#' @export
plot_doseresponse <- function(risk) {
  
  dr <- dr.db_model(dr.db = risk$input$doseresponse$paras) 
  
  ggplot(dr, ggplot2::aes_string(x = "dose", 
             y = "infectionProbability", 
             col = "PathogenName")) +
  geom_point() +
  coord_trans(x = "log10") +
  xlogscale(paraVals = dr$dose) +
  myggtheme() +
  labs(x = "Dose",
       y = "Infection probability")
}


### Plot: exposure per event
#' @title plot exposure per event
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @return ggplot for exposure per event
#' @import ggplot2
#' @export
plot_event_exposure <- function(risk) {

ggplot(risk$output$events, aes_string(x = "PathogenName",
                               y = "exposure_perEvent",
                               col = "PathogenGroup")) +
  geom_jitter() +
  coord_trans(y = "log10") +
  ylogscale(risk$output$events$exposure_perEvent) +
  facet_wrap(~ TreatmentSchemeName,ncol = 1) +
  geom_hline(yintercept = 1,
             col = "grey",
             lty = 2,
             lwd = 1.5) +
  myggtheme() +
  labs(x = "Pathogen",
       y = "Exposure per event (ingested pathogens)")
}


### Plot: dose per event
#' @title plot dose per event
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @return ggplot for dose per event
#' @import ggplot2
#' @export
plot_event_dose <- function(risk) {
ggplot(risk$output$events, ggplot2::aes_string(x = "PathogenName",
                               y = "dose_perEvent",
                               col = "PathogenGroup")) +
  geom_jitter() +
  coord_trans(y= "log10") +
  ylogscale(risk$output$events$dose_perEvent) +
  facet_wrap(~  TreatmentSchemeName,ncol = 1) +
  geom_hline(yintercept = 1,
             col = "grey",
             lty = 2,
             lwd = 1.5) +
  myggtheme() +
  labs(x = "Pathogen",
       y = "Dose per event (ingested pathogens)")
}


### Plot: infection probability
#' @title plot infection probability
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @return ggplot for infection probability
#' @import ggplot2
#' @export
plot_event_infectionProb <- function(risk) {
  
  ggplot(risk$output$events, aes_string(x = "PathogenName", 
                                 y = "infectionProb_per_event", 
                                 col = "PathogenGroup", 
                                 fill = TRUE)) + 
    geom_jitter() + 
    coord_trans(y = "log10") +
    scale_y_continuous(breaks = c(0.001, 0.01,0.05, 0.1, 0.25,0.5,1), 
                       limits = c(0.001,1)) +
    scale_fill_discrete(guide = 'none') + 
    facet_wrap(~ TreatmentSchemeName,ncol = 1) + 
    myggtheme() +
    labs(x = "Pathogen", 
         y = "Infection probability")
}

### Plot: illness probability
#' @title plot illness probability
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @return ggplot for illness probability
#' @import ggplot2
#' @export
plot_event_illnessProb <- function(risk) {
ggplot(risk$output$events, ggplot2::aes_string(x = "PathogenName", 
                               y = "illnessProb_per_event", 
                               col = "PathogenGroup", 
                               fill = TRUE)) + 
  geom_jitter() + 
  coord_trans(y = "log10") +
  scale_y_continuous(breaks = c(0.001, 0.01,0.05, 0.1, 0.25,0.5,1), 
                     limits = c(0.001,1)) +
  scale_fill_discrete(guide = 'none') +
  facet_wrap(~  TreatmentSchemeName,ncol = 1) + 
  myggtheme() + 
  labs(x = "Pathogen", 
       y = "Illness probability")

}

### Plot: DALYs per event
#' @title plot dalys_per_event
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @return ggplot for dalys_per_event
#' @import ggplot2
#' @export
plot_event_dalys <- function(risk) {
ggplot(risk$output$events, aes_string(x = "PathogenName", 
                               y = "dalys_per_event", 
                               col = "PathogenGroup")) + 
  geom_jitter() + 
  geom_hline(yintercept = 1E-6, 
             col = "grey", 
             lty = 2, 
             lwd = 1.5) +
  coord_trans(y = "log10") +
  ylogscale(risk$output$events$dalys_per_event) +
  facet_wrap(~ TreatmentSchemeName, ncol = 1) + 
  myggtheme() +
  labs(x = "Pathogen", 
       y = "DALYs per event")

}


### Plot: total infection probability 
#' @title plot total infection probability 
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @param tolerance accecptable tolerance level of risk (default: 1E-4)
#' @return ggplot for total infection probability 
#' @import ggplot2
#' @export
plot_total_infectionProb <- function(risk, tolerance = 1E-4) {
    risk$output$total %>%  
    dplyr::mutate(label = ifelse(.data$infectionProb_sum > 0,
                                 sprintf("%d per %d",
                                         floor(.data$infectionProb_sum/tolerance),
                                         floor(1/tolerance)),
                                 "Zero")) %>% 
  ggplot(ggplot2::aes_string(x = "TreatmentSchemeName", 
                                y = "infectionProb_sum", 
                                col = "PathogenName",
                                size = 2)) + 
    geom_jitter(width = 0.05,
                alpha = 0.4) + 
    scale_size(guide = 'none') +
    geom_text(ggplot2::aes(label = .data$label),
              nudge_x = 0.2) + 
    geom_hline(yintercept = tolerance, 
               col = "grey", 
               alpha = 0.5,
               lty = 2, 
               lwd = 1.5) +
    geom_text(x = 0.7, y = tolerance*1.3, 
              label = sprintf("1 infection per %d people",
                              floor(1/tolerance)),
              inherit.aes = FALSE,
              col = "grey") +
    coord_trans(y = "log10") +
    ylogscale(risk$output$total$infectionProb_sum,
              userLimit = tolerance) +
    myggtheme() +
    labs(x = "Treatment scheme", 
         y = "Total infection probability")
}

### Plot: total illness probability 
#' @title plot total illness probability 
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @param tolerance accecptable tolerance level of risk (default: 1E-4)
#' @return ggplot for total illness probability 
#' @import ggplot2
#' @export
plot_total_illnessProb <- function(risk, tolerance = 0.0001 ) {
  risk$output$total %>%  
    dplyr::mutate(label = ifelse(.data$illnessProb_sum > 0,
                                 sprintf("%d per %d",
                                         floor(.data$illnessProb_sum/tolerance),
                                         floor(1/tolerance)),
                                 "Zero")) %>% 
ggplot(ggplot2::aes_string(x = "TreatmentSchemeName", 
                              y = "illnessProb_sum", 
                              col = "PathogenName",
                              size = 2)) + 
  geom_jitter(width = 0.05,
                alpha = 0.4) + 
  scale_size(guide = 'none') +
  geom_text(aes_string(label = "label"),
            nudge_x = 0.2) + 
  geom_hline(yintercept = tolerance, 
             col = "grey", 
             alpha = 0.5,
             lty = 2, 
             lwd = 1.5) +
  geom_text(x = 0.7, 
            y = tolerance, 
            label = sprintf("1 infection per %d people", 
                            floor(1/tolerance)), 
            inherit.aes = FALSE,
            col = "grey") +
  coord_trans(y = "log10") +
  ylogscale(risk$output$total$illnessProb_sum, 
            userLimit = tolerance) +
  myggtheme() +
  labs(x = "Treatment scheme", 
       y = "Total illness probability")
}

### Plot: total DALYs
#' @title plot total DALYs
#' @description plotting
#' @param risk list as retrieved by simulate_risk()
#' @param labelling if TRUE labels with absolute DALYs will be plotted (default: 
#' FALSE)
#' @param title title for plot (default: "")
#' @param tolerance accecptable tolerance level of risk (default: 1E-6)
#' @return ggplot for total DALYs
#' @import ggplot2
#' @export
plot_total_dalys <- function(risk, 
                             labelling = FALSE, 
                             title = "",
                             tolerance = 1E-6) {
  tmp <- risk$output$total %>%  
    dplyr::mutate(label = ifelse(.data$dalys_sum > 0,
                                 sprintf("%d per %d",
                                         floor(.data$dalys_sum/tolerance),
                                         floor(1/tolerance)),
                                 "Zero"))
  
  gg <- ggplot(tmp, aes_string(x = "TreatmentSchemeName", 
                              y = "dalys_sum", 
                              col = "PathogenName")) +
  geom_jitter(width = 0.8,
              alpha = 0.4,
              size = 1.5) + 
  scale_size(guide = 'none') +
  ylogscale(risk$output$total$dalys_sum, 
            userLimit = tolerance) +
  geom_hline(yintercept = tolerance, 
             col = "grey", 
             alpha = 0.5,
             lty = 2, 
             lwd = 1.5) 
  
  if (labelling) {
  gg <- gg + geom_text(ggplot2::aes_string(label = "label"),
            nudge_x = 0.2)
  }
  gg + geom_text(x = 1, 
              y = tolerance, 
              label = sprintf("1 DALY per %d people", 
                              floor(1/tolerance)), 
              inherit.aes = FALSE,
              col = "grey") +
  coord_trans(y = "log10") +
  myggtheme() +
  labs(x = "Treatment scheme", 
       y = "Total DALYs") +
  ggtitle(title)
}