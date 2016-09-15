compare_risk <- function(risk_daily, exposure_daysPerYear) {
risk_year_a <- 1-(1-risk_daily)^exposure_daysPerYear
risk_year_b <- risk_daily * exposure_daysPerYear
risk_year_diff <- risk_year_b - risk_year_a 
risk_year_diffPercent <- 100*risk_year_diff/risk_year_a

data.frame(risk_daily = risk_daily,
           exposure_daysPerYear =  exposure_daysPerYear,
           risk_year_a=risk_year_a, 
           risk_year_b=risk_year_b, 
           risk_year_diff=risk_year_diff, 
           risk_year_diffPercent=risk_year_diffPercent)
}

tmp <- compare_risk(risk_daily = c(1E-8,2E-7,1E-6, 1E-4, 1E-3,1/365, 1E-2,1E-1),
             exposure_daysPerYear = 365)

plot(log10(tmp$risk_daily), log10(tmp$risk_year_a))
abline(h=log10(10^-4))
