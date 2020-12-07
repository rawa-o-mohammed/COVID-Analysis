recoding_requests <- function(r, loop) {
  #FOOD CONSUMPTIONS SCORE
  r$fcs <-
    (as.numeric(r$cereals) * 2) + (as.numeric(r$nuts_seed) * 3) + (as.numeric(r$milk_dairy) * 4) + (as.numeric(r$meat) * 4) +
    as.numeric(r$vegetables) + as.numeric(r$fruits) + (as.numeric(r$oil_fats) * 0.5) + (as.numeric(r$sweets) * 0.5)

  r$fcs_strategies <-
    case_when(r$fcs <= 21 ~ 4, r$fcs > 21 & r$fcs < 35 ~ 3, TRUE ~ 1)
  
  r$g14 <- case_when(r$fcs_strategies >= 3 ~ 1, TRUE ~ 0)
  
  
  return(r)
}
