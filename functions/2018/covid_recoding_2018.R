covid_recoding_2018 <- function(df, loop) {
  ############################c14 ######################################
  
  df$c13 <-
    ifelse(df$livelihoods.income.inc_employment < 480000, 1, 0)
  
  df$stress <-
    ifelse(
      df$food_security.coping_strategies_food2.selling_assets %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.borrow_debt  %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.reduce_spending %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.spent_savings %in% c("no_already_did", "yes")   ,
      1,
      0
    )
  df$crisis <-
    ifelse(
      df$food_security.coping_strategies_food2.selling_transportation_means %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.change_place  %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.child_work %in% c("no_already_did", "yes"),
      1,
      0
    )
  df$emergency <-
    ifelse(
      df$food_security.coping_strategies_food2.child_droput_school %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.male_illigal_acts %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.family_migrating %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.child_marriage %in% c("no_already_did", "yes"),
      1,
      0
    )
  
  #FOOD EXPENDITURE SHARE
  df$food_share <-
    df$livelihoods.expenses.food_exp_basic_needs / df$livelihoods.expenses.tot_expenditure
  
  #FOOD CONSUMPTIONS SCORE
  df$fcs <-
    (as.numeric(df$food_security.fcs.cereals) * 2) + (as.numeric(df$food_security.fcs.nuts_seed) * 3) + (as.numeric(df$food_security.fcs.milk_dairy) * 4) + (as.numeric(df$food_security.fcs.meat) * 4) +
    as.numeric(df$food_security.fcs.vegetables) + as.numeric(df$food_security.fcs.fruits) + (as.numeric(df$food_security.fcs.oil_fats) * 0.5) + (as.numeric(df$food_security.fcs.sweets) * 0.5)
  
  df$livelihood_strategies <-
    case_when(df$emergency == 1 ~ 4,
              df$crisis == 1 ~ 3,
              df$stress == 1 ~ 2,
              TRUE ~ 1)
  df$food_share_strategies <-
    case_when(
      df$food_share < 0.5 ~ 1,
      between(df$food_share, 0.5, 0.6499) ~ 2,
      between(df$food_share, 0.65, 0.7499) ~ 3,
      df$food_share >= 0.75 ~ 4
    )
  df$fcs_strategies <-
    case_when(df$fcs < 21 ~ 4, between(df$fcs, 21, 35) ~ 3, TRUE ~ 1)
  
  df$mean_coping_capacity <-
    mean_row(df$livelihood_strategies,
             df$food_share_strategies,
             na.rm = TRUE)
  
  df$c14 <-
    round2(mean_row(df$mean_coping_capacity, df$fcs_strategies, na.rm = TRUE))
  
  
  df$t6 <- ifelse(df$crisis == 1 | df$stress == 1 | df$emergency == 1, 1, 0)
  df$t8 <- ifelse(df$livelihoods.expenses.medical_exp_basic_needs / df$livelihoods.expenses.tot_expenditure >= 0.2, 1, 0)
  ############################c15 ##############################################
  
  df$c15 <- ifelse(df$fcs_strategies == 4, 1, 0)
  
  ############################c16 ###############################################
  
  df$c16 <-
    ifelse(
      df$food_security.coping_strategies_food2.selling_assets %in% c("No_already_did", "yes") |
        df$food_security.coping_strategies_food2.borrow_debt  %in% c("No_already_did", "yes") |
        df$food_security.coping_strategies_food2.reduce_spending %in% c("No_already_did", "yes") |
        df$food_security.coping_strategies_food2.child_work %in% c("No_already_did", "yes") |
        df$food_security.coping_strategies_food2.male_illigal_acts %in% c("No_already_did", "yes"),
      1,
      0
    )
  
  ############################c17 ###############################################
  
  df$c17 <-
    ifelse(
      df$livelihoods.expenses.tot_expenditure * 0.4 <= df$livelihoods.expenses.food_exp_basic_needs,
      1,
      0
    )
  
  ############################c18 ###############################################
  
  df$c18 <-
    ifelse(df$health_group.child_distress == "yes", 1, 0)
  
  ############################c21 ###############################################
  
  df$c21 <-
    apply(
      df,
      1,
      FUN = function(x) {
        ifelse(any(
          loop$Household_Roster.member.age[which(loop$X_submission__uuid == x["X_uuid"])] > 17 &
            loop$Household_Roster.member.work[which(loop$X_submission__uuid == x["X_uuid"])] == "no" &
            loop$Household_Roster.member.actively_seek_work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"
        ),
        1,
        0)
      }
    )
  ############################c22 ###############################################
  
  df$c22 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(loop$Household_Roster.member.age[which(loop$X_submission__uuid == x["X_uuid"])] < 18 &
                   loop$Household_Roster.member.work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
    }
  )
  
  ############################c25 ###############################################
  
  df$female_hhh <-
    ifelse(
      loop$Household_Roster.member.relationship[match(df$X_uuid, loop$X_submission__uuid)] == "head" &
        loop$Household_Roster.member.sex[match(df$X_uuid, loop$X_submission__uuid)] == "female" &
        loop$Household_Roster.member.marital_status[match(df$X_uuid, loop$X_submission__uuid)] %in% c("single", "widowed", "divorced", "separated"),
      1,
      0
    )
  df$c25 <-
    case_when(df$female_hhh == 1 &
                df$livelihoods.income.inc_employment < 480000 ~ 1,
              df$female_hhh == 1 &
                df$livelihoods.income.inc_employment >= 480000 ~ 0,
              TRUE ~ NA_real_)
  
  ############################c26 ###############################################
  
  df$c26 <-
    ifelse(df$livelihoods.primary_livelihood.NGO_charity_assistance == 1,
           1,
           0)
  
  ############################c27 ###############################################
  
  df$c27 <-
    ifelse(df$livelihoods.employment_seasonal == "yes", 1, 0)
  
  ############################c28 ###############################################
  
  df$c28 <-
    ifelse(
      df$livelihoods.expenses.tot_expenditure * 0.3 <= df$livelihoods.expenses.rent_exp_basic_needs,
      1,
      0
    )
  
  ############################c29 ###############################################
  
  df$c29 <-
    ifelse(df$livelihoods.how_much_debt > 505000, 1, 0)
  
  ############################c30 ###############################################
  
  df$c30 <-
    ifelse(
      df$livelihoods.reasons_for_debt %in% c(
        "basic_hh_expenditure",
        "health",
        "health Food",
        "food",
        "Education"
      ),
      1,
      0
    )
  
  ############################c32 ###############################################
  
  df$c32 <- ifelse(df$S_NFI.hh_risk_eviction == "yes", 1, 0)
  
  df$c37 <-
    ifelse(df$accountability.aid_received == "yes", 1, 0)
  
  df$c39 <-
    ifelse(df$accountability.aid_satisfaction == "yes", 1, 0)
  
  df$c40_i   <-
    case_when(
      df$accountability.aid_not_satisfied == "quality" ~ 1,
      (df$accountability.aid_satisfaction == "yes" &
        df$accountability.aid_received == "yes") |
        df$accountability.aid_not_satisfied %in% c("quantity", "delays", "other") ~ 0,
      TRUE ~ NA_real_
    )
  df$c40_ii  <-
    case_when(
      df$accountability.aid_not_satisfied == "quantity" ~ 1,
      (df$accountability.aid_satisfaction == "yes" &
        df$accountability.aid_received == "yes") |
        df$accountability.aid_not_satisfied %in% c("quality", "delays", "other") ~ 0,
      TRUE ~ NA_real_
    )
  df$c40_iii <-
    case_when(
      df$accountability.aid_not_satisfied == "delays" ~ 1,
      (df$accountability.aid_satisfaction == "yes" &
        df$accountability.aid_received == "yes") |
        df$accountability.aid_not_satisfied %in% c("quantity", "quality", "other") ~ 0,
      TRUE ~ NA_real_
    )
  df$c40_iv  <-
    case_when(
      df$accountability.aid_not_satisfied == "other" ~ 1,
      (df$accountability.aid_satisfaction == "yes" &
        df$accountability.aid_received == "yes") |
        df$accountability.aid_not_satisfied %in% c("quantity", "delays", "quality") ~ 0,
      TRUE ~ NA_real_
    )
  return(df)
}
