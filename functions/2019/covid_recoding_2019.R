covid_recoding_2019 <- function(df, loop) {
  ############################c14 for 2019 ######################################
  df <- df %>%
    new_recoding(target = c) %>%
    recode_to(
      1,
      where = passport_u18 == "no" |
        id_card_u18 == "no" |
        citizenship_u18 == "no" | birth_cert_u18 == "no" |
        marriage_cert_u18 == "non_valid" |
        divorce_cert_u18 == "non_valid"
    ) %>%
    recode_to(
      0,
      where = !(
        passport_u18 == "no" |
          id_card_u18 == "no" |
          citizenship_u18 == "no" | birth_cert_u18 == "no" |
          marriage_cert_u18 == "non_valid" |
          divorce_cert_u18 == "non_valid"
      )
    )
    
    
    df$stress <-
      ifelse(
        df$selling_assets %in% c("no_already_did", "yes") |
          df$borrow_debt  %in% c("no_already_did", "yes") |
          df$reduce_spending %in% c("no_already_did", "yes") |
          df$spending_savings %in% c("no_already_did", "yes")   ,
        1,
        0
      )
    df$crisis <-
      ifelse(
        df$selling_transportation_means %in% c("no_already_did", "yes") |
          df$change_place  %in% c("no_already_did", "yes") |
          df$child_work %in% c("no_already_did", "yes"),
        1,
        0
      )
    df$emergency <-
      ifelse(
        df$child_dropout_school %in% c("no_already_did", "yes") |
          df$adult_risky  %in% c("no_already_did", "yes") |
          df$family_migrating %in% c("no_already_did", "yes") |
          df$child_forced_marriage %in% c("no_already_did", "yes"),
        1,
        0
      )
    
    #FOOD EXPENDITURE SHARE
    df$food_share <- df$food_exp / df$tot_expenditure
    
    #FOOD CONSUMPTIONS SCORE
    df$fcs <-
      as.numeric(df$cereals) * 2 + as.numeric(df$nuts_seed) * 3 + as.numeric(df$milk_dairy) * 4 + as.numeric(df$meat) * 4 +
      as.numeric(df$vegetables) + as.numeric(df$fruits) + as.numeric(df$oil_fats) * 0.5 + as.numeric(df$sweets) * 0.5
    
    df$livelihood_strategies <-
      case_when(df$emergency == 1 ~ 4,
                df$crisis == 1 ~ 3,
                df$stress == 1 ~ 2,
                TRUE ~ 1)
    df$food_share_strategies <-
      case_when(
        df$food_share <= 0.5 ~ 1,
        (df$food_share > 0.5 & df$food_share <= 0.65) ~ 2,
        (df$food_share > 0.65 & df$food_share <= 0.75) ~ 3,
        TRUE ~ 4
      )
    df$fcs_strategies <-
      case_when(df$fcs <= 28 ~ 4, (df$fcs > 28 &
                                     df$fcs <= 42) ~ 3, TRUE ~ 1)
    
    df$mean_coping_capacity <-
      rowMeans(df[, c("livelihood_strategies", "food_share_strategies")])
    df$c14 <-
      rowMeans(df[, c("mean_coping_capacity", "fcs_strategies")])
    
    df$health_share <- df$medical_exp / df$tot_expenditure
    df$t5 <- ifelse(df$c14 >= 3, 1, 0)
    df$t6 <-
      ifelse(df$crisis == 1 | df$stress == 1 | df$emergency == 1, 1, 0)
    df$t8 <- ifelse(df$health_share >= 0.2, 1, 0)
    
    ############################c14 for 2020 ######################################
    
    df$c15 <- ifelse(df$fcs_strategies == 4, 1, 0)
    
    df$c16 <-
      ifelse(
        df$selling_assets %in% c("no_already_did", "yes") |
          df$borrow_debt  %in% c("no_already_did", "yes") |
          df$reduce_spending %in% c("no_already_did", "yes") |
          df$child_work %in% c("no_already_did", "yes") |
          df$adult_risky  %in% c("no_already_did", "yes"),
        1,
        0
      )
    df$c16_i <-
      case_when(
        df$selling_assets %in% c("no_already_did", "yes") ~ 1,
        df$selling_assets == "no_nobody_in_hh_did" ~ 0,
        TRUE ~ NA_real_
      )
    
    df$c16_ii <-
      case_when(
        df$borrow_debt %in% c("no_already_did", "yes") ~ 1,
        df$borrow_debt == "no_nobody_in_hh_did" ~ 0,
        TRUE ~ NA_real_
      )
    
    df$c16_iii <-
      case_when(
        df$reduce_spending %in% c("no_already_did", "yes") ~ 1,
        df$reduce_spending == "no_nobody_in_hh_did" ~ 0,
        TRUE ~ NA_real_
      )
    
    df$c16_iv <-
      case_when(
        df$adult_risky %in% c("no_already_did", "yes") ~ 1,
        df$adult_risky == "no_nobody_in_hh_did" ~ 0,
        TRUE ~ NA_real_
      )
    
    df$c16_v <-
      case_when(
        df$child_work %in% c("no_already_did", "yes") ~ 1,
        df$child_work == "no_nobody_in_hh_did" ~ 0,
        TRUE ~ NA_real_
      )
    df$c17 <-
      ifelse(df$tot_expenditure * 0.4 <= df$food_exp, 1, 0)
    
    #not validated
    df$c18_i <-     ifelse(df$child_distress_number < 1 |
                             is.na(df$child_distress_number),
                           0,
                           1)
    
    #not validated
    df$c18_ii <- df$child_distress_number
    
    ############################c19 ###############################################
    
    #not validated
    df$c19_i <-
      ifelse(df$adult_distress_number < 1 |
               is.na(df$adult_distress_number),
             0,
             1)
    #not validated
    df$c19_ii <- df$adult_distress_number
    
    #not validated
    df$c20 <- ifelse((
      df$adult_distress_number < 1 |
        is.na(df$adult_distress_number)
    ) & (
      df$child_distress_number < 1 |
        is.na(df$child_distress_number)
    ),
    0 ,
    1)
    
    df$c21 <- apply(
      df,
      1,
      FUN = function(x) {
        ifelse(any(
          loop$age[which(loop$X_submission__uuid == x["X_uuid"])] > 17 &
            loop$work[which(loop$X_submission__uuid == x["X_uuid"])] == "no" &
            loop$actively_seek_work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"
        ),
        1,
        0)
      }
    )
    
    df$c22 <- apply(
      df,
      1,
      FUN = function(x) {
        ifelse(any(loop$age[which(loop$X_submission__uuid == x["X_uuid"])] < 18 &
                     loop$work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
      }
    )
    
    ############################c24 ###############################################
    
    df$c24 <- ifelse(df$inc_employment < 480000, 1, 0)
    
    df_female_headed <-
      df[which(df$X_uuid %in% loop$X_submission__uuid[which(
        loop$sex == "female" &
          loop$relationship == "head" &
          loop$marital_status %in% c("single", "separated", "widowed", "divorced")
      )]), ]
    df$c25 <- NA
    df$c25[which(df$X_uuid %in% df_female_headed$X_uuid)] <-
      ifelse(rowSums(df_female_headed[, c("inc_employment", "inc_pension")], na.rm =
                       T) < 480000, 1, 0)
    
    df$c26 <-
      ifelse(df$primary_livelihood.ngo_charity_assistance == 1, 1, 0)
    
    df$c27 <- ifelse(df$employment_seasonal == "yes", 1, 0)
    
    #not validated
    df$c28 <-
      ifelse(df$tot_expenditure * 0.3 <= df$rent_exp, 1, 0)
    
    df$c29 <- ifelse(df$how_much_debt > 505000, 1, 0)
    
    df$c30_1 <-
      ifelse(
        df$reasons_for_debt %in%  c("basic_hh_expenditure",
                                    "health",
                                    "food",
                                    "education"),
        1,
        0
      )
    
    df$c30_2_i <-
      ifelse(df$reasons_for_debt == "basic_hh_expenditure", 1, 0)
    #not validated
    df$c30_2_ii <- ifelse(df$reasons_for_debt == "health", 1, 0)
    #not validated
    df$c30_2_iii <- ifelse(df$reasons_for_debt == "food", 1, 0)
    #not validated
    df$c30_2_iv <-
      ifelse(df$reasons_for_debt == "education", 1, 0)
    #not validated
    df$c30_2_v <-
      ifelse(df$reasons_for_debt == "clothing", 1, 0)
    #not validated
    df$c30_2_vi <-
      ifelse(df$reasons_for_debt == "purchase_pro_assets", 1, 0)
    
    #not validated
    df$c32 <- ifelse(df$hh_risk_eviction == "yes", 1, 0)
    
    ############################c33 ###############################################
    
    #not validated
    df$c33_i   <- ifelse(df$hh_main_risks.lack_funds == 1, 1, 0)
    #not validated
    df$c33_ii  <-
      ifelse(df$hh_main_risks.no_longer_hosted == 1, 1, 0)
    #not validated
    df$c33_iii <-
      ifelse(df$hh_main_risks.no_agreement == 1, 1, 0)
    #not validated
    df$c33_iv <-
      ifelse(df$hh_main_risks.owner_request == 1, 1, 0)
    
    
    df$c37 <- ifelse(df$aid_received == "yes", 1, 0)
    df$c38 <-
      ifelse(df$aid_received == "yes" &
               df$aid_type.healthcare == 1, 1, 0)
    
    df$c39 <- ifelse(df$aid_satisfaction == "yes",
                     1,
                     ifelse(
                       df$aid_satisfaction %in% c("decline_to_answer", "do_not_know", "no"),
                       0,
                       NA
                     ))
    
    #not validated
    df$c40_i  <- case_when(
      df$aid_not_satisfied.quantity == 1 ~ 1,
      (df$aid_received == "yes" &
         df$aid_satisfaction == "yes") |
        df$aid_not_satisfied.quantity == 0 ~ 0,
      TRUE ~  NA_real_
    )
    #not validated
    df$c40_ii <- case_when(
      df$aid_not_satisfied.quantity == 1 ~ 1,
      (df$aid_received == "yes" &
         df$aid_satisfaction == "yes") |
        df$aid_not_satisfied.quantity == 0 ~ 0,
      TRUE ~  NA_real_
    )
    #not validated
    df$c40_iii <- case_when(
      df$aid_not_satisfied.quantity == 1 ~ 1,
      (df$aid_received == "yes" &
         df$aid_satisfaction == "yes") |
        df$aid_not_satisfied.quantity == 0 ~ 0,
      TRUE ~  NA_real_
    )
    #not validated
    df$c40_iv <- case_when(
      df$aid_not_satisfied.quantity == 1 ~ 1,
      (df$aid_received == "yes" &
         df$aid_satisfaction == "yes") |
        df$aid_not_satisfied.quantity == 0 ~ 0,
      TRUE ~  NA_real_
    )
    
    df$c45 <- df$info_aid.healthcare
    #not validated
    df$c46 <- case_when(
      df$restriction_clearance == "yes" |
        df$restriction_documents == "yes" |
        df$restriction_physical == "yes" |
        df$restriction_time == "yes" |
        df$restriction_reason == "yes" |
        df$restriction_other == "yes" ~ 1,
      df$restriction_clearance == "no" &
        df$restriction_documents == "no" &
        df$restriction_physical == "no" &
        df$restriction_time == "no" &
        df$restriction_reason == "no" &
        df$restriction_other == "no" ~ 0,
      TRUE ~ NA_real_
    )
    
    #not validated
    df$c47_i <-
      ifelse(df$employment_primary_barriers.increased_competition == 1,
             1,
             0)
    #not validated
    df$c47_ii <-
      ifelse(df$employment_primary_barriers.lack_of_connections  == 1,
             1,
             0)
    #not validated
    df$c47_iii <-
      ifelse(df$employment_primary_barriers.underqualified_for_jobs == 1,
             1,
             0)
    #not validated
    df$c47_iv <-
      ifelse(df$employment_primary_barriers.jobs_far == 1, 1, 0)
    
    
    #not validated
    df$c48 <-
      case_when(
        df$hh_risk_eviction == "yes" &
          df$hh_main_risks.owner_request == 1 ~ 1,
        df$hh_risk_eviction == "yes" &
          df$hh_main_risks.owner_request == 0 ~ 0,
        TRUE ~ NA_real_
      )
    
    #not validated
    df$c49_i <- ifelse(df$health_barriers.cost %in% c(NA, 0), 0, 1)
    #not validated
    df$c49_ii <-
      ifelse(df$health_barriers.no_medicine %in% c(NA, 0), 0, 1)
    #not validated
    df$c49_iii <-
      ifelse(df$health_barriers.no_offered_treatment %in% c(NA, 0), 0, 1)
    #not validated
    df$c49_iv <-
      ifelse(df$health_barriers.distance_to_treatmentcenter %in% c(NA, 0),
             0,
             1)
    #not validated
    df$c49_v <-
      ifelse(df$health_barriers.phc_closed %in% c(NA, 0), 0, 1)
    
    return(df)
}
