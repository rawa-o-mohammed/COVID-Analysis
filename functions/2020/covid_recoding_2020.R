covid_recoding_2020 <- function(df, loop) {
  df$a0 <- case_when(df$property_damaged == "yes" ~ 1,
                     df$property_damaged == "no" ~ 0,
                     TRUE ~ NA_real_)
  
  df$a1 <-
    case_when(
      df$a0 == 1 & df$aware_compensation == "yes" ~ 1,
      df$a0 == 1 & df$aware_compensation == "no" ~ 0,
      TRUE ~ 0
    )
  
  #please check - requires validation
  df$a2 <- case_when(
    df$a0 == 1 & df$applied_compensation == "yes" ~ 1,
    df$a0 == 1 & df$applied_compensation == "no" ~ 0,
    TRUE ~ NA_real_
  )
  #please check - requires validation
  df$a3 <- case_when(
    df$a0 == 1 & df$received_compensation == "yes" ~ 1,
    df$a0 == 1 & df$received_compensation == "no" ~ 0,
    TRUE ~ NA_real_
  )
  
  df$a4 <- case_when(
    df$unsafe_areas.facilities == 1 ~ 1,
    df$unsafe_areas.facilities == 0 ~ 0,
    TRUE ~ NA_real_
  )
  
  df$a5 <- case_when(df$unsafe_areas.markets == 1 ~ 1,
                     df$unsafe_areas.markets == 0 ~ 0,
                     TRUE ~ NA_real_)
  
  df$a6 <- case_when(
    df$unsafe_areas.distribution_areas == 1 ~ 1,
    df$unsafe_areas.distribution_areas == 0 ~ 0,
    TRUE ~ NA_real_
  )
  
  
  df$a7 <- case_when(
    df$unsafe_areas.water_points == 1 ~ 1,
    df$unsafe_areas.water_points == 0 ~ 0,
    TRUE ~ NA_real_
  )
  
  df$a8 <- case_when(
    df$unsafe_areas.social_areas == 1 ~ 1,
    df$unsafe_areas.social_areas == 0 ~ 0,
    TRUE ~ NA_real_
  )
  
  df$a9 <- case_when(
    df$unsafe_areas.way_to_school == 1 ~ 1,
    df$unsafe_areas.way_to_school == 0 ~ 0,
    TRUE ~ NA_real_
  )
  
  df$a10 <- case_when(
    df$unsafe_areas.way_to_centers == 1 ~ 1,
    df$unsafe_areas.way_to_centers == 0 ~ 0,
    TRUE ~ NA_real_
  )
  
  df$a11 <- case_when((
    df$a4 == 1 |
      df$a5 == 1 |
      df$a6 == 1 |
      df$a7 == 1 |
      df$a8 == 1 |
      df$a9 == 1 |
      df$a10 == 1
  ) &
    df$gender_respondent == "female" ~ 1,
  df$gender_respondent == "female" ~ 0,
  TRUE ~ NA_real_
  )
  
  df$a12 <-
    ifelse(
      df$distance_hospital %in% c("less_15", "less_30") |
        df$distance_clinic %in% c("less_15", "less_30"),
      1,
      0
    )
  
  df$a13 <-
    ifelse(df$access_soap == "yes" &
             df$use_of_soap.handwashing == 1,
           1,
           0)
  
  df$a14 <- ifelse(
    df$health_barriers.civ_docs_problems == 1 |
      df$health_barriers.cost == 1 |
      df$health_barriers.distance_to_treatmentcenter == 1 |
      df$health_barriers.no_fem_staff == 1 |
      df$health_barriers.no_medicine == 1 |
      df$health_barriers.no_offered_treatment == 1 |
      df$health_barriers.no_referral_phc == 1 |
      df$health_barriers.not_inclusive == 1 |
      df$health_barriers.phc_closed == 1 |
      df$health_barriers.refused_treatment == 1 |
      df$health_barriers.unqualified_staff == 1,
    1,
    0
  )
  
  df$a15_i <-
    case_when(
      df$distance_hospital == "less_15" &
        df$hospital_emergency_ser == "yes" ~ 1,
      df$hospital_emergency_ser %in% c("do_not_know", "no") ~ NA_real_,
      TRUE ~ 0
    )
  
  #please check - requires validation
  df$a15_ii <-
    case_when(
      df$distance_hospital == "less_30" &
        df$hospital_emergency_ser == "yes" ~ 1,
      df$hospital_emergency_ser %in% c("do_not_know", "no") ~ NA_real_,
      TRUE ~ 0
    )
  
  
  #please check - requires validation
  df$a15_iii <-
    case_when(
      df$distance_hospital == "less_hour" &
        df$hospital_emergency_ser == "yes" ~ 1,
      df$hospital_emergency_ser %in% c("do_not_know", "no") ~ NA_real_,
      TRUE ~ 0
    )
  
  
  df$a15_iv <-
    case_when(
      df$distance_hospital %in% c("less_3hours", "more_3hours") &
        df$hospital_emergency_ser == "yes" ~ 1,
      df$hospital_emergency_ser %in% c("do_not_know", "no") ~ NA_real_,
      TRUE ~ 0
    )
  
  #c7, c8 and c10 are moved to the individual analysis
  df$c9 <-
    ifelse(
      df$distance_hospital %in% c("less_15", "less_30", "less_hour") |
        df$distance_clinic %in% c("less_15", "less_30", "less_hour"),
      1,
      0
    )
  
  df$c11_i <- df$health_barriers.cost
  df$c11_ii <- df$health_barriers.phc_closed
  df$c11_iii <- df$health_barriers.distance_to_treatmentcenter
  df$c11_iv <- df$health_barriers.no_medicine
  df$c11_v <- df$health_barriers.no_offered_treatment
  
  df$c12_i <-
    ifelse(df$access_soap == "yes" &
             df$use_of_soap.handwashing == 1,
           1,
           0)

    df$c12_ii <-
    ifelse(df$access_soap == "yes" &
             df$use_of_soap.bathing == 1,
           1,
           0)

    df$c12_iii <-
    ifelse(df$access_soap == "yes" &
             df$use_of_soap.laundry == 1,
           1,
           0)

    df$c12_iv <-
    ifelse(df$access_soap == "yes" &
             df$use_of_soap.dish_washing == 1,
           1,
           0)

  df$c12_v <-
    ifelse(df$access_soap == "yes" &
             df$use_of_soap.not == 1,
           1,
           0)
  
  df$c13 <-
    ifelse(df$access_hygiene_items %in% c("satisfied", "very_satisfied"),
           1,
           0)
  
  ############################c14 ###############################################
  
  
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
  df$food_share <- df$food_exp / df$tot_expenses
  
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
    mean_row(df$livelihood_strategies,
             df$food_share_strategies,
             na.rm = TRUE)
  df$c14 <-
    round2(mean_row(df$mean_coping_capacity, df$fcs_strategies, na.rm = TRUE))
  
  #please check - requires validation
  df$c15_i <- ifelse(df$fcs_strategies == 4, 1, 0)
  #please check - requires validation
  df$c15_ii <- ifelse(df$fcs_strategies == 3, 1, 0)
  #please check - requires validation
  df$c15_iii <- ifelse(df$fcs_strategies == 1, 1, 0)
  
  df$health_share <- df$medical_exp / df$tot_expenses
  
  df$t1 <- ifelse(df$c14 == 1, 1, 0)
  df$t2 <- ifelse(df$c14 == 2, 1, 0)
  df$t3 <- ifelse(df$c14 == 3, 1, 0)
  df$t4 <- ifelse(df$c14 == 4, 1, 0)
  df$t5 <- ifelse(df$c14 >= 3, 1, 0)
  df$t6 <-
    ifelse(df$crisis == 1 |
             df$stress == 1 | df$emergency == 1, 1, 0)
  df$t8 <- ifelse(df$health_share >= 0.2, 1, 0)
  df$t9 <-
    ifelse(df$health_share >= 0 & df$health_share < 0.1, 1, 0)
  df$t10 <-
    ifelse(df$health_share >= 0.1 & df$health_share < 0.3, 1, 0)
  df$t11 <- ifelse(df$health_share >= 0.3, 1, 0)

  #df$t12 wasn't needed and not used in the FS, so it's deleted
  ############################c16 ###############################################
  
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
  
  ############################c17 ###############################################
  
  df$c17 <-
    ifelse(df$food_share >= 0.4, 1, 0)
  df$c17_i <-
    ifelse(df$food_share >= 0 & df$food_share < 0.2, 1, 0)
  df$c17_ii <-
  df$c17_iii <-
    ifelse(df$food_share >= 0.4 & df$food_share < 0.6, 1, 0)
  df$c17_iv <-
    ifelse(df$food_share >= 0.6 & df$food_share < 0.8, 1, 0)
  df$c17_v <-
    ifelse(df$food_share >= 0.8 & df$food_share <= 1, 1, 0)
  
  ############################c18 ###############################################
  
  df$c18 <- case_when(
    df$child_distress_number > 0 ~ 1,
    df$child_distress_number == 0 |
      df$hh_member_distress == "no" ~ 0,
    TRUE ~ NA_real_
  )
  
  #df$c18_ii was moved to the individual analysis
  ############################c19 ###############################################
  
  #please check - requires validation
  df$c19 <- case_when(
    df$adult_distress_number > 0 ~ 1,
    df$adult_distress_number == 0 |
      df$hh_member_distress == "no" ~ 0,
    TRUE ~ NA_real_
  )
  
  #df$c19_ii was moved to the individual analysis
  ############################c21 ###############################################
  
  df$c21 <-
    apply(
      df,
      1,
      FUN = function(x) {
        ifelse(any(
          loop$age[which(loop$X_uuid == x["X_uuid"])] > 17 &
            loop$work[which(loop$X_uuid == x["X_uuid"])] == "no" &
            loop$actively_seek_work[which(loop$X_uuid == x["X_uuid"])] == "yes"
        ),
        1,
        0)
      }
    )
  
  ############################c22 ###############################################
  indiv_hoh <- loop[which(loop$relationship == "head"),]
  df$c22 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(loop$age[which(loop$X_uuid == x["X_uuid"])] < 18 &
                   loop$work[which(loop$X_uuid == x["X_uuid"])] == "yes"), 1, 0)
    }
  )
  
  ############################c23 ###############################################
  
  df$c23_i   <-
    df$employment_primary_barriers.increased_competition
  df$c23_ii  <- df$employment_primary_barriers.jobs_far
  df$c23_iii <-
    df$employment_primary_barriers.only_low_available
  df$c23_iv  <- df$employment_primary_barriers.lack_jobs_women
  
  ############################c24 ###############################################
  
  #please check - requires validation
  #the dap updated
  df$c24_i <-
    case_when(
      df$inc_employment_pension < 480000 ~ 1,
      df$inc_employment_pension >= 480000 ~ 0,
      TRUE ~ NA_real_
    )
  #please check - requires validation
  #the dap updated
  df$c24_ii <-
    case_when(
      df$inc_employment_pension >= 480000 &
        df$inc_employment_pension < 800000 ~ 1,
      
      df$inc_employment_pension < 480000 |
        df$inc_employment_pension >= 800000 ~ 0,
      TRUE ~ NA_real_
    )
  #please check - requires validation
  #the dap updated
  df$c24_iii <-
    case_when(
      df$inc_employment_pension >= 800000 ~ 1,
      df$inc_employment_pension < 800000 ~ 0,
      TRUE ~ NA_real_
    )
  
  ############################c25 ###############################################
  df$single_headed_household <-
    case_when(
      indiv_hoh$marital_status[match(df$X_uuid, indiv_hoh$`X_uuid`)] %in%
        c("single", "separated", "widowed", "divorced") ~ 1,
      indiv_hoh$marital_status[match(df$X_uuid, indiv_hoh$`X_uuid`)] == "married" ~ 0,
      TRUE ~ NA_real_
    )
  df$gender_hhh <- indiv_hoh$sex[match(df$X_uuid, indiv_hoh$X_uuid)]
  df$single_female_hhh <-
    case_when(
      df$single_headed_household == 1 & df$gender_hhh == "female" ~ 1,
      is.na(df$single_headed_household) &
        is.na(df$gender_hhh)  ~ NA_real_,
      TRUE ~ 0
    )
  df$c25_i <-
    case_when(
      df$inc_employment_pension < 480000 &
        df$single_female_hhh == 1 ~ 1,
      df$inc_employment_pension >= 480000 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c25_ii <-
    case_when(
      df$inc_employment_pension >= 480000 &
        df$inc_employment_pension < 800000 &
        df$single_female_hhh == 1 ~ 1,
      (
        df$inc_employment_pension < 480000 |
          df$inc_employment_pension >= 800000
      ) &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c25_iii <-
    case_when(
      df$inc_employment_pension >= 800000 &
        df$single_female_hhh == 1 ~ 1,
      df$inc_employment_pension < 800000 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ############################c26 ###############################################
  
  df$c26 <-
    ifelse(df$primary_livelihood.ngo_charity_assistance == 1, 1, 0)
  
  ############################c27 ###############################################
  
  df$c27 <- ifelse(df$employment_seasonal == "yes", 1, 0)
  
  ############################c28 ###############################################
  df$rent_share <- df$rent / df$tot_expenses
  df$c28 <-
    ifelse(df$rent_share >= 0.3, 1, 0)
  df$c28_i <-
    ifelse(df$rent_share >= 0 & df$rent_share < 0.2, 1, 0)
  df$c28_ii <-
    ifelse(df$rent_share >= 0.2 & df$rent_share < 0.4, 1, 0)
  df$c28_iii <-
    ifelse(df$rent_share >= 0.4 & df$rent_share < 0.6, 1, 0)
  df$c28_iv <-
    ifelse(df$rent_share >= 0.6 & df$rent_share < 0.8, 1, 0)
  df$c28_v <-
    ifelse(df$rent_share >= 0.8 & df$rent_share <= 1, 1, 0)
  
  
  ############################c29 ###############################################
  
  #please check - requires validation
  #the dap updated
  df$c29_i <-
    case_when(df$how_much_debt >= 1000000 ~ 1,
              df$how_much_debt < 1000000 ~ 0,
              TRUE ~ NA_real_)
  #please check - requires validation
  #the dap updated
  df$c29_ii <-
    case_when(
      df$how_much_debt >= 505000 &
        df$how_much_debt < 1000000 ~ 1,
      (df$how_much_debt < 505000 |
         df$how_much_debt >= 1000000) ~ 0,
      TRUE ~ NA_real_
    )
  #please check - requires validation
  #the dap updated
  df$c29_iii <-
    case_when(df$how_much_debt < 505000 ~ 1,
              df$how_much_debt >= 505000 ~ 0,
              TRUE ~ NA_real_)
  #please check - requires validation
  #the dap updated
  df$c29 <- ifelse(df$how_much_debt > 505000, 1, 0)
  
  ############################c30 ###############################################
  
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
  df$c30_2_ii <- ifelse(df$reasons_for_debt == "health", 1, 0)
  df$c30_2_iii <- ifelse(df$reasons_for_debt == "food", 1, 0)
  df$c30_2_iv <-
    ifelse(df$reasons_for_debt == "education", 1, 0)
  df$c30_2_v <-
    ifelse(df$reasons_for_debt == "clothing", 1, 0)
  df$c30_2_vi <-
    ifelse(df$reasons_for_debt == "purchase_pro_assets", 1, 0)
  
  ############################c31 ###############################################
  
  df$c31_1 <- ifelse(df$covid_loss_job == "yes"  ,
                     1,
                     0)
  
  #please check - requires validation
  df$c31_2 <- case_when(df$covid_loss_job_permanent > 0 ~ 1,
                        df$covid_loss_job_permanent == 0 ~ 0,
                        TRUE ~ 0)
  
  #please check - requires validation
  df$c31_3 <- case_when(df$covid_loss_job_temp > 0 ~ 1,
                        df$covid_loss_job_temp == 0 ~ 0,
                        TRUE ~ 0)
  
  ############################c32 ###############################################
  
  df$c32 <- case_when(df$hh_risk_eviction == "yes" ~ 1,
                      df$hh_risk_eviction == "no" ~ 0,
                      TRUE ~ NA_real_)
  
  ############################c33 ###############################################
  
  #please check - requires validation
  df$c33_i   <- 
    case_when(
      df$hh_risk_eviction == "yes" &
        df$hh_main_risks.lack_funds == 1 ~ 1,
      df$hh_risk_eviction == "yes" &
        df$hh_main_risks.lack_funds == 0 ~ 0,
      TRUE ~ NA_real_
    )
  #please check - requires validation
  df$c33_ii  <-
    case_when(
      df$hh_risk_eviction == "yes" &
        df$hh_main_risks.no_longer_hosted == 1 ~ 1,
      df$hh_risk_eviction == "yes" &
        df$hh_main_risks.no_longer_hosted == 0 ~ 0,
      TRUE ~ NA_real_
    )
  #please check - requires validation
  df$c33_iii <-
    case_when(
      df$hh_risk_eviction == "yes" &
        df$hh_main_risks.no_agreement == 1 ~ 1,
      df$hh_risk_eviction == "yes" &
        df$hh_main_risks.no_agreement == 0 ~ 0,
      TRUE ~ NA_real_
    )
  #please check - requires validation
  df$c33_iv <-
    case_when(
      df$hh_risk_eviction == "yes" &
        df$hh_main_risks.owner_request == 1 ~ 1,
      df$hh_risk_eviction == "yes" &
        df$hh_main_risks.owner_request == 0 ~ 0,
      TRUE ~ NA_real_
    )
  
  
  ############################c34 ###############################################
  
  df$c34_i  <- df$info_aid.healthcare
  df$c34_ii <- df$info_aid.aid
  
  ############################c35 ###############################################
  
  df$c35_1     <- ifelse(df$covid_info_need == "yes", 1, 0)
  
  #please check - requires validation
  df$c35_2_i   <-
    case_when(df$covid_info_type.causes == 1 & df$c35_1 == 1 ~ 1,
              df$c35_1 == 1 ~ 0,
              TRUE ~ NA_real_)
  #please check - requires validation
  df$c35_2_ii  <-
    case_when(df$covid_info_type.signs == 1 & df$c35_1 == 1 ~ 1,
              df$c35_1 == 1 ~ 0,
              TRUE ~ NA_real_)
  #please check - requires validation
  df$c35_2_iii <-
    case_when(df$covid_info_type.prevention == 1 &
                df$c35_1 == 1 ~ 1,
              df$c35_1 == 1 ~ 0,
              TRUE ~ NA_real_)
  #please check - requires validation
  df$c35_2_iv  <-
    case_when(df$covid_info_type.treatment == 1 & df$c35_1 == 1 ~ 1,
              df$c35_1 == 1 ~ 0,
              TRUE ~ NA_real_)
  #please check - requires validation
  df$c35_2_v   <-
    case_when(df$covid_info_type.consequences == 1 &
                df$c35_1 == 1 ~ 1,
              df$c35_1 == 1 ~ 0,
              TRUE ~ NA_real_)
  #please check - requires validation
  df$c35_2_vi  <-
    case_when(df$covid_info_type.other == 1 & df$c35_1 == 1 ~ 1,
              df$c35_1 == 1 ~ 0,
              TRUE ~ NA_real_)
  
  ############################c36 ###############################################
  
  df$c36_i    <- ifelse(df$info_mode.direct_obs == 1, 1, 0)
  df$c36_ii   <- ifelse(df$info_mode.face_cmmunic == 1, 1, 0)
  df$c36_iii  <- ifelse(df$info_mode.television == 1, 1, 0)
  df$c36_iv   <- ifelse(df$info_mode.telephone == 1, 1, 0)
  df$c36_v    <- ifelse(df$info_mode.mobile == 1, 1, 0)
  df$c36_vi   <- ifelse(df$info_mode.facebook_app == 1, 1, 0)
  df$c36_vii  <-
    ifelse(df$info_mode.facebook_messenger == 1, 1, 0)
  df$c36_viii <- ifelse(df$info_mode.whatsapp == 1, 1, 0)
  df$c36_ix   <- ifelse(df$info_mode.viber == 1, 1, 0)
  df$c36_x    <- ifelse(df$info_mode.other_social == 1, 1, 0)
  df$c36_xi   <- ifelse(df$info_mode.notice_board == 1, 1, 0)
  df$c36_xii  <- ifelse(df$info_mode.newspapers == 1, 1, 0)
  df$c36_xiii <- ifelse(df$info_mode.leaflet == 1, 1, 0)
  df$c36_xiv  <- ifelse(df$info_mode.loud_speakers == 1, 1, 0)
  df$c36_xv   <- ifelse(df$info_mode.radio == 1, 1, 0)
  df$c36_xvi  <- ifelse(df$info_mode.other == 1, 1, 0)
  
  ############################c37 ###############################################
  
  df$c37 <- ifelse(df$aid_received == "yes", 1, 0)
  
  ############################c38 ###############################################
  
  df$c38_i     <- ifelse(df$aid_type.cash == 1, 1, 0)
  df$c38_ii    <- ifelse(df$aid_type.food == 1, 1, 0)
  df$c38_iii   <- ifelse(df$aid_type.water == 1, 1, 0)
  df$c38_iv    <- ifelse(df$aid_type.fuel == 1, 1, 0)
  df$c38_v     <- ifelse(df$aid_type.shelter == 1, 1, 0)
  df$c38_vi    <-
    ifelse(df$aid_type.seasonal_items == 1, 1, 0)
  df$c38_vii   <- ifelse(df$aid_type.healthcare == 1, 1, 0)
  df$c38_viii  <- ifelse(df$aid_type.education == 1, 1, 0)
  df$c38_ix    <- ifelse(df$aid_type.other_nfi == 1, 1, 0)
  df$c38_x     <- ifelse(df$aid_type.protection == 1, 1, 0)
  
  ############################c39 ###############################################
  
  df$c39 <-     case_when(df$aid_satisfaction == "yes" ~ 1,
                          is.na(df$aid_satisfaction) ~ NA_real_ ,
                          TRUE ~ 0)
  
  
  ############################c40 ###############################################
  
  df$c40_i  <- case_when(
    df$aid_not_satisfied.quality == 1 ~ 1,
    (df$aid_received == "yes" &
       df$aid_satisfaction == "yes") |
      df$aid_not_satisfied.quality == 0 ~ 0,
    TRUE ~  NA_real_
  )
  df$c40_ii <- case_when(
    df$aid_not_satisfied.quantity == 1 ~ 1,
    (df$aid_received == "yes" &
       df$aid_satisfaction == "yes") |
      df$aid_not_satisfied.quantity == 0 ~ 0,
    TRUE ~  NA_real_
  )
  df$c40_iii <- case_when(
    df$aid_not_satisfied.delay == 1 ~ 1,
    (df$aid_received == "yes" &
       df$aid_satisfaction == "yes") |
      df$aid_not_satisfied.delay == 0 ~ 0,
    TRUE ~  NA_real_
  )

  df$c40_iv <- case_when(
    df$aid_not_satisfied.other == 1 ~ 1,
    (df$aid_received == "yes" &
       df$aid_satisfaction == "yes") |
      df$aid_not_satisfied.other == 0 ~ 0,
    TRUE ~  NA_real_
  )
  
  ############################c41 ###############################################
  
  df$c41 <- case_when(
    df$complaint_mechanisms == "yes" ~ 1,
    df$complaint_mechanisms == "no"  ~ 0,
    TRUE ~  NA_real_
  )
  
  ############################c42 ###############################################
  
  df$c42_i   <-
    case_when(
      df$restriction_clearance == "yes" &
        df$restriction_clearance_covid %in% c("yes", "similar") ~ 1,
      df$restriction_clearance == "no" |
        df$restriction_clearance_covid == "no" ~ 0,
      TRUE ~ NA_real_
    )
  df$c42_ii  <-
    case_when(
      df$restriction_documents == "yes" &
        df$restriction_documents_covid %in% c("yes", "similar") ~ 1,
      df$restriction_documents == "no" |
        df$restriction_documents_covid == "no" ~ 0,
      TRUE ~ NA_real_
    )
  df$c42_iii  <-
    case_when(
      df$restriction_time == "yes" &
        df$restriction_time_covid %in% c("yes", "similar") ~ 1,
      df$restriction_time == "no" |
        df$restriction_time_covid == "no" ~ 0,
      TRUE ~ NA_real_
    )
  df$c42_iv   <-
    case_when(
      df$restriction_reason == "yes" &
        df$restriction_reason_covid %in% c("yes", "similar") ~ 1,
      df$restriction_reason == "no" |
        df$restriction_reason_covid == "no" ~ 0,
      TRUE ~ NA_real_
    )
  df$c42_v  <-
    case_when(
      df$restriction_physical == "yes" &
        df$restriction_physical_covid %in% c("yes", "similar") ~ 1,
      df$restriction_physical == "no" |
        df$restriction_physical_covid == "no" ~ 0,
      TRUE ~ NA_real_
    )
  #df42_vi has been removed as you said it wasn't related to covid
  
  #please check - requires validation
  df$c42 <-
    case_when(
      df$c42_i == 1 |
        df$c42_ii == 1 |
        df$c42_iii == 1 |
        df$c42_iv == 1 |
        df$c42_v == 1 ~ 1,
      df$c42_i == 0 &
        df$c42_ii == 0 &
        df$c42_iii == 0 &
        df$c42_iv == 0 &
        df$c42_v == 0 ~ 0,
      TRUE ~ NA_real_
    )
  
  df$c42_2_i   <-
    case_when(
      df$restriction_clearance == "yes" &
        df$restriction_clearance_covid == "no" ~ 1,
      df$restriction_clearance == "no" |
        df$restriction_clearance_covid %in% c("yes", "similar") ~ 0,
      TRUE ~ NA_real_
    )

    df$c42_2_ii  <-
    case_when(
      df$restriction_documents == "yes" &
        df$restriction_documents_covid == "no" ~ 1,
      df$restriction_documents == "no" |
        df$restriction_documents_covid %in% c("yes", "similar") ~ 0,
      TRUE ~ NA_real_
    )
  df$c42_2_iii  <-
    case_when(
      df$restriction_time == "yes" &
        df$restriction_time_covid == "no" ~ 1,
      df$restriction_time == "no" |
        df$restriction_time_covid %in% c("yes", "similar") ~ 0,
      TRUE ~ NA_real_
    )
  df$c42_2_iv   <-
    case_when(
      df$restriction_reason == "yes" &
        df$restriction_reason_covid == "no" ~ 1,
      df$restriction_reason == "no" |
        df$restriction_reason_covid %in% c("yes", "similar") ~ 0,
      TRUE ~ NA_real_
    )
  df$c42_2_v  <-
    case_when(
      df$restriction_physical == "yes" &
        df$restriction_physical_covid == "no" ~ 1,
      df$restriction_physical == "no" |
        df$restriction_physical_covid %in% c("yes", "similar") ~ 0,
      TRUE ~ NA_real_
    )
  df$c42_2_vi <-
    case_when(df$restriction_other == "yes" ~ 1,
              df$restriction_other == "no" ~ 0,
              TRUE ~ NA_real_)
  df$c42_2 <-
    case_when(
      df$c42_2_i == 1 |
        df$c42_2_ii == 1 |
        df$c42_2_iii == 1 |
        df$c42_2_iv == 1 |
        df$c42_2_v == 1 |
        df$c42_2_vi == 1 ~ 1,
      df$c42_2_i == 0 &
        df$c42_2_ii == 0 &
        df$c42_2_iii == 0 &
        df$c42_2_iv == 0 &
        df$c42_2_v == 0 &
        df$c42_2_vi == 0 ~ 0,
      TRUE ~ NA_real_
    )
  ############################c43 #############################################
  
  df$c43_i <- df$primary_livelihood.savings
  df$c43_ii <- df$primary_livelihood.renting
  df$c43_iii <- df$primary_livelihood_employment
  df$c43_iv <- df$primary_livelihood.remittences
  df$c43_v <- df$primary_livelihood_retirement_pension
  df$c43_vi <- df$primary_livelihood.selling_assets
  df$c43_vii <- df$primary_livelihood.selling_assistance_received
  df$c43_viii <- df$primary_livelihood.loans_debts
  df$c43_ix <- df$primary_livelihood.modm_cash_assistance
  df$c43_x <- df$primary_livelihood.support_from_community
  df$c43_xi <- df$primary_livelihood.ngo_charity_assistance
  df$c43_xii <- df$primary_livelihood.social_service
  df$c43_xiii <- df$primary_livelihood.illegal_activity
  df$c43_xiv <- df$primary_livelihood.zakat
  
  df$c43_2_i <-
    case_when(
      df$primary_livelihood.savings == 1 & df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.savings == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_ii  <-
    case_when(
      df$primary_livelihood.renting == 1 & df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.renting == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_iii  <-
    case_when(
      df$primary_livelihood_employment == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood_employment == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_iv  <-
    case_when(
      df$primary_livelihood.remittences == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.remittences == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_v  <-
    case_when(
      df$primary_livelihood_retirement_pension == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood_retirement_pension == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_vi  <-
    case_when(
      df$primary_livelihood.selling_assets == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.selling_assets == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_vii  <-
    case_when(
      df$primary_livelihood.selling_assistance_received == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.selling_assistance_received == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_viii  <-
    case_when(
      df$primary_livelihood.loans_debts == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.loans_debts == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_ix  <-
    case_when(
      df$primary_livelihood.modm_cash_assistance == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.modm_cash_assistance == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_x  <-
    case_when(
      df$primary_livelihood.support_from_community == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.support_from_community == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_xi  <-
    case_when(
      df$primary_livelihood.ngo_charity_assistance == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.ngo_charity_assistance == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_xii  <-
    case_when(
      df$primary_livelihood.social_service == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.social_service == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_xiii  <-
    case_when(
      df$primary_livelihood.illegal_activity == 1 &
        df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.illegal_activity == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  df$c43_2_xiv  <-
    case_when(
      df$primary_livelihood.zakat == 1 & df$single_female_hhh == 1 ~ 1,
      df$primary_livelihood.zakat == 0 &
        df$single_female_hhh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  
  ##############################c44 ###########################################
  df$c44_i <-
    case_when(
      df$distance_hospital == "less_15" |
        df$distance_clinic == "less_15" ~ 1,
      is.na(df$distance_hospital) &
        is.na(df$distance_clinic) ~ NA_real_,
      TRUE ~ 0
    )
  
  df$c44_ii <-
    case_when(
      (
        df$distance_hospital == "less_30" |
          df$distance_clinic == "less_30"
      ) &
        !(
          df$distance_hospital == "less_15" |
            df$distance_clinic == "less_15"
        )  ~ 1,
      is.na(df$distance_hospital) &
        is.na(df$distance_clinic) ~ NA_real_,
      TRUE ~ 0
    )
  
  df$c44_iii <-
    case_when(
      (
        df$distance_hospital == "less_hour" |
          df$distance_clinic == "less_hour"
      ) &
        !(
          df$distance_hospital %in% c("less_15", "less_30") |
            df$distance_clinic %in% c("less_15", "less_30")
        )  ~ 1,
      is.na(df$distance_hospital) &
        is.na(df$distance_clinic) ~ NA_real_,
      TRUE ~ 0
    )
  
  df$c44_iv <-
    case_when(
      (
        df$distance_hospital == "less_3hours" |
          df$distance_clinic == "less_3hours"
      ) &
        !(
          df$distance_hospital %in% c("less_15", "less_30", "less_hour") |
            df$distance_clinic %in% c("less_15", "less_30", "less_hour")
        )  ~ 1,
      is.na(df$distance_hospital) &
        is.na(df$distance_clinic) ~ NA_real_,
      TRUE ~ 0
    )
  df$c44_v <-
    case_when(
      (
        df$distance_hospital == "more_3hours" |
          df$distance_clinic == "more_3hours"
      ) &
        !(
          df$distance_hospital %in% c("less_15", "less_30", "less_hour", "less_3hours") |
            df$distance_clinic %in% c("less_15", "less_30", "less_hour", "less_3hours")
        )  ~ 1,
      is.na(df$distance_hospital) &
        is.na(df$distance_clinic) ~ NA_real_,
      TRUE ~ 0
    )
  
  df$c45 <- df$info_aid.healthcare
  
  df$c46_i <-
    case_when(
      df$distance_hospital == "less_15" ~ 1,
      df$distance_hospital %in% c("less_30", "less_hour", "less_3hours", "more_3hours") ~ 0,
      TRUE ~ NA_real_
    )
  
  df$c46_ii <-
    case_when(
      df$distance_hospital == "less_30" ~ 1,
      df$distance_hospital %in% c("less_15", "less_hour", "less_3hours", "more_3hours") ~ 0,
      TRUE ~ NA_real_
    )
  
  df$c46_iii <-
    case_when(
      df$distance_hospital == "less_hour" ~ 1,
      df$distance_hospital %in% c("less_30", "less_15", "less_3hours", "more_3hours") ~ 0,
      TRUE ~ NA_real_
    )
  
  df$c46_iv <-
    case_when(
      df$distance_hospital == c("less_3hours") ~ 1,
      df$distance_hospital %in% c("less_30", "less_15", "less_hour", "more_3hours") ~ 0,
      TRUE ~ NA_real_
    )
  
  df$c46_v <-
    case_when(
      df$distance_hospital == c("more_3hours") ~ 1,
      df$distance_hospital %in% c("less_30", "less_15", "less_3hours", "less_hour") ~ 0,
      TRUE ~ NA_real_
    )
  
  return(df)
}