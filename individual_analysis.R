rm(list = ls(all = T))
library(xlsx)
library(plyr)
library(dplyr)
library(koboquest) # manage kobo questionnaires
library(kobostandards) # check inputs for inconsistencies
library(hypegrammaR)
library(surveyweights)
library(expss)
library(composr) # horziontal operations
library(readr)
library(readxl)

source("functions/2020/postprocessing_functions.R")

#LOAD INPUT FILES
source("functions/2020/1_load_inputs.R", local = T)
names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"

name <- "indiv_analysis"

idp <- read.csv("input/2020/individual_analysis_files/idp.csv")
returnee_dataset <-
  read.csv("input/2020/individual_analysis_files/returnee.csv")
conversion <-
  read.csv("input/2020/individual_analysis_files/conversion_name.csv",
           encoding = "UTF-8")
camp <-
  read.csv(
    "input/2020/individual_analysis_files/Camp_Master_List_and_Population_Flow_September_2020.csv",
    encoding = "UTF-8"
  )

#ADJUST WRONG NAMES INCAMP
response$district_mcna <-
  ifelse(response$district_mcna == "al.hinidya",
         "al.hindiya",
         response$district_mcna)
response$district_mcna <-
  ifelse(response$district_mcna == "falluja",
         "al.falluja",
         response$district_mcna)

loop$district <-
  response$district_mcna[match(loop$X_uuid, response$X_uuid)]
loop$population_group <-
  response$population_group[match(loop$X_uuid, response$X_uuid)]
loop$all <- "all"

## matching the districts of the MCNA and the CCCM
## and then calculating the total individuals who live in those camps that are assessed
idp_in_camp_pop <- camp %>%
  mutate(district = conversion$X.U.FEFF.district_mcna[match(camp$District, conversion$admin2Name_en)])
idp_in_camp_pop <- idp_in_camp_pop %>%
  group_by(district) %>%
  summarize(population = sum(Total.no.of.individuals, na.rm = TRUE))

idp_in_camp_pop <-
  idp_in_camp_pop %>% filter(!is.na(district), population != 0, district %in% loop$district[which(loop$population_group == "idp_in_camp")])
## calculating the total of individuals who live out of camps
## and matching the district names between DTM and MCNA
idp_out_camp_pop <- idp %>%
  mutate(
    individual_out_camp = sum_row(
      Host.families,
      Hotel.Motel.or.short.term.rental,
      Own.Property,
      Rental..Habitable.,
      Rental..Uninhabitable.,
      Informal.settlements,
      Unfinished.Abandoned.building,
      Non.residential.structure,
      School.building,
      Religious.building,
      Other.formal.settlements..collective.centres,
      Other.shelter,
      Unknown.shelter.type
    ),
    district = conversion$X.U.FEFF.district_mcna[match(idp$District, conversion$district_iom)]
  )

idp_out_camp_pop <- idp_out_camp_pop %>%
  group_by(district) %>%
  summarize(population = sum(individual_out_camp, na.rm = TRUE))
idp_out_camp_pop <-
  idp_out_camp_pop %>% filter(!is.na(district), population != 0, district %in% loop$district[which(loop$population_group == "idp_out_camp")])


returnee_pop <- returnee_dataset %>%
  mutate(district = conversion$X.U.FEFF.district_mcna[match(returnee_dataset$District, conversion$district_iom)])

returnee_pop <- returnee_pop %>%
  group_by(district) %>%
  summarize(population = sum(Individuals, na.rm = TRUE))

returnee_pop <-
  returnee_pop %>% filter(!is.na(district), population != 0, district %in% loop$district[which(loop$population_group == "returnee")])

#preparing the sampling frames
loop <- loop %>%
  mutate(strata = paste0(district, population_group))

idp_in_camp_pop$stratum <-
  paste0(idp_in_camp_pop$district, "idp_in_camp")
idp_out_camp_pop$stratum <-
  paste0(idp_out_camp_pop$district, "idp_out_camp")
returnee_pop$stratum <- paste0(returnee_pop$district, "returnee")

ind_sampling_frame <-
  rbind(idp_in_camp_pop, idp_out_camp_pop, returnee_pop)

# MERGE QUESTIONNAIRES
questionnaire <-
  load_questionnaire(loop , questions, choices, choices.label.column.to.use = "name")

#recoding indicators
loop <- loop %>%
  mutate(
    above64 = case_when(age >= 64 ~ 1, TRUE ~ 0),
    above64_with_chronic = case_when(age >= 64 &
                                       health_issue.chronic == 1 ~ 1, TRUE ~ 0),
    above64_or_chronic = case_when(age >= 64 |
                                     health_issue.chronic == 1 ~ 1, TRUE ~ 0),
    single_famale_hhh = case_when(
      marital_status %in%
        c("single", "separated", "widowed", "divorced") &
        sex == "female" & relationship == "head" ~ 1,
      marital_status == "married" | relationship != "head" ~ 0,
      TRUE ~ NA_real_
    )
  )

#STRATA WEIGHTING
strata_weight_fun <-
  map_to_weighting(
    sampling.frame = ind_sampling_frame,
    sampling.frame.population.column = "population",
    sampling.frame.stratum.column = "stratum",
    data.stratum.column = "strata",
    data = loop
  )

weight_fun <- strata_weight_fun

loop$weights <- weight_fun(loop)

#LOAD ANALYSISPLAN
analysisplan <-
  read.csv(
    "input/2020/dap/dap_ind_covid_analysis_2020.csv",
    stringsAsFactors = F,
    na.strings = c("", "na", "NA", "NaN", "#N/A")
  )
analysisplan$repeat.for.variable <- "district"
analysisplan$independent.variable <- "all"

# Mapping to output
result <-
  from_analysisplan_map_to_output(
    data = loop,
    analysisplan = analysisplan,
    weighting = weight_fun,
    questionnaire = questionnaire,
    confidence_level = 0.9
  )


summary <-
  bind_rows(lapply(result[[1]], function(x) {
    x$summary.statistic
  }))
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA, 1))

#getting the total population for each population group
tot_in_camp <- idp_in_camp_pop %>%
  summarise(sum(population))
tot_out_camp <- idp_out_camp_pop %>%
  summarise(sum(population))
tot_returnee <- returnee_pop %>%
  summarise(sum(population))
tot <-
  data.frame(
    idp_in_camp = tot_in_camp[[1]],
    idp_out_camp = tot_out_camp[[1]],
    returnee = tot_returnee[[1]]
  )
tot$total <- sum_row(tot)

#calculating the population for each population in each district
pop_numbers <-
  full_join(idp_in_camp_pop, idp_out_camp_pop, by = "district")
pop_numbers <- full_join(pop_numbers, returnee_pop, by = "district")
pop_numbers <- rename(
  pop_numbers,
  idp_in_camp = "population.x",
  idp_out_camp = "population.y",
  returnee = "population"
)
pop_numbers <-
  pop_numbers[, c("district", "idp_in_camp", "idp_out_camp", "returnee")]
pop_numbers$total <- sum_row(pop_numbers[, 2:4])


groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]

#arranging the output
if (analysisplan$repeat.for.variable == "district" &
    analysisplan$independent.variable == "population_group") {
  for (i in 1:length(groups)) {
    subset <-
      summary[which(summary$independent.var.value == groups[i]), ]
    df <- data.frame(district = unique(ind_sampling_frame$district))
    vars <- unique(subset$dependent.var)
    districts <- unique(subset$repeat.var.value)
    n_pop <-
      pop_numbers[, c("district", paste0(groups[i]), "total")]
    df <- left_join(df, n_pop, by = "district")
    df <- df[complete.cases(df), ]
    for (j in 1:length(vars)) {
      var_result <- subset[which(subset$dependent.var == vars[j]),]
      df[, vars[j]] <-
        var_result[match(df$district, var_result$repeat.var.value), "numbers"]
      df[, sprintf("Number_of_%s", vars[j])] <-
        as.numeric(df[, vars[j]]) * as.numeric(df[, paste0(groups[i])])
    }
    df <- df[ncol(df) - rowSums(is.na(df)) > 3,]
    df <-
      left_join(df, conversion, by = c("district" = "X.U.FEFF.district_mcna"))
    write_excel_csv(df,
                    sprintf("output/2020/individual/district_%s.csv", groups[i]))
  }
} else if (analysisplan$repeat.for.variable == "all" &
           analysisplan$independent.variable == "population_group") {
  for (i in 1:length(groups)) {
    subset <-
      summary[which(summary$independent.var.value == groups[i]), ]
    vars <- unique(subset$dependent.var)
    df <-
      tot[, c(paste0(groups[i]), "total")]
    df <- df[complete.cases(df), ]
    for (j in 1:length(vars)) {
      var_result <- subset[which(subset$dependent.var == vars[j]),]
      df[, vars[j]] <-
        var_result[, "numbers"]
      df[, sprintf("Number_of_%s", vars[j])] <-
        as.numeric(df[, vars[j]]) * as.numeric(df[, paste0(groups[i])])
    }
    write_excel_csv(df,
                    sprintf("output/2020/individual/all_%s.csv", groups[i]))
  }
} else if (analysisplan$repeat.for.variable == "district" &
           analysisplan$independent.variable == "all") {
  df <- data.frame(district = unique(ind_sampling_frame$district))
  vars <- unique(summary$dependent.var)
  districts <- unique(summary$repeat.var.value)
  n_pop <- pop_numbers[, c("district", "total")]
  df <- left_join(df, n_pop, by = "district")
  df <- df[complete.cases(df), ]
  for (j in 1:length(vars)) {
    var_result <- summary[which(summary$dependent.var == vars[j]),]
    df[, vars[j]] <-
      var_result[match(df$district, var_result$repeat.var.value), "numbers"]
    df[, sprintf("Number_of_%s", vars[j])] <-
      as.numeric(df[, vars[j]]) * as.numeric(df[, "total"])
  }
  df <- df[ncol(df) - rowSums(is.na(df)) > 3,]
  df <-
    left_join(df, conversion, by = c("district" = "X.U.FEFF.district_mcna"))
  write_excel_csv(df, "output/2020/individual/district_all.csv")
} else if (analysisplan$repeat.for.variable == "all" &
           analysisplan$independent.variable == "all") {
  vars <- unique(summary$dependent.var)
  df <- tot
  for (j in 1:length(vars)) {
    var_result <- summary[which(summary$dependent.var == vars[j]),]
    df[, vars[j]] <- var_result[, "numbers"]
    df[, sprintf("Number_of_%s", vars[j])] <-
      as.numeric(df[, vars[j]]) * as.numeric(df[, "total"])
  }
  write_excel_csv(df, "output/2020/individual/all_all.csv")
}