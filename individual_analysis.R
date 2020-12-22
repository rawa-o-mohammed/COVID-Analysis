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
idp_in_camp_pop$population_group <- "idp_in_camp"

idp_out_camp_pop$stratum <-
  paste0(idp_out_camp_pop$district, "idp_out_camp")
idp_out_camp_pop$population_group <- "idp_out_camp"

returnee_pop$stratum <- paste0(returnee_pop$district, "returnee")
returnee_pop$population_group <- "returnee"

ind_sampling_frame <-
  rbind(idp_in_camp_pop, idp_out_camp_pop, returnee_pop)
ind_sampling_frame$all <- "all"

##############################################################################

#recoding indicators
loop <- loop %>%
  mutate(
    above64 = case_when(age >= 64 ~ 1, TRUE ~ 0),
    above64_or_chronic = case_when(age >= 64 |
                                     health_issue.chronic == 1 ~ 1, TRUE ~ 0)
  )

loop$access_healthcare_hour <- ifelse(
  response$distance_hospital[match(loop$X_uuid, response$X_uuid)] %in% c("less_15", "less_30", "less_hour") |
    response$distance_clinic[match(loop$X_uuid, response$X_uuid)] %in% c("less_15", "less_30", "less_hour"),
  1,
  0
)
loop$access_healthcare_services_hour <- ifelse(
  response$distance_hospital[match(loop$X_uuid, response$X_uuid)] %in% c("less_15", "less_30", "less_hour") &
    response$hospital_emergency_ser[match(loop$X_uuid, response$X_uuid)] == "yes" &
    response$hospital_maternity_ser[match(loop$X_uuid, response$X_uuid)] == "yes" &
    response$hospital_pediatric_ser[match(loop$X_uuid, response$X_uuid)] == "yes" &
    response$hospital_surgical_ser[match(loop$X_uuid, response$X_uuid)] == "yes" ,
  1,
  0
)

lowest_stratification <- loop %>%
  group_by(strata) %>%
  summarize(
    n_above64 = sum(above64, na.rm = TRUE),
    n_chronic_disease = sum(health_issue.chronic, na.rm = TRUE),
    n_above64_or_chronic = sum(above64_or_chronic, na.rm = TRUE),
    n_access_healthcare_hour = sum(access_healthcare_hour, na.rm = TRUE),
    n_access_healthcare_services_hour = sum(access_healthcare_services_hour, na.rm = TRUE),
    population = n()
  )

# lowest_stratification <- full_join(lowest_stratification, ind_sampling_frame, by = c("strata" = "stratum"))
lowest_stratification$district <- lowest_stratification$strata
lowest_stratification$district = gsub("idp_in_camp", "", lowest_stratification$district)
lowest_stratification$district = gsub("idp_out_camp", "", lowest_stratification$district)
lowest_stratification$district = gsub("returnee", "", lowest_stratification$district)

lowest_stratification$population_group <-
  lowest_stratification$strata
lowest_stratification[grep("idp_in_camp", lowest_stratification$population_group), "population_group"] <-
  "idp_in_camp"
lowest_stratification[grep("idp_out_camp", lowest_stratification$population_group), "population_group"] <-
  "idp_out_camp"
lowest_stratification[grep("returnee", lowest_stratification$population_group), "population_group"] <-
  "returnee"
lowest_stratification$all <- "all"

ind_sampling_frame_short <- ind_sampling_frame %>%
  select(population, stratum) %>%
  rename(total_population_strata = population)

lowest_stratification <- lowest_stratification %>%
  rename(total_population_data = population) %>%
  left_join(ind_sampling_frame_short, by = c("strata" = "stratum"))  %>%
  mutate(
    n_above64_strata = round((n_above64 / total_population_data) * total_population_strata),
    n_chronic_disease_strata = round((n_chronic_disease / total_population_data) * total_population_strata
    ),
    n_above64_or_chronic_strata = round((n_above64_or_chronic / total_population_data) * total_population_strata
    ),
    n_access_healthcare_hour_strata = round((n_access_healthcare_hour / total_population_data) * total_population_strata
    ),
    n_access_healthcare_services_hour_strata = round((n_access_healthcare_services_hour / total_population_data) * total_population_strata
    )
  )

district <- lowest_stratification %>%
  group_by(district) %>%
  summarise(
    num_above64 = sum(n_above64_strata),
    num_chronic_disease = sum(n_chronic_disease_strata),
    num_above64_or_chronic = sum(n_above64_or_chronic_strata),
    num_access_healthcare_hour = sum(n_access_healthcare_hour_strata),
    num_access_healthcare_services_hour = sum(n_access_healthcare_services_hour_strata),
    total_population = sum(total_population_strata)
  ) %>%
  mutate(
    avg_above64 = num_above64 / total_population,
    avg_chronic_disease = num_chronic_disease / total_population,
    avg_above64_or_chronic = num_above64_or_chronic / total_population,
    avg_access_healthcare_hour = num_access_healthcare_hour / total_population,
    avg_access_healthcare_services_hour = num_access_healthcare_services_hour /
      total_population
  )

popgroup <- lowest_stratification %>%
  group_by(population_group) %>%
  summarise(
    num_above64 = sum(n_above64_strata),
    num_chronic_disease = sum(n_chronic_disease_strata),
    num_above64_or_chronic = sum(n_above64_or_chronic_strata),
    num_access_healthcare_hour = sum(n_access_healthcare_hour_strata),
    num_access_healthcare_services_hour = sum(n_access_healthcare_services_hour_strata),
    total_population = sum(total_population_strata)
  ) %>%
  mutate(
    avg_above64 = num_above64 / total_population,
    avg_chronic_disease = num_chronic_disease / total_population,
    avg_above64_or_chronic = num_above64_or_chronic / total_population,
    avg_access_healthcare_hour = num_access_healthcare_hour / total_population,
    avg_access_healthcare_services_hour = num_access_healthcare_services_hour /
      total_population
  )

all <- lowest_stratification %>%
  group_by(all) %>%
  summarise(
    num_above64 = sum(n_above64_strata),
    num_chronic_disease = sum(n_chronic_disease_strata),
    num_above64_or_chronic = sum(n_above64_or_chronic_strata),
    num_access_healthcare_hour = sum(n_access_healthcare_hour_strata),
    num_access_healthcare_services_hour = sum(n_access_healthcare_services_hour_strata),
    total_population = sum(total_population_strata)
  ) %>%
  mutate(
    avg_above64 = num_above64 / total_population,
    avg_chronic_disease = num_chronic_disease / total_population,
    avg_above64_or_chronic = num_above64_or_chronic / total_population,
    avg_access_healthcare_hour = num_access_healthcare_hour / total_population,
    avg_access_healthcare_services_hour = num_access_healthcare_services_hour /
      total_population
  )

all <- rename(all, strata = "all")
popgroup <- rename(popgroup, strata = "population_group")
district <- rename(district, strata = "district")

result <- rbind(district, popgroup, all)


result <- result[, c(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12)]

result <-
  left_join(result, conversion, by = c("strata" = "X.U.FEFF.district_mcna"))

write_excel_csv(result, "output/2020/individual/individual_findings.csv")
