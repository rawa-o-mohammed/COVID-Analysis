source("functions/2020/postprocessing_functions.R")
source("functions/2020/to_alphanumeric_lowercase.R")
source("functions/2020/analysisplan_factory.R")
source("functions/2020/covid_recoding_2020.R")
source("functions/2020/Binary_Recoding.R")
source("functions/2020/helper_functions.R")

#LOAD INPUT FILES
source("functions/2020/1_load_inputs.R", local = T)
names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"
response$district <- response$district_mcna
response$all <- "all"
#' creates objects:
#'
#'    response representative clean
#'    response indicative clean
#'    analysisplan
#'    choices
#'    questions
#'    cluster_lookup_table
#'    loop
#'    samplingframe
#'    samplingframe_in_camp


#PREPARE SAMPLING FRAMES AND STRATAS
source("functions/2020/2_prepare_samplingframe.R", local = T)
#' Prepare sampling frames and Strata names:
#'     3.1 prepare columns in out of camp cluster level sampling frame
#'     3.2 aggregate out-of-camp to stratum level
#'     3.3.make strata id for in-camp sampling frame
#'     3.4.combine the stratum sampling frames
#'     3.5.add strata ids to the dataset
#'     3.6. throw error if any don't match

#IDENTIFY ANY FURTHER PROBLEMS WITH THE SAMPLING FRAMES MATCHING
strata_samplingframe_issues <-
  as.data.frame(response[which(!response$strata %in% samplingframe_strata$stratum), c("X_uuid", "strata")])
if (nrow(strata_samplingframe_issues) != 0) {
  print(strata_samplingframe_issues)
  warning("something's not right with the strata id matching!")
}


#STRATA WEIGHTING
strata_weight_fun <-
  map_to_weighting(
    sampling.frame = samplingframe_strata,
    sampling.frame.population.column = "population",
    sampling.frame.stratum.column = "stratum",
    data.stratum.column = "strata",
    data = response
  )

weight_fun <- strata_weight_fun

response$weights <- weight_fun(response)

#CREATE NEW FUNCTION FOR WEIGHTING
weight_fun <- function(df) {
  df$weights
}

#RECODING OF INDICATORS
response_with_composites <- covid_recoding_2020(response, loop)


#LOAD ANALYSISPLAN
analysisplan <-
  read.csv(sprintf("input/2020/dap/dap_%s.csv", dap_name), stringsAsFactors = F, na.strings = c("", "na", "NA", "NaN", "#N/A"))
analysisplan$repeat.for.variable <- aggregate
analysisplan$independent.variable <- disaggregate

result <-
  from_analysisplan_map_to_output(
    data = response_with_composites,
    analysisplan = analysisplan,
    weighting = weight_fun,
    questionnaire = questionnaire,
    confidence_level = 0.9
  )

lookup_in_camp <-
  load_samplingframe("input/2020/sampling_frame/sampling_frame_in_camp.csv")
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp")] <-
  "name"
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp.long.name")] <-
  "english"
names(lookup_in_camp)[which(names(lookup_in_camp) == "governorate")] <-
  "filter"

summary <-
  bind_rows(lapply(result[[1]], function(x) {
    x$summary.statistic
  }))
write.csv(summary,
          sprintf("output/2020/raw_results/raw_results_%s.csv", name),
          row.names = F)
summary <-
  read.csv(sprintf("output/2020/raw_results/raw_results_%s.csv", name),
           stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA, 1))
write.csv(
  summary,
  sprintf("output/2020/raw_results/raw_results_%s_filtered.csv", name),
  row.names = F
)
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
library(plyr)
for (i in 1:length(groups)) {
  df <-
    pretty.output(
      summary,
      groups[i],
      analysisplan,
      cluster_lookup_table,
      lookup_table,
      severity = name == "severity",
      camp = F
    )
  if (i == 1) {
    xlsx::write.xlsx(
      df,
      file = sprintf("output/2020/summary_sorted/summary_sorted_%s.xlsx", name),
      sheetName = groups[i],
      row.names = FALSE
    )
  } else {
    xlsx::write.xlsx(
      df,
      file = sprintf("output/2020/summary_sorted/summary_sorted_%s.xlsx", name),
      sheetName = groups[i],
      append = TRUE,
      row.names = FALSE
    )
  }
}
