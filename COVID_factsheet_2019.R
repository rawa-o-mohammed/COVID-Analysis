# setup

source("functions/2019/postprocessing_functions.R")
source("functions/2019/to_alphanumeric_lowercase.R")
source("functions/2019/analysisplan_factory.R")
source("functions/2019/covid_recoding_2019.R")


#' load input files & make everything match:
source("functions/2019/load_inputs.R", local = T)
#' creates objects:
#'
#'    response
#'    analysisplan
#'    choices
#'    questions
#'    cluster_lookup_table
#'    idp_in_camp
#'    loop
#'    loop_in_camp
#'    samplingframe
#'    samplingframe_in_camp

# any further problems with the sampling frame matching?
response$all <- "all"
strata_samplingframe_issues <-
  as.data.frame(response[which(!response$strata %in% samplingframe_strata$stratum), c("X_uuid", "strata")])
if (nrow(strata_samplingframe_issues) != 0) {
  print(strata_samplingframe_issues)
  warning("something's not right with the strata id matching!")
}

strata_weight_fun <-
  map_to_weighting(
    sampling.frame = samplingframe_strata,
    sampling.frame.population.column = "population",
    sampling.frame.stratum.column = "stratum",
    data.stratum.column = "strata",
    data = response
  )

# weight_fun <- combine_weighting_functions(strata_weight_fun, clusters_weight_fun)
weight_fun <- strata_weight_fun

response$weights <- weight_fun(response)

# new version of weights for SDC analysis
if (F) {
  library(purrr)
  samp_amount <- response$strata %>% table
  samplingframe_strata$samp_amount[match(names(samp_amount), samplingframe_strata$stratum)] <-
    samp_amount
  samplingframe_strata$weights2 <-
    samplingframe_strata$population / samplingframe_strata$samp_amount
  response <-
    response %>% mutate(weights2 = samplingframe_strata$weights2[match(strata, samplingframe_strata$stratum)])
  
}


response_with_composites <- covid_recoding_2019(response, loop)

analysisplan <-
  read.csv(sprintf("input/2019/dap/dap_%s.csv", dap_name), stringsAsFactors = F)
analysisplan$repeat.for.variable <- aggregate
analysisplan$independent.variable <- disaggregate

questionnaire <- load_questionnaire(response,questions,choices, choices.label.column.to.use = "name")

result <-
  from_analysisplan_map_to_output(
    data = response_with_composites,
    analysisplan = analysisplan,
    weighting = weight_fun,
    questionnaire = questionnaire,
    confidence_level = 0.9
  )

saveRDS(result, paste(sprintf("output/2019/RDS/result_%s.RDS", name)))

summary <-
  bind_rows(lapply(result[[1]], function(x) {
    x$summary.statistic
  }))
write.csv(summary,
          sprintf("output/2019/raw_results/raw_results_%s.csv", name),
          row.names = F)
summary <-
  read.csv(sprintf("output/2019/raw_results/raw_results_%s.csv", name),
           stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA, 1))
write.csv(summary,
          sprintf("output/2019/raw_results/raw_results_%s_filtered.csv", name),
          row.names = F)

groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]

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
    write.xlsx(
      df,
      file = sprintf("output/2019/summary_sorted/summary_sorted_%s.xlsx", name),
      sheetName = groups[i],
      row.names = FALSE
    )
  } else {
    write.xlsx(
      df,
      file = sprintf("output/2019/summary_sorted/summary_sorted_%s.xlsx", name),
      sheetName = groups[i],
      append = TRUE,
      row.names = FALSE
    )
  }
}
