question <-
  read.csv(
    "input/2018/kobo/kobo_questions.csv",
    stringsAsFactors = F
  )
choice <-
  read.csv(
    "input/2018/kobo/kobo_choices.csv",
    stringsAsFactors = F
  )

data <-
  read.csv(
    "input/2018/dataset/2018_household level_dataset.csv",
    stringsAsFactors = F,
    na.strings = c("", "na", "NA", "NaN", "#N/A")
  )

loop <-
  read.csv(
    "input/2018/dataset/2018_individual level_dataset.csv",
    stringsAsFactors = F,
    na.strings = c("", "na", "NA", "NaN", "#N/A")
  )

dap <-
  read.csv(
    sprintf("input/2018/dap/dap_%s.csv", dap_name),
    stringsAsFactors = F
  )
dap$repeat.for.variable <- aggregate
dap$independent.variable <- disaggregate
lookup_table <-
  read.csv(
    "input/2018/lookup_tables/lookup_table_names.csv",
    stringsAsFactors = F
  )
sampling_frame <-
  read.csv(
    "input/2018/sampling_frame/sampling_frame.csv",
    stringsAsFactors = F
  )