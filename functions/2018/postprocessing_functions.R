round2 <- function(x, n=0) {
  posneg <- sign(x)
  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z <- z*posneg
  return(z)
}
pretty.output <-
  function(summary,
           independent.var.value,
           analysisplan,
           lookup_table,
           district_gov) {
    subset <-
      summary[which(summary$independent.var.value == independent.var.value),]
    
    independent.var <- subset$independent.var[1]
    if (is.na(independent.var)) {
      analplan_subset <- analysisplan
    } else {
      analplan_subset <-
        analysisplan[which(analysisplan$independent.variable == independent.var), ]
    }
    vars <- unique(subset$dependent.var)
    vars <- unique(subset$dependent.var)
    districts <- unique(subset$repeat.var.value)
    camp <- FALSE
    start <- ifelse(camp, 1, 19)
    df <- data.frame(governorate = lookup_table$filter[start:nrow(lookup_table)][match(districts, lookup_table$name[start:nrow(lookup_table)])],  
                     district = districts, stringsAsFactors = F)
    df <- df[with(df, order(governorate, district)),]
    for (i in 1:length(vars)) {
      var_result <- subset[which(subset$dependent.var == vars[i]),]
      df[, vars[i]] <-
        var_result[match(df$district, var_result$repeat.var.value), "numbers"]
      df[, sprintf("%s_min", vars[i])] <-
        var_result[match(df$district, var_result$repeat.var.value), "min"]
      df[, sprintf("%s_max", vars[i])] <-
        var_result[match(df$district, var_result$repeat.var.value), "max"]
    }
    extra_heading <- data.frame(t(vars), stringsAsFactors = F)
    colnames(extra_heading) <- vars
    extra_heading[1,] <-
      t(analplan_subset$Indicator.Group...Sector[match(vars, analplan_subset$dependent.variable)])
    extra_heading[2,] <-
      t(analplan_subset$research.question[match(vars, analplan_subset$dependent.variable)])
    extra_heading[3,] <-
      t(analplan_subset$sub.research.question[match(vars, analplan_subset$dependent.variable)])
    extra_heading[4,] <-
      t(analplan_subset$dependent.variable.type[match(vars, analplan_subset$dependent.variable)])
    
    df <- plyr::rbind.fill(df, extra_heading)
    df <-
      df[c((nrow(df) - (nrow(extra_heading) - 1)):nrow(df), 1:(nrow(df) - nrow(extra_heading))),]
    
    df[1:nrow(extra_heading), which(is.na(df[1,]))] <- ""
    df
  }

correct.zeroes <- function(summary) {
  zeroes <-
    which(summary$dependent.var.value == 0 & summary$numbers == 1)
  summary$dependent.var.value[zeroes] <- 1
  summary$numbers[zeroes] <- 0
  summary$min[zeroes] <- 0
  summary$max[zeroes] <- 0
  return(summary)
}
