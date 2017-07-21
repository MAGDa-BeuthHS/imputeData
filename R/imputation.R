
#' organizes the whole imputation-process
#'
#' gets the incomplete dataset, imputes it according to strategy-choices and returns complete dataset
#'
#' @param data the data.table incl. variables with missing data
#' @param vars vector of names of variables with missing data
#' @param choices list of strategies with limits and parameters
#'
#' @return data.table with sensor-id, timestamp and imputed variables
#'
#' @export
impute = function(data, vars = c("b", "g"), choices = get_std_choices()) {
  sensors = unique(data[, sensor])
  # impute every sensor and variable separately
  for (s in sensors) {
    print(paste("imputations for sensor", s))
    for (var in vars) {
      data[sensor == s, var] = impute_combined(data = data[sensor == s], var = var, choices = choices[[var]])
    }
  }
  data = data[, c("sensor", "z", vars), with = F]
  return(data)
}

#' imputes the missing data of one variable
#'
#' imputes the missing data of one variable
#'
#' @param data the data.table incl. variables with missing data
#' @param var name of variable with missing data
#' @param choices list of strategies with limits and parameters for this variable
#'
#' @return vector with complete data of variable
#'
#' @export
impute_combined = function(data, var, choices)
{
  # column as vector
  col = data[[which(colnames(data) == var)]]
  # per function in choices
  for(a in 1:length(choices)){
    limit = choices[[a]]$limit   # gapsize-limits
    fun = choices[[a]]$fun       # imputation-function
    name = choices[[a]]$name
    para = list("var" = var, "data" = data)
    if (length(choices[[a]]) > 2) {
      para = append(para, choices[[a]][3:length(choices[[a]])])
    }

    # gaps to be imputed
    gaps = get_gaps(col) # returns df with starts and sizes
    gaps = gaps[which(gaps$gap_size <= limit),]
    print(paste(length(gaps$gap_start), "gaps in variable", var, "with max. length", limit))
    id_na = gaps$gap_start        # all gap.starts
    sizes = gaps$gap_size         # all gap.sizes
    id_na = unlist(mapply(FUN = seq.int, id_na, length.out = sizes, SIMPLIFY = T)) # all IDs of NA-values

    # impute
    new_values = impute_var(FUN = fun, para = para)
    col[id_na] = new_values[id_na]
    print(paste("--> successfully imputed with", name))
  }
  return(col)
}

#' applies the chosen imputation-function
#'
#' applies the chosen imputation-function
#'
#' @param FUN function to be applied
#' @param para parameters for the chosen function (incl. variable-name, data.table with all data and additional parameters)
#'
#' @return completed data as vector
#'
#' @export
impute_var = function(FUN, para)
{
  result = do.call(FUN, args=para)
  return (result)
}



