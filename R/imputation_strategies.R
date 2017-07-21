
#' Impute missing values of one variable by substitution with avg-week or holi- and bridgeday
#'
#' Impute missing values of one variable by substitution with avg-week or holi- and bridgeday
#'
#' @param data the data.table incl. variables with missing data
#' @param var the name of the variable with missing data
#' @param roll_interval which aggregation-interval of avg-week (5, 10 or 20)
#' @param ... not used
#'
#' @return vector with complete data of variable
#'
#' @export
impute_avg = function(data, var, roll_interval = 1, ...) {
  week = paste(var, ".rollmean", roll_interval, sep = "")
  holiday = paste(var, ".holiday", roll_interval, sep = "")
  bridge = paste(var, ".bridge", roll_interval, sep = "")
  x = unlist(data[, week, with = F])
  x[which(data$isbridge)] = unlist(data[, bridge, with = F][which(data$isbridge)])
  x[which(data$isholiday & data$dow != "Sonntag")] = unlist(data[, holiday, with = F][which(data$isholiday)])
  return(x)
}

#' Impute missing values of one variable by Weighted Moving Average (imputeTS-package)
#'
#' Impute missing values of one variable by Weighted Moving Average (imputeTS-package)
#'
#' @param data the data.table incl. variables with missing data
#' @param var the name of the variable with missing data
#' @param k size of moving window (Expands to both sides of the center element e.g. k=2 means 4 observations (2 left, 2 right))
#' @param weighting Weighting to be used. Accepts the following input:
#'                   "simple" - Simple Moving Average (SMA)
#'                   "linear" - Linear Weighted Moving Average (LWMA)
#'                   "exponential" - Exponential Weighted Moving Average (EWMA)
#'                   (explanation from imputeTS-package)
#' @param ... not used
#'
#' @return vector with complete data of variable
#'
#' @export
impute_weighted_ma = function(data, var, k = 10, weighting = "simple", ...) {
  x = data[[which(colnames(data) == var)]]
  x = imputeTS::na.ma(x, k = k, weighting = weighting)
  return(x)
}

#' Impute missing values of one variable by na.kalman-function (imputeTS-package)
#'
#' Impute missing values of one variable by na.kalman-function (imputeTS-package)
#'
#' @param data the data.table incl. variables with missing data
#' @param var the name of the variable with missing data
#' @param ... not used
#'
#' @return vector with complete data of variable
#'
#' @export
impute_kalman = function(data, var, ...) {
  x = data[[which(colnames(data) == var)]]
  x = imputeTS::na.kalman(x)
  return(x)
}


#' Impute missing values of one variable by a arima-model with kalman-filter
#'
#' Impute missing values of one variable by a arima-model with kalman-filter
#'
#' @param data the data.table incl. variables with missing data
#' @param var the name of the variable with missing data
#' @param p the order of AR
#' @param d the order of I
#' @param q the order of MA
#' @param ... not used
#'
#' @return vector with complete data of variable
#'
#' @export
impute_arima = function(data, var, p = 2, d = 1, q = 2, ...) {
  day = 24 * 60 / 5
  week = 7 * day
  mis_ts = ts(data[, var, with = F], frequency = day);

  fit = arima(mis_ts[1:(week*4)], order = c(p, d, q))
  kr = KalmanRun(mis_ts, fit$model)

  if(length(fit$model$Z)==1) {
    new_ts = kr$states
  } else {
    # Scalarproduct of every state with the model$Z
    new_ts = kr$states %*% fit$model$Z
  }

  x = as.vector(new_ts)

  # change negative values to 0
  x[x < 0] = 0

  return(x)
}
