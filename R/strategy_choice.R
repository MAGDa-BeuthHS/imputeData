
#' Get the recommendet strategies depending on variable and gap
#'
#' Each choice gets a maximal gapsize (limit), the strategy as function,
#' required parameters of the stratgy and its name
#'
#' @return list of lists of strategies (one list per variable)
#'
#' @export
get_std_choices = function() {
  choices = list()

  b1 = c(limit = 120, fun = impute_kalman, name = "Impute Kalman")
  b2 = c(limit = 1000000, fun = impute_avg, roll_intervall = 20, name = "Impute Average Week Rollmean 20")
  choices[["b"]] = list(b1, b2)

  g1 = c(limit = 60, fun = impute_kalman, name = "Impute Kalman")
  g2 = c(limit = 1000000, fun = impute_avg, roll_intervall = 20, name = "Impute Average Week Rollmean 20")
  choices[["g"]] = list(g1, g2)


  return(choices)
}

#' Get test strategies to be tested with every gapsize
#'
#' Each choice gets a maximal gapsize (limit), the strategy as function,
#' required parameters of the stratgy and its name
#'
#' @return list of lists of strategies (one list per variable)
#'
#' @export
get_test_choices = function() {
  choices = list()

  b1 = c(limit = 10000, fun = impute_weighted_ma, k = 10, weighting = "simple", name = "Impute Weighted Moving Average (k = 10)")
  b2 = c(limit = 10000, fun = impute_avg, roll_intervall = 20, name = "Impute Average Week Rollmean 20")
  b3 = c(limit = 10000, fun = impute_kalman, name = "Impute Kalman")
  b4 = c(limit = 10000, fun = impute_arima, name = "Impute Arima")
  choices[["b"]] = list(b1, b2, b3, b4)
  # choices[["b"]] = list(b4)

  g1 = c(limit = 10000, fun = impute_weighted_ma, k = 10, weighting = "simple", name = "Impute Weighted Moving Average (k = 10)")
  g2 = c(limit = 10000, fun = impute_avg, roll_intervall = 20, name = "Impute Average Week Rollmean 20")
  g3 = c(limit = 10000, fun = impute_kalman, name = "Impute Kalman")
  g4 = c(limit = 10000, fun = impute_arima, name = "Impute Arima")
  choices[["g"]] = list(g1, g2, g3, g4)
  # choices[["g"]] = list(g4)

  return(choices)
}
