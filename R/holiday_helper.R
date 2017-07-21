
#' All Holidays (Dresden), including 24.12.
#'
#' All Holidays (Dresden), including 24.12.
#'
#' @param year the year you want to get holidays from (only 2015 available)
#'
#' @return vector of characters
#'
#' @export
get_holidays = function(year = 2015){
  if (year != 2015) stop("Holidays only available for 2015")
  reformationDay = timeDate(paste(year, "-10-31", sep=""))
  repetanceDay = timeDate(paste(year, "-11-18", sep=""))
  christmas2 = timeDate(paste(year, "-12-26", sep=""))
  holidays = c(holiday(year, c("NewYearsDay", "GoodFriday", "EasterMonday", "LaborDay",
                               "DEAscension", "PentecostMonday", "DEGermanUnity", "DEChristmasEve",
                               "ChristmasDay", "DENewYearsEve")),
               reformationDay, repetanceDay, christmas2)
  return (as.character(holidays))
}

#' Check date for holiday
#'
#' Tests if date is a holiday (in Dresden)
#'
#' @param x character-vecor with dates to be checked
#' @param holidays character-vecor with dates of holidays
#'
#' @return bool-vector with T = is holiday and F = is no holiday
#'
#' @export
is_holiday = function(x, holidays = get_holidays())
{
  date = substr(x, 1, 10)
  return (date %in% holidays)
}

#' Check date for bridge-day
#'
#' Check date for bridge-day (day between holiday and weekend) (in Dresden)
#'
#' @param x data.table with dow and isholiday
#'
#' @return bool-vector with T = is bridge-day and F = is no bridge-day
#'
#' @export
is_bridge = function(x)
{
  isyesterday = shift(x[, isholiday], n = 12*24, fill = F, type = "lag")
  istomorrow = shift(x[, isholiday], n = 12*24, fill = F, type = "lead" )

  isbridge = (x$dow == "Freitag" & isyesterday) | (x$dow == "Montag" & istomorrow)
  return (isbridge)
}
