
#' Create average week
#'
#' Create average week (dow, time) excluding holidays and bridgedays
#' and create average holidays and bridgedays
#'
#' @param data the input data.table (containing sensor, z, b, g)
#' @param vars the variable names to be averaged
#' @param interval the data interval
#'
#' @return data.table with avg, rollmean10, rollmean20 for every variable in var
#'
#' @export
create_avg_week = function(data, vars = c("b", "g"), interval = 5) {
  # long zero-sequences might mean problems with the sensor or very unusual conditions like road clusures
  # these shouln't be considered in average values
  if ("b" %in% vars) {
    data = remove_long_zero(data)
  }

  # averages for each sensor, dow, time, depending on holi- and bridgedays
  avg_week = data[, lapply(.SD, mean, na.rm = T), by = list(sensor, dow, time, isholiday, isbridge), .SDcols = vars]

  # split averages depending on holi- and bridgedays
  holidays = avg_week[which(avg_week$isholiday==T & avg_week$dow != "Sonntag"),]
  bridges = avg_week[which(avg_week$isbridge==T & avg_week$isholiday==F),]
  avg_week = avg_week[which(avg_week$isholiday==F & avg_week$isbridge==F),]

  # sensor-ids
  sensors = unique(avg_week[,sensor])

  # produce a temporary sequence of dow and times from mon till sun (to bring avg_week into order)
  by = paste(interval, " mins", sep = "")
  seq = timeDate(seq.POSIXt(
    from = ISOdatetime(2015, 8, 4, 2, 0, 0, tz = ""),
    to = ISOdatetime(2015, 8, 11, 1, 59, 0, tz = ""),
    zone = "Europe/Berlin",
    by = by
  ))
  week = strftime(seq,'%A %H:%M')
  avg_week$day_time = paste(avg_week$dow, avg_week$time, sep = " ")

  # repeat sensors and time sequence (every timestamp for every sensor)
  long_sensors = rep(sensors, each=length(week))
  long_week = rep(week, times = length(sensors))
  # create table with dows, times and sensors
  long_sensor_week = data.table("sensor" = long_sensors, "day_time" = long_week)

  # create table with dows, times and sensors and averages of incomplete variables
  avg_week = merge(long_sensor_week, avg_week, all.x = T, by = c("sensor", "day_time"))
  # remove unnecessary columns
  avg_week = avg_week[, isholiday := NULL][, isbridge := NULL][, day_time := NULL]

  # aggregate average holi- and bridgedays
  holidays = holidays[, lapply(.SD, mean, na.rm = T), by = list(sensor, time), .SDcols = vars]
  bridges = bridges[, lapply(.SD, mean, na.rm = T), by = list(sensor, time), .SDcols = vars]

  # full sequence of times of as many days, as there are sensors
  day = strftime(seq[1:(60*24/interval)],'%H:%M')
  long_sensors = rep(sensors, each=length(day))
  long_day = rep(strftime(seq[1:(60*24/interval)],'%H:%M'), times = length(sensors))
  holi_bridge = data.table("sensor" = long_sensors, "time" = long_day)

  for (var in vars) {
    varname = paste(var, ".rollmean1", sep = "")

    # rename column (for later use)
    names(avg_week)[names(avg_week) == var] = varname
    # impute missing values
    for (s in sensors) {
      avg_week[sensor == s, varname] = imputeTS::na.kalman(unlist(avg_week[sensor == s, varname, with = F]))
    }
    # Rollmean
    avg_week[, paste(var, ".rollmean2", sep = "")] =
      zoo::rollmean(avg_week[, varname, with = F], 2, fill = "extend", by = "sensor")
    avg_week[, paste(var, ".rollmean4", sep = "")] =
      zoo::rollmean(avg_week[, varname, with = F], 4, fill = "extend", by = "sensor")

    # holidays
    varname = paste(var, ".holiday1", sep = "")
    holi_bridge[, varname] = merge(holi_bridge, holidays, all.x = T, by = c("sensor", "time"))[, var, with = F]
    # impute missing values
    holi_bridge[, varname] = rm_nan(unlist(holi_bridge[, varname, with = F]))
    for (s in sensors) {
      holi_bridge[sensor == s, varname] = imputeTS::na.kalman(unlist(holi_bridge[sensor == s, varname, with = F]))
    }
    holi_bridge[, paste(var, ".holiday2", sep = "")] =
      zoo::rollmean(holi_bridge[, varname, with = F], 2, fill = "extend")
    holi_bridge[, paste(var, ".holiday4", sep = "")] =
      zoo::rollmean(holi_bridge[, varname, with = F], 4, fill = "extend")

    # bridgedays
    varname = paste(var, ".bridge1", sep = "")
    holi_bridge[, varname] = merge(holi_bridge, bridges, all.x = T, by = c("sensor", "time"))[, var, with = F]
    # impute missing values
    holi_bridge[, varname] = rm_nan(unlist(holi_bridge[, varname, with = F]))
    for (s in sensors) {
      holi_bridge[sensor == s, varname] = imputeTS::na.kalman(unlist(holi_bridge[sensor == s, varname, with = F]))
    }
    holi_bridge[, paste(var, ".bridge2", sep = "")] =
      zoo::rollmean(holi_bridge[, varname, with = F], 2, fill = "extend")
    holi_bridge[, paste(var, ".bridge4", sep = "")] =
      zoo::rollmean(holi_bridge[, varname, with = F], 4, fill = "extend")
  }

  return (list(avg_week = avg_week, holi_bridge = holi_bridge))
}
