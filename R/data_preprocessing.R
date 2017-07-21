
#' Preprocessing of raw data
#'
#' read from csv, add rows, aggregate, create and add averages
#'
#' @param file the csv-file to load
#' @param vars the variables to be imputed
#' @param header logical, if csv-input-file has header
#' @param sep the separator between columns in the csv-input
#' @param ... can be first_timestamp and last_timestamp
#'
#' @return data.table with all timestamps, averages and incomplete variables
#'
#' @export
preprocess_data = function(file, vars = c("b", "g"), header = F, sep = ',', ...) {
  raw_data = read_data_csv(file, header = header, select = 1:(2+length(vars)),
                           vars = vars, sep = sep)
  full_data = add_empty_rows(raw_data, vars = vars, ...)
  agg_data = aggregate_to_interval(full_data, vars = vars)
  agg_data_time_cols = add_time_col(agg_data)
  avg = create_avg_week(agg_data_time_cols, vars = vars, interval = 5)
  ready_to_impute = add_avg_cols(agg_data_time_cols, avg)
  ready_to_impute = impute_first_value(ready_to_impute, vars = vars)
  return(ready_to_impute)
}

#' Add average week and holiday columns to table
#'
#' Add average week and holiday columns to table (also rollmean)
#'
#' @param data the data.table where avg-cols should be added
#' @param avg the list containing avg-week and holi- and bridgeday
#'
#' @return data.table with added avg-columns
#'
#' @export
add_avg_cols = function(data, avg) {
  merged_data = merge(data, avg[["avg_week"]], by = c("sensor", "dow", "time"), all.x = T, sort = F)
  merged_data = merge(merged_data, avg[["holi_bridge"]], by = c("sensor", "time"), all.x = T, sort = F)
  return(merged_data)
}

#' Impute first value if it is NA
#'
#' Imputes the first value of each variable if it is NA.
#' This is because some methods can't handle NA at the beginning.
#'
#' @param data the data.table where avg-cols should be added
#' @param vars the variables with missing data
#'
#' @return the data.table without NA at the beginning
#'
#' @export
impute_first_value = function(data, vars) {
  # impute the first value (if NA), because some functions can't handle NA at start
  for (var in vars) {
    if (is.na(data[1, var, with = F])) {
      data[1, var] = data[1, paste(var, ".rollmean1", sep = ""), with = F]
    }
  }
  return(data)
}

#' Aggregate data to other time-intervals
#'
#' Take first elements of every column (sensor, z, posix)
#' and use mean for variables with missing data
#'
#' @param data the input-datatable (including sensor, z, posix and variables with missing data)
#' @param vars the variables with missing data
#' @param interval the interval of the target-datatable
#'
#' @return a datatable with bigger time-intervals
#'
#' @export
aggregate_to_interval = function(data, vars = c("b", "g"), interval = 5) {
  # data has to be ordered by sensor and date

  # datatable with sensor and z column, use only every fifth element
  agg_data = data.table(data[seq(1, length(data$sensor), interval), list(sensor, z, posix)])

  # add the variables with missing data
  data[, "helper"] = rep(seq(1:(length(data$sensor)/interval)), each = interval) # ids for 5 consecutive rows
  agg_data[, vars] = data[, lapply(.SD, mean, na.rm = T), by = helper, .SDcols = vars][, vars, with = F]
  agg_data[, vars] = agg_data[, lapply(.SD, rm_nan), .SDcols = vars]

  return(agg_data)
}


#' Read csv to data.table
#'
#' Read csv to data.table
#'
#' @param file filename to read from
#' @param sep separator within rows in csv-file
#' @param header does csv-file contain header
#' @param select which columns to read
#' @param vars the variables with missing data
#'
#' @return filled data.table
#'
#' @export
read_data_csv = function(file, sep = ',', header = F, select = c(1, 2, 3, 4),
                         vars = c("b", "g")) {
  colnames = c("sensor", "z", vars)
  raw_data = fread(file, sep = sep, header = header, select = select,
                    col.names = colnames)
  return(raw_data)
}


#' Adds rows to data.table for timestamps with missing data
#'
#' Adds rows to data.table for timestamps with missing data,
#' Removes impossible speed-values and rows with double timestamps
#'
#' @param raw data-input (data.table or data.frame)
#' @param vars the variables with missing data
#' @param first_timestamp start date
#' @param last_timestamp end date
#'
#' @return data.table rows for each timestamps
#'
#' @export
add_empty_rows = function(raw, vars = c("b", "g"), first_timestamp = NULL, last_timestamp = NULL) {
  raw = data.table(raw)

  # remove impossible speed-values
  if ("b" %in% vars & "g" %in% vars) {
    raw[b == 0, "g"] = NA
  }

  sensors = unique(raw[, sensor])
  # set keys for raw
  setkeyv(raw, cols = c("sensor", "z"))

  # remove rows with double keys (first used)
  raw = unique(raw)
  raw = raw[order(sensor, z)]
  if (is.null(first_timestamp)) {
    first_timestamp = raw[1,z]
    first_timestamp = paste(substr(first_timestamp, 1, 15), "0:00", sep = "")
  }
  if (is.null(last_timestamp)) {
    last_timestamp = raw[length(raw$z),z]
    last_timestamp = paste(substr(last_timestamp, 1, 15), "9:00", sep = "")
  }
  # create sequence between start and end
  by = "1 mins"
  from = strptime(first_timestamp, "%Y-%m-%d %H:%M:%S")
  to = strptime(last_timestamp, "%Y-%m-%d %H:%M:%S")
  seq = seq.POSIXt(
    from = from,
    to = to,
    format = '%d-%m-%Y %H:%M:%S',
    zone = "Europe/Berlin",
    by = by
  )

  z = strftime(seq,'%F %T') # Date and Time as String

  # repeat sensors and time sequence (every timestamp for every sensor)
  long_sensors = rep(sensors, each=length(seq))
  long_seq = rep(seq, times = length(sensors))
  long_z = rep(z, times = length(sensors))
  # create table with timestamps and sensors
  data = data.table("sensor" = long_sensors, "z" = long_z, "posix" = long_seq, key = c("sensor", "posix"))

  # set keys for merge
  # setkeyv(data, cols=c("sensor", "posix"))

  # merge time-sensor-table with full data
  data = merge(data, raw, all.x = T, by = c("sensor", "z"), sort = F)
  # data = data[order(sensor, z)]

  return(data)
}

#' Adds additional columns for avg-imputations
#'
#' Adds additional columns for avg-imputations: dow, time, isholiday and isbridge
#'
#' @param data data-input
#'
#' @return data.table
#'
#' @export
add_time_col = function(data) {
  data[, "dow"] = data[, weekdays(posix)]
  data[, "time"] = substr(data[, z],12,16)
  data[, "isholiday"] = is_holiday(data[, z])
  data[, "isbridge"] = is_bridge(data[, list(isholiday, dow)])
  return(data)
}
