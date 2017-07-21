
#' Imputes the data in all csv-files of directory and writes new files
#'
#' Reads, preprocesses and imputes the data in all csv-files of a directory and writes new files to a subdirectory.
#' Default-strategies are those recommended in the text.
#' Only works for data-aggregation-interval of 5 minutes.
#' Lines in the input-csv files have to be in this format:
#' 182,2014-01-01 00:01:00,0,0 (which means: sensor-id, timestamp, occupancy, speed).
#' Filenames of input-csv: sensor-x.csv (where x can be any number)
#'
#' @param source_dir the directory of the input-csv-files
#' @param target_dir the directory where the result-csv-files should be written to
#' @param vars vector of the variablenames to be imputed
#' @param choices list of strategies, including gapsize-limits and function-parameters
#' @param pattern pattern of the input-file-names
#' @param first_timestamp first date of the result-data (as character)
#' @param last_timestamp last date of the result-data (as character)
#' @param header logical, if csv-input-file has header: T
#' @param sep the separator between columns in the csv-input
#' @param append logical, if all results should be written into one csv-file: T
#'
#' @import data.table
#' @import timeDate
#'
#' @return nothing, but writes new csv-files with imputed data to target_dir
#'
#' @export
run_all = function(source_dir = "data/missing",
                   target_dir = "data/imputed",
                   vars = c("b", "g"),
                   choices = get_std_choices(),
                   pattern = "sensor-[0-9]*.csv",
                   first_timestamp = "2015-01-01 00:00:00",
                   last_timestamp = "2015-12-31 23:59:00",
                   header = F,
                   sep = ',',
                   append = F) {
  filenames = list.files(source_dir, pattern = pattern, full.names = TRUE)
  if (!file.exists(target_dir)) {
    dir.create(target_dir)
  }
  new_filename = paste(target_dir, "/imputed_sensordata.csv", sep = "")
  for (file in filenames) {
    old_filename = sub(paste(source_dir, "/", sep = ""), "", file)
    print(paste("start working with", old_filename))
    ready_to_impute = preprocess_data(file = file, vars = vars,
                                      first_timestamp = first_timestamp,
                                      last_timestamp = last_timestamp,
                                      header = header, sep = sep)
    print("preprocessing done")
    imputed_data = impute(data = ready_to_impute, vars = vars, choices = choices )
    print("")
    if (append == F) {
      new_filename = paste(target_dir, "/imputed_", old_filename, sep = "")
    }
    write.table(imputed_data, file = new_filename, sep = ",",
                row.names = F, col.names = F, quote = F, append = append)
  }
}

#' Tests whole imputation-process and measures computing-time for different strategies
#'
#' Reads, preprocesses and imputes the data in all csv-files of a directory and writes new files to a subdirectory.
#' Measures computing-time for strategies and writes results to csv-file in target_dir.
#' Only works for data-aggregation-interval of 5 minutes.
#' Lines in the input-csv files have to be in this format:
#' 182,2014-01-01 00:01:00,0,0 (which means: sensor-id, timestamp, occupancy, speed).
#' Filenames of input-csv: sensor-x.csv (where x can be any number).
#'
#' @param source_dir the directory of the input-csv-files
#' @param target_dir the directory where the result-csv-files should be written to
#' @param vars vector of the variablenames to be imputed
#' @param choices list of lists of strategies (see get_test_choices())
#' @param pattern pattern of the input-file-names
#' @param first_timestamp first date of the result-data (as character)
#' @param last_timestamp last date of the result-data (as character)
#'
#' @return nothing, but writes new csv-files with imputed data to target_dir
#'
#' @export
test_all = function(source_dir = "data/test",
                    target_dir = "data/test_results",
                    vars = c("b", "g"),
                    choices = get_test_choices(),
                    pattern = "sensor-[0-9]*.csv",
                    first_timestamp = "2015-01-01 00:00:00",
                    last_timestamp = "2015-12-31 23:59:00") {
  filenames = list.files(source_dir, pattern = pattern, full.names = TRUE)
  if (!file.exists(target_dir)) {
    dir.create(target_dir)
  }
  for(i in 1:length(choices[[vars[1]]])) {
    choice = list("b" = choices[["b"]][i], "g" = choices[["g"]][i])
    for (file in filenames) {
      old_filename = sub(paste(source_dir, "/", sep = ""), "", file)
      print(paste("start working with", old_filename))
      write(choice[[vars[1]]][[1]]$name, paste(target_dir, "/speed.csv", sep = ""), append = T)
      write(paste(Sys.time(), "start preprocessing"), paste(target_dir, "/speed.csv", sep = ""), append = T)
      ready_to_impute = preprocess_data(file = file, vars = vars,
                                        first_timestamp = first_timestamp,
                                        last_timestamp = last_timestamp)
      print("preprocessing done")
      write(paste(Sys.time(), "start imputing"), paste(target_dir, "/speed.csv", sep = ""), append = T)
      imputed_data = impute(data = ready_to_impute, vars = vars, choices = choice )
      write(paste(Sys.time(), "imputation done"), paste(target_dir, "/speed.csv", sep = ""), append = T)
      print("")
      new_filename = paste(target_dir, "/", old_filename, sep = "")
      write.table(imputed_data, file = new_filename, sep = ",",
                  row.names = F, col.names = F, quote = F)
    }
  }
}
