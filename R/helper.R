
#' Substitutes NaN in a vector with NA
#'
#' Substitutes NaN in a vector with NA
#'
#' @param x the vecor with NaN
#'
#' @return the vecor without NaN, but with NA instead
#'
#' @export
rm_nan = function(x) {
  x[is.nan(x)] = NA
  return(x)
}

#' Finds gaps in a vector and returns their startpoints and lengths
#'
#' Finds gaps in a vector and returns their startpoints and lengths
#'
#' @param x the vector
#'
#' @return a data.table with startpoints and lengths of all gaps in x
#'
#' @export
get_gaps = function(x) {
  ind = which(!is.na(x))            # get positions of nonmissing values
  if (is.na(x[1]))                  # if it begins with NA
    ind = c(0, ind)                 # add first pos
  gap_size = diff(c(ind, length(x) + 1)) - 1
  gap_start = ind[which(gap_size > 0)] + 1
  gap_size = gap_size[which(gap_size > 0)]
  return (data.table(gap_start, gap_size))
}


#' Exclude long 0-sequences
#'
#' Removes all rows with long 0-sequences in var from a table
#'
#' @param data the table
#' @param var the variable to be checked
#' @param limit the max. allowed 0-sequence-length
#'
#' @return the table without 0-sequences in var
#'
#' @export
remove_long_zero = function(data, var = "b", limit = 120)
{
  # starts and lengths of same-value-intervals in data$var
  rnums = rle(unlist(data[, .SD, .SDcols = var]))
  # starts of same-value-intervals in data
  length_sums = cumsum(c(1,rnums$lengths))
  # indizes of long 0-sequences in rnums
  long_series_length_idx = which(rnums$lengths > limit & rnums$values == 0)
  # lengths of long 0-sequences
  long_series_lengths = rnums$lengths[long_series_length_idx]
  # starts of 0-sequences in data
  long_series_idx = length_sums[long_series_length_idx]
  # indizes of long 0-sequence-elements in data
  all_long_element_idx = as.vector(unlist(mapply(seq, from=long_series_idx, length.out=long_series_lengths)))
  # remove long 0-sequences from data
  if (length(all_long_element_idx) != 0) {
    data = data[-(all_long_element_idx),]
  }
  return (data)
}
