## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib reval, .registration = TRUE
## usethis namespace: end
NULL

#' @importFrom magrittr %>%

#' The latest (digested) combined minimum data set specification at
#' the time of this package's last build.
#'
#' This data frame contains sufficient information to determine the
#' the checks that need to be performed on each variable in a data
#' set. It is a combination of the pages found in the specifications
#' distributed by BACPAC's DAC.
#' 
#' @format The exact format of this data is an implementation
#'     detail. See the function reval::merge_spec_files, which allows
#'     the user to take a directory of downloaded MDS specifications
#'     and produce the appropriate data frame if they want to use a
#'     newer or otherwise modified form of the specification.
#' 
#' @source Contact the BACPAC DAC.
"min_data_set_spec"

#' An artificially created QSMD test dataset.
#'
#' @format see QSMDS Specification
#' 
#' @source Contact the BACPAC DAC.
"qsmd_test_data"

