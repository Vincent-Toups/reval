#' Check if a character vector can be parsed entirely as numeric
#'
#' This function checks whether all elements in a character vector can be successfully
#' parsed as numeric values. If all elements can be parsed as numeric, the function
#' returns TRUE; otherwise, it returns FALSE. 
#' 
#' @param x A character vector to be checked.
#' @return A logical value indicating whether all elements in the vector can be parsed as numeric.
#' 
#' @examples
#' is_numeric_codelist(c("1", "2", "3"))  # TRUE
#' is_numeric_codelist(c("1", "2", "a"))  # FALSE
#'
is_numeric_codelist <- function(x) {
    sum(is.na(suppressWarnings(as.numeric(x)))) == 0
}

#' @title Get Codelist Terms
#' @description Get the unique codelist terms from the specification for a given variable.
#' @param variable_name A character string specifying the name of the variable.
#' @param spec A data frame containing the specification of the dataset variables.
#' @return A character vector containing unique codelist terms.
#' @examples
#' # Assuming a dataset specification `spec`
#' get_codelist_terms("variable", spec)
#' @author Vincent Toups
get_codelist_terms <- function(variable_name, spec){
    variable_spec <- dplyr::filter(spec, variable == variable_name)
    unique(variable_spec$codelist_terms)
}

#' Check if values can be parsed as ISO8601 durations
#'
#' @description
#' This function is a wrapper for the `check_iso8601_durations_cpp` function 
#' implemented in C++. It checks if the input values can be parsed as durations
#' according to the ISO8601 standard.
#' 
#' @param duration_vec A character vector containing the duration strings to check.
#' 
#' @return A logical vector with the same length as `duration_vec`, 
#' indicating whether each value can be parsed as an ISO8601 duration.
#' 
#' @note
#' The actual implementation of the parsing check is done by the 
#' `check_iso8601_durations_cpp` function, which provides a more efficient
#' implementation using C++.
#' 
#' @seealso
#' \code{\link{check_iso8601_durations_cpp}}
#' 
#' @examples
#' \dontrun{
#' parses_as_iso8601_duration(c("P1Y2M3DT4H5M6S", "invalid"))
#' }
#' 
#' @export
parses_as_iso8601_duration <- function(duration_vec) {
    check_ISO8601_durations_cpp(duration_vec)
}


#' Debugging status variable
#'
#' @keywords internal
debugging <- FALSE

#' Print debugging information conditionally
#'
#' This function behaves like the base print function, but only prints the output
#' when the package-level variable 'debugging' is TRUE.
#'
#' @param x The object to be printed.
#' @param ... Further arguments passed to the print function.
#'
#' @export
debug_print <- function(x, ...){
    if (get("debugging", envir = parent.frame(), inherits = TRUE)){
        print(x, ...)
    }
}

#' Return the unparsed column name for "name"
#'
#' @param name (or names)
#' @return the unparsed name
unparsed_column_name <- function(name){
    paste("unparsed__",name,sep="");
}


#' Load a data frame but keep both a parsed and unparsed version of each column.
#'
#' @param filename Path to the CSV file to be loaded.
#' @param col_types Column types. NULL by default.
#' @return A tibble with twice the columns of the original file, half of which are unparsed duplicates.
#' @export
reval_read_csv <- function(filename, col_types = NULL){
    parsed <- if (is.null(col_types)) { 
                  readr::read_csv(filename, guess_max = 1e5)
              } else {
                  readr::read_csv(filename, col_types = col_types)
              }
    unparsed <- readr::read_csv(filename, col_types = readr::cols(.default = "c"))
    names(unparsed) <- unparsed_column_name(names(unparsed))
    dplyr::bind_cols(parsed, unparsed) %>%
    dplyr::mutate(index__ = seq(nrow(parsed))) %>%
    tibble::as_tibble()
}

#' Load a data frame from an xpt but keep both a parsed and unparsed version of each column.
#'
#' @param filename Path to the XPT file to be loaded.
#' @return A tibble with twice the columns of the original file, half of which are unparsed duplicates.
#' @export
reval_read_xpt <- function(filename){
    data <- haven::read_xpt(filename) %>% dplyr::as_tibble()
    tmpfn <- tempfile()
    readr::write_csv(data, tmpfn)
    out <- reval_read_csv(tmpfn)
    file.remove(tmpfn)
    out
}

#' Load either a CSV or XPT file and save unparsed rows
#' 
#' @param filename Path to the CSV or XPT file to be loaded.
#' @return A tibble with both parsed and unparsed columns.
#' @export
reval_read_data <- function(filename){
    ext <- tools::file_ext(filename);
    out <- switch(ext,
                  csv = reval_read_csv(filename),
                  xpt = reval_read_xpt(filename))
    if(is.null(out)){
        stop(sprintf("Unrecognized file extension %s. Only 'csv' and 'xpt' files are supported.", ext))
    }
    out %>% tibble::as_tibble()
}

#' Check if a character vector contains valid ISO 8601 date strings
#'
#' @param date_vec Character vector with potential date strings.
#' @return Logical vector indicating whether each string is a valid ISO 8601 date.
#' @export
parses_as_iso8601_date <- function(date_vec) {
  # Regular expression to check syntax
  reg_expr <- "^\\d{4}-\\d{2}-\\d{2}$"
  
  # Apply regular expression and date parsing checks
  stringr::str_detect(date_vec, reg_expr) & 
    !is.na(lubridate::ymd(date_vec, quiet = TRUE))
}
## parses_as_iso8601_date <- function(date_vec) {
##   # Regular expression to check syntax
##   reg_expr <- "^\\d{4}-\\d{2}-\\d{2}$"
##   # Apply regular expression and date parsing checks
##   purrr::map_lgl(date_vec, ~{
##     stringr::str_detect(.x, reg_expr) & 
##     !is.na(lubridate::ymd(.x, quiet = TRUE))
##   })
## }


#' Convert "Yes" and "No" to Logical Values
#'
#' This function takes a character vector as input and converts elements with the value "Yes" to `TRUE`
#' and elements with the value "No" to `FALSE`. If any other values are encountered in the vector, 
#' the function will stop execution and return an informative error message.
#'
#' @param char_vec A character vector that should contain only "Yes" and "No" values to convert to logical.
#'
#' @return A logical vector with `TRUE` for "Yes" and `FALSE` for "No".
#'
#' @export
convert_yes_no_to_logical <- function(char_vec) {
  
  # Check for unexpected values
  unexpected_values <- setdiff(unique(char_vec), c("Yes", "No"))
  if (length(unexpected_values) > 0) {
    stop("The input vector contains unexpected values: ", paste(unexpected_values, collapse = ", "), 
         ". Expected 'Yes' or 'No' only.")
  }
  
  # Convert "Yes" to TRUE and "No" to FALSE
  logical_vec <- ifelse(char_vec == "Yes", TRUE, FALSE)
  
  return(logical_vec)
}


#' @title Collapses a Character Vector into a Comma-Separated String
#'
#' @description This function takes a character vector as input and collapses it into a single
#' string, with elements separated by commas. 
#'
#' @param s a character vector
#'
#' @return a single string with elements of the input vector separated by commas
#'
#' @examples
#' collapse_commas(c("apple", "banana", "cherry"))
#' # [1] "apple, banana, cherry"
#'
collapse_commas <- function(s){
    stringr::str_c(s, collapse = ", ")
}


#' @title Conditionally Stop Execution
#' @description Stop the execution of the program if the condition is true. 
#' This function is useful for debugging, as it can provide additional context around errors.
#' @param cond A logical value. If TRUE, the execution stops.
#' @param msg A character string. The error message to be returned if the condition is true.
#' @param ... Further arguments to be passed to the base::sprintf function for the error message.
#' @return No return value. The function either stops the execution with an error or does nothing.
#' @examples
#' # Execution continues
#' stop_if(FALSE, "This is an error message")
#' # Execution stops with an error
#' stop_if(TRUE, "This is an error message")
#' @author Vincent Toups
stop_if <- function(cond, msg, ...){
    if(cond){
        print(base::sys.calls());
        base::stop(base::sprintf(msg,...));
    }
}

#' @title Get Variable Type
#' @description Get the type of a variable in a dataset according to a provided specification.
#' @param dataset_name A character string specifying the name of the dataset.
#' @param variable_name A character string specifying the name of the variable.
#' @param spec A data frame containing the specification of the dataset variables.
#' @return A character string indicating the type of the variable.
#' @examples
#' # Assuming a dataset specification `spec` and a dataset `dataset`
#' get_variable_type("dataset", "variable", spec)
#' @author Vincent Toups
get_variable_type <- function(dataset_name, variable_name, spec){
    var_info <- dplyr::filter(spec, dataset == dataset_name & variable == variable_name)
  
    if (variable_name == "QSEVLINT") { ## unfortunately there doesn't
                                       ## appear to be anyway to avoid
                                       ## a special case for this
                                       ## column.
        "QSEVLINT"
    } else if (!is.na(var_info$where_clause[[1]])) {
        debug_print(dplyr::select(var_info, variable, where_clause, where_clause_variable, where_clause_value, value_level_codelist, codelist))
        "WHERECLAUSE"
    } else if (!is.na(var_info$codelist[[1]])) {
        #print(var_info)
        "CODELIST"
    } else {
        var_info$data_type
    }
}


#' @title Get Format Field
#' @description Get the format for a variable in a dataset according to a provided specification.
#' @param dataset_name A character string specifying the name of the dataset.
#' @param variable_name A character string specifying the name of the variable.
#' @param spec A data frame containing the specification of the dataset variables.
#' @return A character string indicating the format of the variable.
#' @examples
#' # Assuming a dataset specification `spec` and a dataset `dataset`
#' get_format("dataset", "variable", spec)
#' @author Vincent Toups
get_format <- function(dataset_name, variable_name, spec){
  var_info <- dplyr::filter(spec, dataset == dataset_name &
                              variable == variable_name);
  var_info$format;
}

#' @title Get Mandatory Field
#' @description Get the mandatory field for a variable in a dataset according to a provided specification.
#' @param dataset_name A character string specifying the name of the dataset.
#' @param variable_name A character string specifying the name of the variable.
#' @param spec A data frame containing the specification of the dataset variables.
#' @return A logical value indicating if the variable is mandatory or not.
#' @examples
#' # Assuming a dataset specification `spec` and a dataset `dataset`
#' get_mandatory("dataset", "variable", spec)
#' @author Vincent Toups
get_mandatory <- function(dataset_name, variable_name, spec){
  var_info <- dplyr::filter(spec, dataset == dataset_name &
                                  variable == variable_name);
  v <- var_info$variable_mandatory %>% unique();
  if(length(v) != 1){
      stop(sprintf("Only one value of the %s variable_mandatory expected in get_mandatory, but we got %s",variable_name, collapse_commas(v)));
  }
  v
}

#' @title Add Context to Check Messages
#' @description This function adds additional context to the check_name and message columns of a data frame. 
#' The context is defined by a given variable and its associated where_clause variable and value.
#' @param df A data frame containing at least the columns check_name and message.
#' @param variable The name of the variable adding context.
#' @param where_clause_variable The name of the variable that is part of the where_clause condition.
#' @param where_clause_variable_value The value of the where_clause variable.
#' @return The same data frame, but with additional context added to the check_name and message columns.
#' @examples
#' df <- data.frame(check_name = "Check 1", message = "Error in row 1")
#' add_whereclause_context(df, "var1", "cond1", "value1")
#' @author Vincent Toups
add_where_clause_context <- function(df, variable, where_clause_variable, where_clause_variable_value) {
    # Construct context string
    context <- paste0("(Context: variable ", variable, 
                      " when ", where_clause_variable, 
                      " is ", where_clause_variable_value, "): ")
    
    # Add context to check_name and message columns
    df$check_name <- paste0(context, df$check_name)
    df$message <- paste0(context, df$message)
    
    return(df)
}


#' @title Apply WHERE Clause Checks
#' @description This function applies a set of checks, based on a given specification, to a data column within a dataset. 
#' The data column is assumed to have a WHERE clause, meaning it needs to be evaluated differently depending on the values of another column. 
#' This function groups the data by that other column's values and applies the appropriate checks for each group.
#' @param dataset A data frame representing the dataset to be checked.
#' @param spec A data frame containing the specification of the dataset variables.
#' @param dataset_name A character string specifying the name of the dataset.
#' @param variable_name A character string specifying the name of the variable.
#' @return A data frame containing information about the variable for each subset of data defined by the WHERE clause.
#' @examples
#' # Assuming a dataset `data` and its specification `spec`
#' check_where_clause(data, spec, "dataset_name", "variable_name")
#' @author Vincent Toups
check_where_clause <- function(dataset, spec, dataset_name, variable_name){
    variable_information <- dplyr::filter(spec,
                                          dataset == dataset_name &
                                          variable == variable_name) %>%
        dplyr::select(variable,
                      where_clause,
                      where_clause_variable,
                      where_clause_value,
                      value_level_codelist,
                      value_level_data_type,
                      value_level_format,
                      value_level_mandatory,
                      codelist,
                      codelist_terms, format);
    wcvar <- variable_information$where_clause_variable[[1]];
    dataset %>%
        dplyr::group_by(across(all_of(wcvar))) %>%
        dplyr::group_split() %>%
        purrr::map_dfr(function(df){
            constraint <- df[[wcvar]][[1]];
            info <- dplyr::filter(variable_information,
                                  where_clause_value == constraint) %>%
                dplyr::distinct();
            if(nrow(info)==0){
                stop(sprintf("No where_clause information when checking %s %s:%s.",
                             variable_name,
                             wcvar,
                             constraint));
            }
            codelist <- unique(dplyr::pull(info, value_level_codelist));
            if(length(codelist)>1){
                stop(sprintf("During check_where_clause (%s) we got a multi-codelist value: %s",
                             variable_name,
                             collapse_commas(codelist)));
            }
            if(length(codelist)==0){
                codelist <- NA;
            }
            codelist_terms <- dplyr::pull(info, codelist_terms) %>%
                stringr::str_split("::", simplify = T);
            data_type <- dplyr::pull(info, value_level_data_type);
            mandatory <- dplyr::pull(info, value_level_mandatory)[[1]];
            format <- unique(dplyr::pull(info, format));
            if(length(format)!=1){
                stop(sprintf("In check_where_clause we encountered an ambiguous format (%s) checking %s (%s:%s).",
                     collapse_commas(format),
                     variable_information,
                     wcvar,
                     constraint));
            }
            debug_print(sprintf("variable: %s, constraint: %s, wc_variable: %s, codelist: %s, terms %s, data_type %s, format %s, mandatory %s",
                          variable_name,
                          constraint,
                          wcvar,
                          paste(codelist, collapse = ","),
                          paste(codelist_terms, collapse = ",\n\t"),
                          data_type, format, mandatory));
            if(!is.na(codelist)){
                r <- check_codelist(df, variable_name, codelist_terms, convert_yes_no_to_logical(mandatory));
                r <- add_where_clause_context(r, variable_name, wcvar, constraint);
            } else {
                r <- check_type(df, variable_name, data_type, format, convert_yes_no_to_logical(mandatory));
                r <- add_where_clause_context(r, variable_name, wcvar, constraint);
            }
            r
        });
}

#' @title Add Rows to a Tibble
#' @description This function creates a new tibble from provided arguments and appends it to an existing tibble.
#' @param tbl A tibble to which new rows will be added.
#' @param ... Variable arguments used to create a new tibble.
#' @return The original tibble with the new rows appended at the end.
#' @examples
#' # Assume `tbl` is an existing tibble
#' add_to_tibble(tbl, col1 = "value1", col2 = "value2")
#' @author Vincent Toups
add_to_tibble <- function(tbl, ...){
    new_tibble <- dplyr::tibble(...);
    debug_print(new_tibble);
    base::rbind(tbl, new_tibble);
}

#' @title Format Row Numbers
#' @description This function formats a vector of row numbers into a comma-separated string. 
#' If the length of the vector is larger than `max_n`, it will be truncated and " ..." will be appended at the end.
#' @param row_numbers A vector of row numbers to be formatted.
#' @param max_n Maximum number of row numbers to be included in the formatted string.
#' @return A formatted string of row numbers.
#' @examples
#' format_row_numbers(c(1,2,3,4,5), max_n = 3)
#' @author Vincent Toups
format_row_numbers <- function(row_numbers, max_n=10){
    if(length(row_numbers)==0) return(NA);
    truncated <- FALSE;
    if(length(row_numbers)>max_n){
        row_numbers <- row_numbers[seq_len(max_n)];
        truncated <- TRUE;
    }
    sprintf("%s%s", collapse_commas(row_numbers), if(truncated) {" ..."} else {""})
}


#' Check Numeric Codelist
#'
#' This function is called by check_codelist when the codelist is numeric.
#' It converts the codelist and the column values to numeric before running
#' the check_codelist function again.
#'
#' @param dataset A tibble/data.frame containing the data to be checked.
#' @param variable_name The name of the column to check.
#' @param codelist A numeric vector of valid values for the column.
#' @param allow_na Logical indicating whether NA values should be allowed in the column. Default is TRUE.
#' @return A tibble containing information about the checks and potential mismatches.
#' @examples
#' check_numeric_codelist(df, "column_name", c("1", "2"), allow_na = TRUE)
#' @author Vincent Toups
#' @export
check_numeric_codelist <- function(dataset, variable_name, codelist, allow_na = TRUE) {
  # Convert the codelist and variable column to numeric
  codelist_numeric <- as.numeric(codelist)
  dataset[[variable_name]] <- as.numeric(dataset[[variable_name]])
  
  # Call the check_codelist function again
  check_codelist(dataset, variable_name, codelist_numeric, allow_na)
}


#' @title Check Codelist
#' @description This function verifies if all the values of a specified column of a dataset are found in a given codelist. 
#' It generates messages detailing checks and any mismatches. If a value is not in the codelist, it is considered an error.
#' @param dataset A tibble/data.frame containing the data to be checked.
#' @param variable_name The name of the column to check.
#' @param codelist A vector of valid values for the column.
#' @param allow_na Logical indicating whether NA values should be allowed in the column. Default is TRUE.
#' @return A tibble containing information about the checks and potential mismatches.
#' @examples
#' check_codelist(df, "column_name", c("value1", "value2"), allow_na = TRUE)
#' @author Vincent Toups
check_codelist <- function(dataset, variable_name, codelist, allow_na = TRUE){
    if(length(unique(allow_na))!=1){
        stop(sprintf("in check_codelist non-uniform allow_na %s", collapse_commas(allow_na)));
    }

    if (is.character(codelist) & is_numeric_codelist(codelist)) {
        return(check_numeric_codelist(dataset, variable_name, codelist, allow_na))
    }
  
    messages <- tibble::tibble(check_name=character(0), pass=logical(0), message=character(0), row_numbers=character(0));
    na_ii <- is.na(dataset[[variable_name]]);
    allow_na <- if(unique(allow_na)=="Yes") { T } else { F };
    if(!allow_na){
        messages <- rbind(messages, check_na(dataset, variable_name));
    }
    not_na_rows <- dataset %>% dplyr::filter(!na_ii);
    ## print(variable_name);
    ## print(codelist);
    ## print(not_na_rows);    
    not_in_codelist <- not_na_rows %>% dplyr::filter(!(.data[[variable_name]] %in% codelist));
    not_in_codelist_values <- not_in_codelist %>% `[[`(variable_name);
    passed2 <- nrow(not_in_codelist) == 0;
    debug_print(messages);
    messages <- add_to_tibble(messages,
                              check_name=sprintf("Column %s in codelist.", variable_name),
                              pass=passed2,
                              message = if(passed2){
                                            "All columns in the codelist."
                                        } else {
                                            sprintf("%s: %d rows were not in the codelist (%s). These values were in error: (%s), this is the codelist: (%s). Common issues have to do with quotation marks, which must be ASCII, not Unicode, values.", variable_name,
                                                    length(not_in_codelist_values),
                                                    codelist,
                                                    collapse_commas(unique(not_in_codelist_values)),
                                                    collapse_commas(codelist));
                                        },
                              row_numbers=format_row_numbers(not_in_codelist$index__));
    messages
}

#' @title Check for NA Values in a Column of a Dataset
#' 
#' @description 
#' This function checks if a column in a dataset has any NA (Not Available) values. 
#' It generates a tibble that includes the check name, whether the check passed, 
#' the associated message, and the row numbers where NA values were found.
#' 
#' @param dataset A tibble or dataframe containing the data to be checked.
#' @param variable_name A string that specifies the column name to be checked for NA values.
#' 
#' @return 
#' A tibble that includes the check name (specified as "Column [variable_name] doesn't have NA values."), 
#' a boolean indicating whether the check passed, an associated message, and the row numbers where NA values were found. 
#' The row numbers are formatted using the `format_row_numbers` function.
#' 
#' @examples 
#' # Assuming 'df' is a dataset and 'age' is one of its columns
#' check_na(df, "age")
#' 
#'  
check_na <- function(dataset, variable_name){
    messages <- tibble::tibble(check_name=character(0), pass=logical(0), message=character(0), row_numbers=character(0));
    na_ii <- is.na(dataset[[variable_name]]);

    na_ii <- is.na(dataset[[variable_name]]);
    na_indices <- dataset[["index__"]][na_ii];
    passed <- sum(na_ii) == 0;
    messages <- add_to_tibble(messages,
                              check_name=sprintf("Column %s doesn't have NA values.", variable_name),
                              pass=passed,
                              message = if(passed){
                                            "All values valid."
                                        } else {
                                            sprintf("Some values were missing/NA");
                                        },
                              row_numbers=if(passed) {
                                              NA
                                          } else {
                                              format_row_numbers(na_indices)
                                          });

    messages;
}

#' @title Parse as Integer
#' 
#' @description 
#' This function checks if given values can be parsed as integers.
#' 
#' @param values A character vector to be checked.
#' 
#' @return 
#' A logical vector indicating whether each value can be parsed as an integer.
#' 
#'  
parses_as_int <- function(values){
    grepl("^-?\\d+$", values);
}

#' @title Parse as Formatted Integer
#' 
#' @description 
#' This function checks if a given string can be parsed as an integer with a specified maximum number of digits.
#' 
#' @param input_string A string to be checked.
#' @param format A string specifying the maximum number of digits the integer can have.
#' 
#' @return 
#' A logical value indicating whether the string can be parsed as a formatted integer.
#' 
#'  
parses_as_formatted_int <- function(input_string, format) {    
    if(is.na(format)){
        return(input_string==input_string);
    }
  # Format for integer
    max_digits <- as.integer(format)
    if(is.na(max_digits)){
        stop(sprintf("Error parsing format during parse_as_formatted_int %s.", format))
    }

  # Construct the regular expression
  regex <- paste0("^-?\\d{1,", max_digits, "}$")

  # Check the string against the regex
  grepl(regex, input_string)
}

#' @title Parse as Float
#' 
#' @description 
#' This function checks if given values can be parsed as floating point numbers.
#' 
#' @param values A character vector to be checked.
#' 
#' @return 
#' A logical vector indicating whether each value can be parsed as a float.
#' 
#'  
parses_as_float <- function(values){
    grepl("^-?\\d*\\.?\\d+([eE][-+]?\\d+)?$", values)
}

#' @title Parse as Formatted Float
#' 
#' @description 
#' This function checks if a given string can be parsed as a float with a specified format.
#' 
#' @param input_string A string to be checked.
#' @param format A string specifying the maximum number of digits before and after the decimal point.
#' 
#' @return 
#' A logical value indicating whether the string can be parsed as a formatted float.
#' 
#'  
parses_as_formatted_float <- function(input_string, format) {
                                        # Split the format into the maximum number of digits before and after the decimal
    format_parts <- strsplit(format, "\\.")[[1]]
    before_dot <- as.integer(format_parts[1])
    after_dot <- as.integer(format_parts[2])

    if(is.na(before_dot)){
        stop(sprintf("Failed to parse first part of formatted float %s", format))
    }

    if(is.na(after_dot)){
        stop(sprintf("Failed to parse second part of formatted float %s", format))
    }


                                        # Construct the regular expression
    regex <- paste0("^-?\\d{1,", before_dot, "}\\.?\\d{0,", after_dot, "}$")
    regex_no_dot <- paste0("^-?\\d{1,", before_dot, "}$")

                                        # Check the string against the regex
    grepl(regex, input_string) | grepl(regex_no_dot, input_string)
}

#' Check if values in a column can be parsed as integers
#'
#' @param dataset A data frame containing the data to be checked.
#' @param variable_name The name of the variable/column to check.
#' @param format An optional specification of the format that the integer values should conform to.
#' @param allow_na Logical flag indicating whether NA values are allowed. Defaults to FALSE.
#' 
#' @return A tibble that records the name of the check, whether it passed, a message summarizing the check results, and the row numbers of any values that failed the check.
#' 
#' @examples 
#' \dontrun{
#' check_integer(dataset = df, variable_name = "column1", format = NULL, allow_na = FALSE)
#' }
#' @export
check_integer <- function(dataset, variable_name, format=NULL, allow_na=FALSE){
    if(length(unique(allow_na))!=1){
        stop(sprintf("in check_integer non-uniform allow_na %s", collapse_commas(allow_na)));
    }
    messages <- tibble::tibble(check_name=character(0), pass=logical(0), message=character(0), row_numbers=character(0));
    na_ii <- is.na(dataset[[variable_name]]);
    if(!allow_na){
        messages <- base::rbind(messages, check_na(dataset, variable_name));
    }
    not_na_rows <- dataset %>% dplyr::filter(!na_ii);
    int_ii <- if(is.null(format)) {
                  addendum <- "";
                  parses_as_int(not_na_rows[[unparsed_column_name(variable_name)]]);
              } else {
                  addendum <- sprintf(" with no more than %s digits", format);
                  parses_as_formatted_int(not_na_rows[[unparsed_column_name(variable_name)]], format);
              }
    check_name <- sprintf("Column %s contains only integers.", variable_name);
    add_to_tibble(messages,
                  if(sum(!(int_ii))==0){
                      tibble::tibble(check_name=check_name,
                             pass=TRUE,
                             message=sprintf("All values were integers%s.",addendum),
                             row_numbers=NA);
                  } else {
                      not_int_rows <- not_na_rows %>% dplyr::filter(!int_ii) %>% dplyr::pull(index__);
                      bad_count <- length(not_int_rows);
                      tibble::tibble(check_name=check_name,
                             pass=FALSE,
                             message=sprintf("%d values failed to parse as integers%s.", bad_count, addendum),
                             row_numbers=format_row_numbers(not_int_rows))
                  });                  
}

#' Check if values in a column are textual, with optional NA check
#'
#' @param dataset A data frame containing the data to be checked.
#' @param variable_name The name of the variable/column to check.
#' @param allow_na Logical flag indicating whether NA values are allowed. Defaults to FALSE.
#' 
#' @return A tibble that records the name of the check, whether it passed, a message summarizing the check results, and the row numbers of any values that failed the check.
#' 
#' @examples 
#' \dontrun{
#' check_text(dataset = df, variable_name = "column1", allow_na = FALSE)
#' }
#' @export
check_text <- function(dataset, variable_name, spec_type, allow_na = FALSE){
    if(length(unique(allow_na))!=1){
        stop(sprintf("in check_text non-uniform allow_na %s", collapse_commas(allow_na)));
    }

    messages <- tibble::tibble(check_name=character(0), pass=logical(0), message=character(0), row_numbers=character(0));
    na_ii <- is.na(dataset[[variable_name]]);
    #print(allow_na);
    if(!allow_na){
        messages <- base::rbind(messages, check_na(dataset, variable_name));
    }
    check_name <- sprintf("Column %s contains only text values.", variable_name);
    messages
}


#' Check if values in a column can be parsed as floats
#'
#' @param dataset A data frame containing the data to be checked.
#' @param variable_name The name of the variable/column to check.
#' @param format An optional specification of the format that the float values should conform to.
#' @param allow_na Logical flag indicating whether NA values are allowed. Defaults to FALSE.
#' 
#' @return A tibble that records the name of the check, whether it passed, a message summarizing the check results, and the row numbers of any values that failed the check.
#' 
#' @examples 
#' \dontrun{
#' check_float(dataset = df, variable_name = "column1", format = NA, allow_na = FALSE)
#' }
#' @export
check_float <- function(dataset, variable_name, format=NA, allow_na=FALSE){
    if(length(unique(allow_na))!=1){
        stop(sprintf("in check_float non-uniform allow_na %s", collapse_commas(allow_na)));
    }
    messages <- tibble::tibble(check_name=character(0), pass=logical(0), message=character(0), row_numbers=character(0));
    na_ii <- is.na(dataset[[variable_name]]);
    if(!allow_na){
        messages <- base::rbind(messages, check_na(dataset, variable_name));
    }
    not_na_rows <- dataset %>% dplyr::filter(!na_ii);
    format <- unique(format);
    if(length(format)>1){
        stop("Multiple formats specified during check float", format);
    }
    int_ii <- if(is.null(format)|is.na(format)){
                  addendum <- "";
                  parses_as_float(not_na_rows[[unparsed_column_name(variable_name)]]);
              } else {
                  addendum <- sprintf(" formatted like %s.",format);
                  parses_as_formatted_float(not_na_rows[[unparsed_column_name(variable_name)]], format);
              }
    check_name <- sprintf("Column %s contains only floating point numbers%s.", variable_name, addendum);
    add_to_tibble(messages,
                  if(sum(!int_ii)==0){
                      tibble::tibble(check_name=check_name,
                             pass=TRUE,
                             message=sprintf("All values were floats%s.",addendum),
                             row_numbers=NA);
                  } else {
                      not_int_rows <- not_na_rows %>% dplyr::filter(!int_ii) %>% dplyr::pull(index__);
                      bad_count <- length(not_int_rows);
                      tibble::tibble(check_name=check_name,
                             pass=FALSE,
                             message=sprintf("%d values failed to parse as floats%s.", bad_count, addendum),
                             row_numbers=format_row_numbers(not_int_rows))
                  });                  
}

#' Check if values in a column can be parsed as ISO8601 durations
#'
#' @param dataset A data frame containing the data to be checked.
#' @param variable_name The name of the variable/column to check.
#' @param format A specification of the format that the duration values should conform to. Must be "ISO8601".
#' @param allow_na Logical flag indicating whether NA values are allowed. Defaults to FALSE.
#' 
#' @return A tibble that records the name of the check, whether it passed, a message summarizing the check results, and the row numbers of any values that failed the check.
#' 
#' @examples 
#' \dontrun{
#' check_iso8601_duration(dataset = df, variable_name = "column1", format = "ISO8601", allow_na = FALSE)
#' }
#' @export
check_iso8601_duration <- function(dataset, variable_name, format=NULL, allow_na=FALSE){
    if(format!="ISO8601") stop("All dates and durations need to have the ISO8601 format specifier in the spec.");    
    messages <- tibble::tibble(check_name=character(0), pass=logical(0), message=character(0), row_numbers=character(0));
    na_ii <- is.na(dataset[[variable_name]]);
    if(!allow_na){
        messages <- base::rbind(messages, check_na(dataset, variable_name));
    }
    not_na_rows <- dataset %>% dplyr::filter(!na_ii);
    duration_ii <- parses_as_iso8601_duration(not_na_rows[[variable_name]]);
    check_name <- sprintf("Column %s contains only ISO8601 durations.", variable_name);
    add_to_tibble(messages,
                  if(sum(!duration_ii)==0){
                      tibble::tibble(check_name=check_name,
                             pass=TRUE,
                             message=sprintf("All values were durations."),
                             row_numbers=NA);
                  } else {
                      not_duration_rows <- not_na_rows %>% dplyr::filter(!duration_ii) %>% dplyr::pull(index__);
                      bad_count <- length(not_duration_rows);
                      tibble::tibble(check_name=check_name,
                             pass=FALSE,
                             message=sprintf("%d values failed to parse as ISO8601 durations.", bad_count),
                             row_numbers=format_row_numbers(not_duration_rows))
                  });

}


#' Check if values in a column can be parsed as ISO8601 dates
#'
#' @param dataset A data frame containing the data to be checked.
#' @param variable_name The name of the variable/column to check.
#' @param format A specification of the format that the date values should conform to. Must be "ISO8601".
#' @param allow_na Logical flag indicating whether NA values are allowed. Defaults to FALSE.
#' 
#' @return A tibble that records the name of the check, whether it passed, a message summarizing the check results, and the row numbers of any values that failed the check.
#' 
#' @examples 
#' \dontrun{
#' check_iso8601_date(dataset = df, variable_name = "column1", format = "ISO8601", allow_na = FALSE)
#' }
#' @export
check_iso8601_date <- function(dataset, variable_name, format=NULL, allow_na=FALSE){
    if(format!="ISO8601") stop("All dates and durations need to have the ISO8601 format specifier in the spec.");    
    messages <- tibble::tibble(check_name=character(0), pass=logical(0), message=character(0), row_numbers=character(0));
    na_ii <- is.na(dataset[[variable_name]]);
    if(!allow_na){
        messages <- base::rbind(messages, check_na(dataset, variable_name));
    }
    not_na_rows <- dataset %>% dplyr::filter(!na_ii);
    date_ii <- parses_as_iso8601_date(not_na_rows[[variable_name]]);
    check_name <- sprintf("Column %s contains only ISO8601 dates.", variable_name);
    add_to_tibble(messages,
                  if(sum(!date_ii)==0){
                      tibble::tibble(check_name=check_name,
                             pass=TRUE,
                             message=sprintf("All values were dates."),
                             row_numbers=NA);
                  } else {
                      not_date_rows <- not_na_rows %>% dplyr::filter(!date_ii) %>% dplyr::pull(index__);
                      bad_count <- length(not_date_rows);
                      tibble::tibble(check_name=check_name,
                             pass=FALSE,
                             message=sprintf("%d values failed to parse as ISO8601 dates.", bad_count),
                             row_numbers=format_row_numbers(not_date_rows))
                  });

}

#' Check the type of values in a column based on provided specification
#'
#' @param dataset A data frame containing the data to be checked.
#' @param variable_name The name of the variable/column to check.
#' @param spec_type The expected type of the values in the column. Must be one of "integer", "float", "text", "date", "duration".
#' @param format An optional specification of the format that the values should conform to.
#' @param allow_na Logical flag indicating whether NA values are allowed. Defaults to FALSE.
#' 
#' @return A tibble that records the name of the check, whether it passed, a message summarizing the check results, and the row numbers of any values that failed the check.
#' 
#' @examples 
#' \dontrun{
#' check_type(dataset = df, variable_name = "column1", spec_type = "integer", format = NULL, allow_na = FALSE)
#' }
#' @export
check_type <- function(dataset, variable_name, spec_type, format=NULL, allow_na=FALSE){
    if(length(unique(allow_na))!=1){
        stop(sprintf("in check_type non-uniform allow_na %s", collapse_commas(allow_na)));
    }
    allow_na <- if(unique(allow_na)=="Yes") { T } else { F };
    if(length(unique(spec_type))!=1){
        stop(sprintf("in check_type non-uniform spec_type %s", collapse_commas(spec_type)));
    }
    spec_type <- unique(spec_type);

    #print(spec_type);
    switch(spec_type,
           "integer" = check_integer(dataset, variable_name, format=format, allow_na=allow_na),
           "float" = check_float(dataset, variable_name, format=format, allow_na=allow_na),
           "text" = check_text(dataset, variable_name, allow_na=allow_na),
           "ISO8601_date" = check_iso8601_date(dataset, variable_name, format=format, allow_na=allow_na),
           "ISO8601_duration" = check_iso8601_duration(dataset, variable_name, format=format, allow_na=allow_na),
           "date" = check_iso8601_date(dataset, variable_name, format=format, allow_na=allow_na),
           "duration" = check_iso8601_duration(dataset, variable_name, format=format, allow_na=allow_na),
           stop(sprintf("Failed to find a checkable type in check_type (%s).", spec_type))
    )
}

#' Check compliance of QSEVLINT column in a dataset with spec
#'
#' This function checks whether the 'QSEVLINT' values in a dataset are consistent with their corresponding
#' 'QSTESTCD' values, as specified in a combined spec.
#' 
#' @param dataset A data frame that includes columns 'QSEVLINT' and 'QSTESTCD'.
#' @param combined_spec A data frame that includes the specifications for the columns, including 
#' 'codelist_terms' and 'evaluation_interval'. Rows where 'evaluation_interval' is NA are ignored.
#' 
#' @return A tibble containing the name of the check, whether it passed, a message indicating the result,
#' and the row numbers in the original dataset that failed the check (if any).
#' 
#' @examples
#' spec <- tibble::tribble(
#'   ~codelist_terms, ~evaluation_interval,
#'   "QSEVLINT_1",         "1",
#'   "QSEVLINT_2",         "2",
#'   "QSEVLINT_3",         NA
#' )
#'
#' data <- tibble::tribble(
#'   ~QSEVLINT, ~QSTESTCD,
#'   "QSEVLINT_1", "1",
#'   "QSEVLINT_2", "2",
#'   "QSEVLINT_3", "3"
#' )
#'
#' check_qsevlint(data, spec)
#'
#' @export
check_qsevlint <- function(dataset, combined_spec) {
    
  # Select out the codelist_terms and evaluation_interval columns from the spec,
  # and drop any rows where evaluation_interval is NA
  spec_subset <- combined_spec %>% 
    dplyr::select(codelist, codelist_terms, evaluation_interval) %>% 
    dplyr::filter(!is.na(evaluation_interval))

    df <- dataset %>%
        inner_join(spec_subset, by=c("QSTESTCD"="codelist_terms")) %>%
        filter(QSEVLINT!=evaluation_interval);
    
    indices <- df %>%
        filter(QSEVLINT!=evaluation_interval) %>% pull(index__);

    if(length(indices) == 0){
        tibble(check_name="QSEVLINT must match its QSTESTCD.",pass=T, message="All matched.", row_numbers=NA);
    } else {
        tibble(check_name="QSEVLINT must match its QSTESTCD.",pass=F, message=sprintf("%d rows failed to match spec.",length(indices)), row_numbers=format_row_numbers(indices));
    } 
}


#' Find pairs of variables with names differing only by the last character being "C" or "N"
#'
#' @param names A character vector of variable/column names.
#' 
#' @return A list of character vectors, each of length 2, representing pairs of variable names where the names are identical except for the last character, which is "C" in one name and "N" in the other.
#' 
#' @examples 
#' \dontrun{
#' find_name_pairs(names = c("variable1C", "variable1N", "variable2", "variable3C"))
#' }
#' @export
find_name_pairs <- function(names) {
    # Extract base names by removing the last character
    base_names <- substr(names, 1, nchar(names) - 1)
    
    # Find base names that appear twice
    dup_base_names <- base_names[duplicated(base_names)]
    
    # Find pairs of names that match the duplicated base names
    name_pairs <- lapply(dup_base_names, function(base_name) {
        pair <- names[base_names == base_name]
        return(pair)
    })
    
    return(name_pairs)
}

#' Validate a dataset against a provided specification.
#'
#' The `validate_dataset` function reads a dataset and validates its structure and content based on the rules 
#' specified in the `combined_specs` data frame. It checks the dataset for compliance with the specification
#' information such as the variable type and if a variable is required to be mandatory, among other things.
#'
#' @param filename A string specifying the path of the file containing the dataset to be validated.
#' @param combined_specs A data frame containing the specifications against which to validate the dataset.
#'   This should include the columns: "dataset", "variable", "where_clause", "where_clause_variable",
#'   "where_clause_value", "value_level_codelist", "value_level_data_type", "value_level_mandatory",
#'   "value_level_format", "codelist", "codelist_terms", "format".
#'
#' @return None. The function will stop and throw an error if the dataset fails to validate against the specifications.
#'   During the process, it prints out debug information about the validation process, which can be useful for 
#'   diagnosing the cause of a validation failure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Suppose we have a dataset in "data.csv" and specifications in the `specs` data frame
#'   validate_dataset("data.csv", specs)
#' }
validate_dataset <- function(filename, combined_specs){
    data <- reval::reval_read_data(filename);
    domain <- data$DOMAIN[[1]];
    domain_spec <- combined_specs %>% dplyr::filter(combined_specs[["dataset"]]==domain);
    if(nrow(domain_spec) == 0) {
      stop(sprintf("Couldn't find any specification information for the domain %s", domain));
    }
    variables <- domain_spec %>%
        dplyr::select(variable) %>%
        dplyr::distinct() %>%
        dplyr::pull(variable);
    variables
    messages <- tibble::tibble();
    warnings <- tibble::tibble();
    for(v in variables){
        reval::debug_print(sprintf("%s %s", domain, v));        
        reval::debug_print(sprintf("%s %s", v, get_variable_type(domain, v, combined_specs)));
        var_type <- get_variable_type(domain, v, combined_specs)
        if(length(unique(var_type))!=1){
            stop(sprintf("in validate_dataset we found multiple types for a given variable. %s %s", v, collapse_commas(var_type)));
        }
        var_type <- unique(var_type);

        messages <- rbind(messages, switch(var_type,
               QSEVLINT = check_qsevlint(data, combined_specs),
               WHERECLAUSE = check_where_clause(data, combined_specs, domain, v),
               CODELIST = {
                   mandatory <- get_mandatory(domain, v, combined_specs);
                   codelist_terms <- get_codelist_terms(v, combined_specs);
                   if(length(codelist_terms)==0 | (length(codelist_terms)==1 & is.na(unique(codelist_terms)[[1]]))) {
                       stop(sprintf("Variable %s's type was detected as codelist but codelist terms were NA or not found.", v));
                   }
                   check_codelist(data, v, codelist_terms, mandatory);
               },
               integer = {
                   mandatory <- get_mandatory(domain, v, combined_specs);
                   format <- get_format(domain, v, combined_specs);
                   check_type(data, v, var_type, format, mandatory)
               },
               float = {
                   mandatory <- get_mandatory(domain, v, combined_specs);
                   format <- get_format(domain, v, combined_specs);
                   check_type(data, v, var_type, format, mandatory)
               },
               text = {
                   mandatory <- get_mandatory(domain, v, combined_specs);
                   format <- get_format(domain, v, combined_specs);
                   check_type(data, v, var_type, format, mandatory)
               },
               date = {
                   mandatory <- get_mandatory(domain, v, combined_specs);
                   format <- get_format(domain, v, combined_specs);
                   check_type(data, v, "ISO8601_date", format, mandatory)
               },
               duration = {
                   mandatory <- get_mandatory(domain, v, combined_specs);
                   format <- get_format(domain, v, combined_specs);
                   check_type(data, v, "ISO8601_duration", format, mandatory)
               },
               ISO8601 = {
                   mandatory <- get_mandatory(domain, v, combined_specs);
                   format <- get_format(domain, v, combined_specs);
                   switch(format,
                          date = check_type(data, v, "ISO8601_date", format, mandatory),
                          duration = check_type(data, v, "ISO8601_duration", format, mandatory),
                          stop("Unrecognized format for ISO8601 type: ", format)
                          )
               },
               stop(sprintf("Unrecognized variable type: %s for %s", var_type, v))));
    }
    messages
}

#' Internal Helper Function for Fixing Formats
#'
#' This function is used internally by fix_format() function. It is not intended to be used directly by users.
#'
#' @param format A character string representing the format to be fixed.
#'
#' @return A character string with the fixed format.
#'
#' @keywords internal
fix_format_helper <- function(format){
    if(is.na(format)) return(format);
    
    if(stringr::str_detect(format, "\\.")){
        parts <- stringr::str_split(format,"\\.",simplify=TRUE);
        if(length(parts)==1) {
            format
        } else {
            a <- parts[[1]];
            b <- sprintf("0.%s",parts[[2]]) %>%
                as.numeric() %>%
                `*`(10) %>%
                round() %>%
                as.integer() %>%
                as.character();
            paste0(a,".",b);
        }        
    } else {
        format;
    }
}

#' Fix Numeric Format Specifications
#'
#' This function fixes numeric format specifications. Specifically, it is designed to deal with issues 
#' that arise due to floating-point precision when serializing and deserializing numbers. The function takes
#' a vector of format specifications as input. If a specification includes a decimal, the decimal part is 
#' rounded to the nearest integer. Specifications without a decimal are returned as is.
#'
#' @param format A vector of character strings representing the formats to be fixed.
#'
#' @return A vector of character strings with the fixed formats.
#'
#' @export
fix_format <- function(format){
    sapply(format, fix_format_helper, simplify=TRUE) %>% base::unname();
}

#' Load and combine a set of dataset specifications.
#'
#' @param spec_directory A string specifying the directory in which the Excel and ODS files are located.
#' @param sheet_names A character vector specifying the names of the sheets to be read from the files.
#' 
#' @return A named list of data frames. The names correspond to the sheet names, and the data frames contain the data read from the corresponding sheets of the files.
#' 
#' @examples 
#' \dontrun{
#' create_list_of_df(spec_directory = "path/to/directory", sheet_names = c("Sheet1", "Sheet2"))
#' }
#' @export
load_and_combine_specs <- function(spec_directory, sheet_names=c("Study", "Datasets", "Variables", "ValueLevel", "WhereClauses", "Codelists", "Dictionaries", "Methods", "Comments", "Documents")) {
    # List all Excel and ODS files in the specified directory
    file_paths <- list.files(path=spec_directory, pattern = "\\.(xlsx|ods)$", full.names = TRUE)
    
    # Initialize list to store data frames
    list_of_df <- list()   
    
    # Loop over each sheet
    for(sheet in sheet_names){
        # Initialize temporary list to store data from each file
        temp_list <- list()
        
        # Loop over each file
        for(file in file_paths){
            # Determine the file type and read the file accordingly
            if(grepl("\\.xlsx$", file)){
                read_fun <- readxl::read_excel
            } else if(grepl("\\.ods$", file)){
                read_fun <- readODS::read_ods
            } else {
                message(paste("Unsupported file type:", file))
                next
            }

            # Try reading the sheet and store the result in a variable
            temp_df <- tryCatch({
                df <- read_fun(file, sheet = sheet)
                # Convert all columns to character type and convert column names to lowercase
                df <- df %>%
                    dplyr::mutate(dplyr::across(everything(), as.character)) %>%
                    dplyr::rename_with(function(cns){
                        cns %>% stringr::str_to_lower() %>%
                            stringr::str_replace_all("[ ]+","_") %>%
                            stringr::str_replace_all("[_]+$","") %>%
                            stringr::str_replace_all("^[_]+","");
                    });
                df
            }, error = function(e){
                message(paste("Skipped sheet:", sheet, "in file:", file))
                NULL
            })

            # If there was no error, add the data frame to the list
            if(!is.null(temp_df)){
                temp_list[[file]] <- temp_df
            }
        }

        # Bind all data frames in temp_list
        combined_df <- dplyr::bind_rows(temp_list) %>% janitor::remove_empty(which="rows")

        # Add the combined data frame to the list
        list_of_df[[sheet]] <- combined_df %>% tibble::as_tibble();
    }

    return(list_of_df)
}



#' Merge specification files
#'
#' This function merges multiple specification files from a directory
#' into a single data frame for each sheet and the combines the result
#' into a format convenient for validation.
#'
#' @param spec_directory String. The directory where the specification files are located.
#' @param sheet_names Character vector. The names of the sheets to be merged.
#'
#' @return A combined and digested data frame with all the information from the specs required to check the covered data domains.
#' 
#' @export
merge_spec_files <- function(spec_directory, sheet_names=c("Study", "Datasets", "Variables", "ValueLevel", "WhereClauses", "Codelists", "Dictionaries", "Methods", "Comments", "Documents")) {
                                        # List all Excel and ODS files in the specified directory
    file_paths <- list.files(path=spec_directory, pattern = "\\.(xlsx|ods)$", full.names = TRUE)
    
                                        # Initialize list to store data frames
    
    list_of_df <- load_and_combine_specs(spec_directory, sheet_names);

    list_of_df$Variables %>%
        dplyr::transmute(dataset, variable, data_type, variable_codelist=codelist, variable_format=format, variable_mandatory=mandatory) %>%
        dplyr::left_join(list_of_df$ValueLevel %>%
                         dplyr::transmute(dataset,
                                          variable,
                                          where_clause,
                                          value_level_description=description,
                                          value_level_data_type=data_type,
                                          value_level_format=format,
                                          value_level_codelist=codelist,
                                          value_level_mandatory=mandatory),
                         by=c("dataset","variable")) %>%
        dplyr::left_join(list_of_df$WhereClauses %>%
                         dplyr::transmute(where_clause=id,
                                          where_clause_dataset=dataset,
                                          where_clause_variable=variable,
                                          comparator,
                                          where_clause_value=stringr::str_replace_all(value, "[ ]*", "") %>% stringr::str_split(",")) %>%
                         tidyr::unnest(cols=where_clause_value),
                         by="where_clause") %>%
        dplyr::mutate(codelist=dplyr::coalesce(value_level_codelist, variable_codelist)) %>%
        dplyr::left_join(list_of_df$Codelists %>%
                         dplyr::rename(codelist=id) %>% 
                         dplyr::group_by(codelist) %>%
                         dplyr::arrange(as.numeric(order)) %>%
                         dplyr::summarize(codelist_data_type=data_type[[1]],
                                          codelist_terms=paste(term, collapse="::"),
                                          codelist_order=paste(order, collapse='::')),
                         by="codelist") %>%
        dplyr::mutate(dataset=stringr::str_to_upper(dataset)) %>%
        dplyr::mutate(dataset=stringr::str_sub(dataset, 1, 2)) %>% dplyr::distinct() %>%
        dplyr::mutate(format=fix_format(dplyr::coalesce(value_level_format, variable_format))) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(codelist_order=str_split(codelist_order,'::'),
                      codelist_terms=str_split(codelist_terms,'::')) %>%
        tidyr::unnest(cols=c("codelist_order","codelist_terms")) %>%
        dplyr::left_join(list_of_df$Codelists %>%
                         select(id,term,evaluation_interval) %>%
                         filter(!is.na(evaluation_interval)) %>%
                         distinct() %>%
                         rename(codelist=id,
                                codelist_terms=term),
                         by=c("codelist","codelist_terms"));
}

