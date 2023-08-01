library(tidyverse)
## code to prepare `DATASET` dataset goes here
source("R/reval.R") # just source this file directly, since we need
                    # some package functions to process the data we
                                        # have.


min_data_set_spec <- merge_spec_files("./package-data");

usethis::use_data(min_data_set_spec, overwrite = TRUE);

qsmd_test_data <- reval_read_data("./test-data/QSMD.csv");

usethis::use_data(qsmd_test_data, overwrite = TRUE);

qsop_test_data <- reval_read_data("./test-data/QSOP.csv");

usethis::use_data(qsop_test_data, overwrite = TRUE);
