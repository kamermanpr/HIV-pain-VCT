############################################################
#                                                          #
#    Generate a basic codebook for the cleaned dataset     #
#                                                          #
############################################################
library(magrittr)

# Import data
clean_data <- readr::read_rds('./data/clean_data.rds')

# Generate a dataframe of column names
column_names <- as.list(names(clean_data))
names(column_names) <- names(clean_data)
column_names <- column_names %>%
    purrr::map_df(paste) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename('column_name' = V1) %>%
    tibble::rownames_to_column()

# Generate a dataframe of column classes
column_class <- clean_data %>%
    purrr::map(class) %>%
    purrr::map(~ paste(.x, collapse = ' ')) %>%
    purrr::map(stringr::str_to_title) %>%
    purrr::map_df(paste) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename('data_class' = V1) %>%
    tibble::rownames_to_column()

# Generate a dataframe of column factor levels
column_levels <- clean_data %>%
    purrr::map(levels) %>%
    purrr::map(~ paste(.x, collapse = ' < ')) %>%
    purrr::map_df(paste) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename('ordered_factors' = V1) %>%
    tibble::rownames_to_column()

# Combine dataframes
clean_data_codebook <- column_names %>%
    dplyr::left_join(column_class) %>%
    dplyr::left_join(column_levels) %>%
    dplyr::select(-rowname)

# Write to CSV
readr::write_csv(clean_data_codebook,
                 path = './data/clean_data_codebook.csv')

# Cleanup
rm(list = ls())
