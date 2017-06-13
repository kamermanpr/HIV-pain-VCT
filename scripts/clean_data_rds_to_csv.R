############################################################
#                                                          #
#           Convert clean_data.rds to CSV format           #
#                                                          #
############################################################
# Import data
clean_data <- readr::read_rds('./data/clean_data.rds')

# Write data to CSV
readr::write_csv(clean_data,
                 path = './data/clean_data.csv')

############################################################
#                                                          #
#                         Clean-up                         #
#                                                          #
############################################################
rm(list = ls())
