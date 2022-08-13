library(readr)
Athletes <- read_csv("Most-paid-athletes/Athletes.csv")
#Drop columns that can't be analyzed
cols_to_drop <- c( "Wikipedia Page", "dbpedia Page", "Image", "Description")
Athletes <- Athletes[, ! names(Athletes) %in% cols_to_drop, Drop = F]
