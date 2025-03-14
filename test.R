library(jsonlite)
source('faasr_check_workflow_cycle.R')

# RScript test.R [JSON file path]

args <- commandArgs(trailingOnly = TRUE)
json_payload <- paste(readLines(args[1]), collapse = "\n")
json_data <- fromJSON(json_payload)
message(json_payload)
pre <- list()
pre <- faasr_check_workflow_cycle(json_data)