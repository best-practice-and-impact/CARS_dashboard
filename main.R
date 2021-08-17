source("R/ingest.R")
source("R/data_cleaning.R")

departments <- remove_low_counts(ingest_department_data())

rmarkdown::render("rmarkdown/index.rmd", output_file = "../html/index.html")


dep_counts <- table(departments)