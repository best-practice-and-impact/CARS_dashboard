source("R/ingest.R")
source("R/data_cleaning.R")
source("R/plot.R")

rmarkdown::render("rmarkdown/index.rmd", output_file = "../docs/index.html")
