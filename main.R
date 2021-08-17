source("R/ingest.R")
source("R/data_cleaning.R")
source("R/plot.R")

departments <- remove_low_counts(ingest_department_data())

rmarkdown::render("rmarkdown/index.rmd", output_file = "../html/index.html", )

dep_counts <- table(departments)

gert::git_branch_checkout("main")
gert::git_add("main.R")
gert::git_commit("automating commit and push", author = "Jacob <jacob.cole@ons.gov.uk>")
gert::git_push()
