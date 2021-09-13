source("R/ingest.R")
source("R/data_cleaning.R")
source("R/plot.R")

rmarkdown::render("rmarkdown/index.rmd", output_file = "../docs/index.html")

gert::git_branch_checkout("main")
gert::git_add("docs/")
gert::git_commit("Latest data", author = "Jacob <jacob.cole@ons.gov.uk>")
gert::git_push()
