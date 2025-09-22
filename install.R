# install.R
pkgs <- c(
  "optparse", "data.table", "readr", "pdftools",
  "stringi", "stringr", "stringdist", "httr", "jsonlite", "readxl"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
cat("All set. Installed/verified packages:\n"); print(pkgs)
