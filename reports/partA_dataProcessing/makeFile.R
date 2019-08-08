# Makes the data processing report
# Saves result to /docs for github pages

# Load package ----

print(paste0("#-> Load GREENGridEECA package"))
library(GREENGridEECA) # local utilities
print(paste0("#-> Done "))

GREENGridEECA::setup() # run setup to set repo level parameters

# Load libraries needed across all .Rmd files ----
localLibs <- c("rmarkdown",
               "bookdown",
               "data.table", # data munching
               "ggplot2", # for fancy graphs
               "GREENGridData", # so as not to re-invent the wheel
               "here", # where are we?
               "lubridate", # fixing dates & times
               "kableExtra" # for fancier tables
)
GREENGridEECA::loadLibraries(localLibs)

# Local functions (if any) ----

# Local parameters ----
version <- "1.0"
repoParams$repoLoc <- here::here()
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Analysis: Data Processing Report v", version)

# --- Code ---

rmdFile <- paste0(repoParams$repoLoc, "/reports/partA_dataProcessing/dataProcessingReport.Rmd")
rmarkdown::render(input = rmdFile,
                  params = list(title = title,
                                subtitle = subtitle),
                  output_file = paste0(ggrParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".html")
)
