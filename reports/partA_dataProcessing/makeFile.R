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
               "drake", # to create data objects
               "GREENGridData", # so as not to re-invent the wheel
               "here", # where are we?
               "lubridate" # fixing dates & times
)
GREENGridEECA::loadLibraries(localLibs)

# Local functions (if any) ----

# Local parameters ----
version <- "1.0"
repoParams$repoLoc <- here::here()
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Analysis: Data Processing Report v", version)
authors <- "Anderson, B."

# --- Code ---

# The original data extraction & cleaning kept all data, it was only the
# UKDA submission that filtered dates.

# put all of this data in 1 data.table. This will be quite large
startDate <- lubridate::date("2010-01-01") # well before
endDate <- lubridate::date("2020-01-01") # well after
dPath <- repoParams$gridSpy
# test
dPath <- "~/greenGridData/cleanData/safe/gridSpy/1min/data/"
cleanData <- GREENGridData::loadCleanGridSpyData(path.expand(dPath),
                                          startDate,
                                          endDate)



rmdFile <- paste0(repoParams$repoLoc, "/reports/partA_dataProcessing/dataProcessingReport.Rmd")
rmarkdown::render(input = rmdFile,
                  params = list(title = title,
                                subtitle = subtitle,
                                authors = authors),
                  output_file = paste0(repoParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".html")
)
