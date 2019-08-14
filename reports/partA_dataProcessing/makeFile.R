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
               "GREENGridData", # so as not to re-invent the wheel
               "here", # where are we?
               "lubridate", # fixing dates & times
               "utils" # for reading .gz files with data.table
)
GREENGridEECA::loadLibraries(localLibs)

# Local functions (if any) ----

getFileList <- function(){
  # should be fast
  dPath <- repoParams$gridSpy
  # test
  dPath <- "~/greenGridData/cleanData/safe/gridSpy/1min/data/imputed/"
  all.files <- list.files(path = dPath, pattern = ".csv.gz")
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  return(dt)
}

getData <- function(filesDT){
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  # this is where we need drake
  # and probably more memory
  l <- lapply(filesDT$fullPath, fread)
  dt <- rbindlist( l )
  #setkey( dt , ID, date )
}

doReport <- function(){
  rmdFile <- paste0(repoParams$repoLoc, "/reports/partA_dataProcessing/dataProcessingReport.Rmd")
  rmarkdown::render(input = rmdFile,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".html")
  )
}

# Local parameters ----
version <- "1.0"
repoParams$repoLoc <- here::here()

#> yaml ----
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Analysis: Data Processing Report v", version)
authors <- "Anderson, B."

#> dates ----
startDate <- lubridate::date("2010-01-01") # well before
endDate <- lubridate::date("2020-01-01") # well after

# --- Code ---

filesDT <- getFileList()

# remove rf_46
filesDT <- filesDT[!(fullPath %like% "rf_46")]

#allDataDT <- getData(filesDT) # breaks memory

# doReport()
