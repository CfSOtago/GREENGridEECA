# Makes the data processing report
# Saves result to /docs for github pages

# Load package ----

print(paste0("#-> Load GREENGridEECA package"))
library(GREENGridEECA) # local utilities
print(paste0("#-> Done "))

GREENGridEECA::setup() # run setup to set repo level parameters

# Load libraries needed across all .Rmd files ----
rLibs <- c("rmarkdown",
               "bookdown",
               "data.table", # data munching
               "GREENGridData", # so as not to re-invent the wheel
               "here", # where are we?
               "lubridate", # fixing dates & times
               "utils" # for reading .gz files with data.table
)
GREENGridEECA::loadLibraries(rLibs)

# Local functions (if any) ----

getFileList <- function(dPath){
  all.files <- list.files(path = dPath, pattern = ".csv.gz")
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  return(dt)
}

getPowerData <- function(filesDT){
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  # this is where we need drake
  # and probably more memory
  message("Loading ", nrow(filesDT), " files")
  l <- lapply(filesDT$fullPath, fread)
  dt <- rbindlist(l)
  dt <- dt[, r_dateTimeHalfHour := lubridate::as_datetime(r_dateTimeHalfHour, # stored as UTC
                                                     tz = "Pacific/Auckland")] # so we can extract within NZ dateTime
  setkey(dt, linkID, circuit, r_dateTimeHalfHour)
  return(dt)
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
version <- "0.1"

# data ----
dPath <- paste0(repoParams$GreenGridData, "gridSpy/halfHour/data/") # use half-hourly data with imputed total load

#> yaml ----
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Processing (Part A) Report v", version)
authors <- "Anderson, B."

# --- Code ---

# this is where we would use drake
# > get the file list ----
filesDT <- getFileList(dPath)

# > get power data  ----
powerDataDT <- getPowerData(filesDT)

# > get hh data  ----
hhDataDT <- data.table::fread(paste0(repoParams$GreenGridData, "survey/ggHouseholdAttributesSafe.csv.gz"))

# > run report ----
doReport()
