# Makes the data processing report
# Saves result to /docs for github pages

# Load package ----

print(paste0("#-> Load GREENGridEECA package"))
library(GREENGridEECA) # local utilities
print(paste0("#-> Done "))

# run setup to set repo level parameters including data paths
# does this by sourcing repoParams.R
GREENGridEECA::setup() 

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

getPowerData <- function(filesDT){
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  # this is where we need drake
  # and probably more memory
  message("Loading ", nrow(filesDT), " files")
  l <- lapply(filesDT$fullPath, fread)
  dt <- rbindlist(l)
  setkey(dt, linkID, circuit, r_dateTimeHalfHour)
  return(dt)
}


makeReport <- function(f){
  # default = html
  rmarkdown::render(input = rmdFile,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".html")
  )
}

makeWordReport <- function(f){
  # reuse last .md for speed will fail is no .md
  if(file.exists(rmdFile)){
    rmarkdown::render(input = rmdFile,
                    output_format = "word_document2",
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".docx")
                    )
  } else {
    message("No such file: ", rmdFile)
  }
}

# Local parameters ----
version <- "0.95b"

# data ----
impdPath <- paste0(repoParams$GreenGridData, "gridSpy/1min/data/imputed/") # imputed total load
hhdPath <- paste0(repoParams$GreenGridData, "gridSpy/halfHour/data/") # use half-hourly data with imputed total load

#> yaml ----
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Processing (Part A) Report v", version)
authors <- "Ben Anderson"

# --- Code ---

# this is where we would use drake

# > get the single imputed load file ----
# this will include files created using different versions of circuitToSum
impfilesDT <- GREENGridEECA::getFileList(impdPath, pattern = ".csv.gz")

# imputed total load (1 minute) data
# this is in the same place as the per-household files so
# need to extract it from the list
imputedLoadF <- impfilesDT[!(all.files %like% "rf_") & # not a household file
                             all.files %like% "v1.1", # latest version of circuitToSum
                           fullPath]

impDataDT <- data.table::fread(imputedLoadF)


# > get the halfhourly files ----
halfHourlyFilesDT <- GREENGridEECA::getFileList(hhdPath, pattern = ".csv.gz")

# aggegated half hourly data
origHalfHourlyPowerDT <- getPowerData(halfHourlyFilesDT)
# note that the circuit column will tell us which version of circuitToSum was used
# in the aggregation - it is not included in the filename
halfHourlyPowerDT <- origHalfHourlyPowerDT[, r_dateTimeHalfHour := lubridate::as_datetime(r_dateTimeHalfHour, # stored as UTC
                                                        tz = "Pacific/Auckland")] # so we can extract within NZ dateTime

# > get household data  ----
hhDataDT <- data.table::fread(paste0(repoParams$GreenGridData, "survey/ggHouseholdAttributesSafe.csv.gz"))

# > run report ----
rmdFile <- paste0(repoParams$repoLoc, "/reports/partA_dataProcessing/dataProcessingReport.Rmd")
makeReport(rmdFile)
makeWordReport(rmdFile)
