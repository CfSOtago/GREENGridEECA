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


makeHtmlReport <- function(f){
  # default = html
  if(file.exists(f)){
    rmarkdown::render(input = f,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".html")
  )
  } else {
    message("No such file: ", f)
  }
}

makeWordReport <- function(f){
  # reuse last .md for speed will fail is no .md
  if(file.exists(f)){
    rmarkdown::render(input = f,
                    output_format = "word_document2",
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".docx")
                    )
  } else {
    message("No such file: ", f)
  }
}

makeOdtReport <- function(f){
  # reuse last .md for speed will fail is no .md
  if(file.exists(f)){
    rmarkdown::render(input = f,
                      output_format = "odt_document2",
                      params = list(title = title,
                                    subtitle = subtitle,
                                    authors = authors),
                      output_file = paste0(repoParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".odt")
    )
  } else {
    message("No such file: ", f)
  }
}
# Local parameters ----
version <- "1.0"

# data ----
impdPath <- paste0(repoParams$GreenGridData, "/1min/data/imputed/") # imputed total load
hhdPath <- paste0(repoParams$GreenGridData, "/halfHour/data/") # use half-hourly data with imputed total load

#> yaml ----
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Processing (Part A) Report v", version)
authors <- "Ben Anderson"

# --- Code ----

# > imputed total load (1 minute) data ----
# this is in the same place as the per-household files so
# this will include files created using different versions of circuitToSum
# need to extract it from the list
impfilesDT <- GREENGridEECA::getFileList(impdPath, pattern = ".csv.gz")
imputedLoadF <- impfilesDT[!(all.files %like% "rf_") & # not a household file
                             all.files %like% "v1.1", # latest version of circuitToSum
                           fullPath]

# > half hourly pre=-aggregated data ----
halfHourlyFilesDT <- GREENGridEECA::getFileList(hhdPath, pattern = ".csv.gz")

# this is where we use drake if we can
plan <- drake::drake_plan(
  impData = data.table::fread(imputedLoadF),
  origHalfHourlyPower = getPowerData(halfHourlyFilesDT)
)

if(require(drake)){
  # we have drake
  plan # test the plan
  make(plan) # run the plan, re-loading data if needed
  # imputed total load
  impDataDT <- drake::readd(impData) # retreive from wherever drake put it
  # half hourly pre-aggregated
  origHalfHourlyPowerDT <- drake::readd(origHalfHourlyPower) # again
  halfHourlyPowerDT <- origHalfHourlyPowerDT[, r_dateTimeHalfHour := lubridate::as_datetime(r_dateTimeHalfHour, # stored as UTC
                                                                                            tz = "Pacific/Auckland")] # so we can extract within NZ dateTime
} else {
  # we don't
  # imputed total load
  impDataDT <- data.table::fread(imputedLoadF)
  # half hourly pre-aggregated
  origHalfHourlyPowerDT <- getPowerData(halfHourlyFilesDT)
  # note that the circuit column will tell us which version of circuitToSum was used
  # in the aggregation - it is not included in the filename
  halfHourlyPowerDT <- origHalfHourlyPowerDT[, r_dateTimeHalfHour := lubridate::as_datetime(r_dateTimeHalfHour, # stored as UTC
                                                                                            tz = "Pacific/Auckland")] # so we can extract within NZ dateTime
}

# > household data  ----
# fast - no need to drake
f <- path.expand(paste0(repoParams$GreenGridData, 
            "survey/ggHouseholdAttributesSafe_2019-10-20.csv.gz"))
hhDataDT <- data.table::fread(f)

# > run report ----
f <- paste0(repoParams$repoLoc, "/reports/partA_dataProcessing/dataProcessingReport.Rmd")
makeHtmlReport(f)
#makeOdtReport(f)
#makeWordReport(rmdFile) # can't seem to handle kableExtra tables
