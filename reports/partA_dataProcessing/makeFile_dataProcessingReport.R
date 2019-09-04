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

makeReport <- function(){
  # default = html
  rmdFile <- paste0(repoParams$repoLoc, "/reports/partA_dataProcessing/dataProcessingReport.Rmd")
  rmarkdown::render(input = rmdFile,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".html")
  )
}

makeWordReport <- function(){
  # reuse last .md for speed will fail is no .md
  mdFile <- paste0(repoParams$repoLoc, "/reports/partA_dataProcessing/dataProcessingReport.knit.md")
  if(file.exists(mdFile)){
    rmarkdown::render(input = mdFile,
                    output_format = "word_document2",
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/partA_dataProcessingReport_v", version, ".docx")
                    )
  }
}

# Local parameters ----
version <- "0.1"

# data ----
impdPath <- paste0(repoParams$GreenGridData, "gridSpy/1min/data/imputed/") # imputed total load
hhdPath <- paste0(repoParams$GreenGridData, "gridSpy/halfHour/data/") # use half-hourly data with imputed total load

#> yaml ----
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Processing (Part A) Report v", version)
authors <- "Ben Anderson"

# --- Code ---

# this is where we would use drake
impfilesDT <- getFileList(impdPath, pattern = ".csv.gz")

# > get the imputed load file list ----
hhfilesDT <- getFileList(hhdPath, pattern = ".csv.gz")

# > get the halfhourly file list ----
filesDT <- getFileList(hhdPath, pattern = ".csv.gz")

# > get power data  ----
origHHDataDT <- getPowerData(hhfilesDT)
hhPowerDataDT <- origHHDataDT[, r_dateTimeHalfHour := lubridate::as_datetime(r_dateTimeHalfHour, # stored as UTC
                                                        tz = "Pacific/Auckland")] # so we can extract within NZ dateTime

imputedLoadF <- impfilesDT[!(all.files %like% "rf_") & # not a household file
                             all.files %like% "v1.1", # latest version
                           fullPath]

impDataDT <- data.table::fread(imputedLoadF)

# > get household data  ----
hhDataDT <- data.table::fread(paste0(repoParams$GreenGridData, "survey/ggHouseholdAttributesSafe.csv.gz"))

# > run report ----
makeReport()
#makeWordReport()