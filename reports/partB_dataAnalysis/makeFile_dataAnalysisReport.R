# Makes the data analysis report
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
               "drake", # speed data loading
               "GREENGridData", # so as not to re-invent the wheel
               "here", # where are we?
               "lubridate", # fixing dates & times
               "utils" # for reading .gz files with data.table
)
GREENGridEECA::loadLibraries(localLibs)

# Local functions (if any) ----

getFileList <- function(dPath){
  # should be fast
  all.files <- list.files(path = dPath, pattern = ".csv.gz")
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  return(dt)
}

getData <- function(filesDT){
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  # this is where we need drake
  # and probably more memory
  message("Loading ", nrow(filesDT), " files")
  l <- lapply(filesDT$fullPath, fread) # uses the very fast fread to laod them to a list
  dt <- rbindlist(l) # rbind them
  return(dt)
  setkey( dt , linkID, r_dateTimeHalfHour )
}

processPowerData <- function(dt){
  #> Process power data.table ----
  
  # put all this here so it gets run once not each time the report is run
  
  powerDT <- dt # need to copy as data.table works by reference https://www.rdocumentation.org/packages/data.table/versions/1.12.2/topics/copy
  
  #DT modifications
  
  #Excluding households based on Part A report: https://cfsotago.github.io/GREENGridEECA/#part-a--data-processing
  exclude <- c("rf_14", "rf_25", "rf_26", "rf_43", "rf_46")
  powerDT <- powerDT[!(linkID %in% exclude)]
  
  # setting negative values to NA
  # to avoid PV effects & a few data errors
  powerDT <- powerDT[, meanPowerW := ifelse(meanPowerW <0, NA, meanPowerW)]
  powerDT <- powerDT[, sdPowerW := ifelse(sdPowerW <0, NA, sdPowerW)]
  powerDT <- powerDT[, minPowerW := ifelse(minPowerW <0, NA, minPowerW)]
  powerDT <- powerDT[, maxPowerW := ifelse(maxPowerW <0, NA, maxPowerW)]
  
  
  # set to NZ time from UTC
  powerDT <- powerDT[, r_dateTime_nz := lubridate::as_datetime(r_dateTimeHalfHour, 
                                                               tz = "Pacific/Auckland")] # this will be UTC unless you set this
  
  powerDT <- powerDT[, date := lubridate::date(r_dateTime_nz)]
  powerDT <- powerDT[, obsHalfHour := hms::as_hms(r_dateTime_nz)]
  #powerDT[, obsHalfHour := format(ymd_hms(r_dateTimeHalfHour), "%H:%M:%S")]
  powerDT <- powerDT[, month := lubridate::month(r_dateTime_nz)]
  powerDT <- powerDT[, year := lubridate::year(r_dateTime_nz)]
  
  #Define Winter/else
  powerDT <- powerDT[month == 12 | month == 1 | month == 2, season := "Summer"]
  powerDT <- powerDT[month == 3 | month == 4 | month == 5, season := "Autumn"]
  powerDT <- powerDT[month == 6 | month == 7 | month == 8, season := "Winter"]
  powerDT <- powerDT[month == 9 | month == 10 | month == 11, season := "Spring"]
  
  #Setting times of peak demand 
  OP1S <- hms::as.hms("00:00:00")
  OP1E <- hms::as.hms("16:30:00")
  
  PS <- hms::as.hms("17:00:00")
  PE <- hms::as.hms("21:00:00")
  
  OP2S <- hms::as.hms("21:30:00")
  OP2E <- hms::as.hms("23:30:00")
  
  powerDT <- powerDT[, peak := 0]
  powerDT <- powerDT[, peak := ifelse(obsHalfHour >= OP1S & obsHalfHour <= OP1E,
                                      "Off Peak 1",
                                      NA)]
  powerDT <- powerDT[, peak := ifelse(obsHalfHour >= OP2S & obsHalfHour <= OP2E,
                                      "Off Peak 2",
                                      peak)]
  powerDT <- powerDT[, peak := ifelse(obsHalfHour >= PS & obsHalfHour <= PE,
                                      "Peak",
                                      peak)]
  # this stops the RHS coercion errors
  
  # do this here once (slow)
  powerDT <- GREENGridEECA::labelEECACircuits(powerDT)
  
  # we will now have the following eecaCircuits:
  table(powerDT$eecaCircuit, useNA = "always")
  # NA is all non-abelled circuits and _will_ include main incomer so not safe
  # to sum. We need to create a calculated 'Other' and add it to the table in place of the NA rows
  labeledDT <- powerDT[!is.na(eecaCircuit) & 
                         !(eecaCircuit %like% "Calculated"), 
                       .(labeledMeanPowerW = sum(meanPowerW)), # all labelled eecaCircuits
                       keyby = .(linkID, r_dateTime_nz)]
  totalDT <- powerDT[eecaCircuit %like% "Calculated"] # calculated total load
  setkey(labeledDT, linkID, r_dateTime_nz)
  setkey(totalDT, linkID, r_dateTime_nz)
  dt <- totalDT[labeledDT] # merge them
  dt[, eecaCircuit := "Calculated_other"]
  dt[, circuit := "Calculated_other"]
  dt[, otherMeanPowerW := meanPowerW - labeledMeanPowerW] # calc other mean power
  # drop vars we don't need or are incorrect
  dt[, meanPowerW := otherMeanPowerW]
  dt$sdPowerW <- NA # this would have been the total load sd
  dt$minPowerW <- NA # same
  dt$maxPowerW <- NA # same
  dt$labeledMeanPowerW <- NULL # not needed
  dt$otherMeanPowerW <- NULL # same
  
  # now add 'other' to the end of powerDT having removed all the eecaCircuit = NA
  # this will make for a much smaller file
  powerDT <- rbind(dt, powerDT[!(is.na(eecaCircuit))])
  
  # amended coding to fit Carsten's .Rmd code
  powerDT[, eecaCircuitOrig := eecaCircuit]
  powerDT[, eecaCircuit := ifelse(eecaCircuitOrig == "Calculated_other",
                                  "Other", # changed all 'XX_Other' to 'Other' in Carsten's code
                                  eecaCircuit)]
  powerDT[, eecaCircuit := ifelse(eecaCircuitOrig == "Calculated_Total",
                                  "Total",
                                  eecaCircuit)]
  return(powerDT)
}

doReport <- function(){
  rmdFile <- paste0(repoParams$repoLoc, "/reports/partB_dataAnalysis/partB_v1.Rmd")
  rmarkdown::render(input = rmdFile,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/partB_dataAnalysisReport_v", version, ".html")
  )
}

# Local parameters ----

# > data paths ----
dPath <- paste0(repoParams$GreenGridData, "gridSpy/halfHour/data/") # use half-hourly data with imputed total load

# drake plan ----
plan <- drake::drake_plan(
  powerData = getData(filesDT),
  cleanPowerData = processPowerData(powerData)
)


# --- Code ----

# this is where we would use drake
# > get the file list ----
filesDT <- getFileList(dPath)

# > get data  ----
#origPowerDT <- getData(filesDT)
# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed


hhSurveyDT <- data.table::fread(paste0(repoParams$GreenGridData, 
                                       "survey/ggHouseholdAttributesSafe.csv.gz"))


# reload data from wherever drake put it
powerDT <- drake::readd(cleanPowerData)
# check
table(powerDT$eecaCircuit, powerDT$eecaCircuitOrig)

# > run report ----
version <- "2.1_final"

#> yaml ----
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Analysis (Part B) Report v", version)
authors <- "Dortans, C., Anderson, B. and Jack, M."
doReport() # uncomment to run automatically
