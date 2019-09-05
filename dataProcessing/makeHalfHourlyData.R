# Makes the half-hourly power aggregates using the full 1 minute W observations dataset

# Load package ----

print(paste0("#-> Load GREENGridEECA package"))
library(GREENGridEECA) # local utilities
print(paste0("#-> Done "))

GREENGridEECA::setup() # run setup to set repo level parameters

# Load libraries needed across all .Rmd files ----
localLibs <- c("data.table", # data munching
               "GREENGridData", # so as not to re-invent the wheel
               "here", # where are we?
               "lubridate", # fixing dates & times
               "utils" # for reading .gz files with data.table
)
GREENGridEECA::loadLibraries(localLibs) # load them

# Local functions (if any) ----

getFileList <- function(iPath){
  # should be fast
  all.files <- list.files(path = iPath, pattern = "^[rf_]") # list individual dwelling files
  dt <- as.data.table(all.files)
  return(dt)
}

processData <- function(filesDT){
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  # this is where we need drake
  for(f in filesDT$all.files){
    # loops over & loads per-dwelling files aggreating & saving as we go
    # for testing: f <- "rf_01_all_1min_data_withImputedTotal_circuitsToSum_v1.1.csv.gz"
    s <- strsplit(f, "_all") # get the file name
    hh <- unlist(s)[1]
    message("Processing: ", hh)
    iFile <- paste0(iPath, f) # load it
    message("Loading ", iFile)
    dt <- data.table::fread(iFile) # make a data.table for speed https://github.com/Rdatatable/data.table/wiki
    dt[, r_dateTime := lubridate::as_datetime(r_dateTime, tz = "Pacific/Auckland")] # this will be UTC unless you set this
    dt[, r_dateTimeHalfHour := lubridate::floor_date(r_dateTime, "30 minutes")] # create half-hour dateTime
    hhDT <- dt[, .(meanPowerW = mean(powerW), # aggregate stats - add more if  you wish
                   nObs = .N,
                   sdPowerW = sd(powerW),
                   minPowerW = min(powerW),
                   maxPowerW = max(powerW)),
               keyby = .(linkID, circuit, r_dateTimeHalfHour)]
    ofile <- paste0(oPath, hh, "_allObs_halfHourly.csv") # set per dwelling aggregate data file name
    print(paste0("Writing ", ofile))
    data.table::fwrite(hhDT, file = ofile) # save aggregate
    message("gzipping...")
    GREENGridEECA::gzipIt(ofile) # gzip it to save space
    message("Done: ", hh)
  }
}

# Local parameters ----
version <- "1.0"
repoParams$repoLoc <- here::here() # where are we?

# data paths
iPath <- paste0(repoParams$GreenGridData, "gridSpy/1min/data/imputed/") # use data with pre-imputed total load
oPath <- paste0(repoParams$GreenGridData,"gridSpy/halfHour/data/") # where to save it

# --- Code ---

filesDT <- getFileList(iPath) # get the input file list

# testing: filesDT <- filesDT[all.files %like% "rf_01"]
processData(filesDT) # do the processing

message("Done!")
