#' Download , process and save monthly GXP data
#'
#' \code{refreshGXPData} gets monthly NZ Electricity Authority Grid Exit Point data 
#' from https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export and 
#' pre-processes them into monthly long form files with friendlier column names. 
#' Function saves a long form data.table.
#'
#' @param years the years to get
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' 
#' @import data.table
#' @import here
#' @import lubridate
#' @export
#' 
#' @family emi
#'
refreshGXPData <- function(years){
  # assumes months is a list of months of the form "201801" to match EA url
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") # srsly
  # > EA GXP data location ----
  rDataLoc <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export/"
  for(y in years){
    for(m in months){
      rDataF <- paste0(rDataLoc, y, m, "_Grid_export.csv")
      print(paste0("Getting, processing and cleaning ", y,m, " (", rDataF, ")"))
      dt <- getGxpData(rDataF) # not exported
      of <- paste0(repoParams$gxpData, "/EA_",y, m, "_GXP_MD.csv")
      data.table::fwrite(dt, file = of)
      gzipIt(of)
    }
  }
}

#' Download GXP data
#'
#' \code{getData} is called by refreshGXPData to get a single monthly NZ Electricity Authority Grid Exit Point data 
#' from https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export and 
#' pre-processes them into monthly long form files with friendlier column names. 
#' Function saves a long form data.table.
#'
#' @param years the years to get
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' 
#' @import curl
#' @import readr
#' 
#' @family emi
#'
getGxpData <- function(f){
  # f <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export/201907_Grid_export.csv"
  req <- curl::curl_fetch_disk(f, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
  if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
    df <- readr::read_csv(req$content)
    print("File downloaded successfully")
    dt <- cleanGXP(df) # clean up to a dt, not exported
    return(dt)
  } else {
    print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
  }
}

#' Clean GXP data
#'
#' \code{cleanGXP} is called by getGxpData. Takes a wide form dataframe and returns
#' a cleaned data.table
#'
#' @param df the data frame
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' 
#' @import data.table
#' @import hms
#' @import lubridate
#' 
#' @family emi
#'
cleanGXP <- function(df){
  # takes a df, cleans & returns a dt
  wideDT <- data.table::as.data.table(df) # make dt
  # reshape the data as it comes in a rather unhelpful form
  longDT <- melt(wideDT,
                     id.vars=c("POC","NWK_Code", "GENERATION_TYPE", "TRADER","TRADING_DATE",
                               "UNIT_MEASURE", "FLOW_DIRECTION","STATUS" ),
                     variable.name = "Time_Period", # converts TP1-48/49/50 <- beware of these ref DST!
                     value.name = "kWh" # energy 
  )
  # convert the given time periods (TP1 -> TP48, 49. 50) to hh:mm
  longDT <- longDT[, c("t","tp") := tstrsplit(Time_Period, "P")]
  longDT <- longDT[, mins := ifelse(as.numeric(tp)%%2 == 0, "30", "00")] # set to start point. 
  # TPX -> if X is even = 30 past the hour
  # So TP1 -> 00:00, TP2 -> 00:30, TP3 -> 01:00, TP4 -> 01:30 etc
  longDT <- longDT[, hours := floor((as.numeric(tp)+1)/2) - 1]
  longDT <- longDT[, strTime := paste0(hours, ":", mins, ":00")]
  longDT <- longDT[, rTime := hms::as.hms(strTime)]
  # head(dt)
  #dt <- dt[, c("t","tp","mins","hours","strTime") := NULL]  #remove these now we're happy
  longDT <- longDT[, rDate := lubridate::dmy(TRADING_DATE)] # fix the dates so R knows what they are. Would be nice if these matched the Gen data
  longDT <- longDT[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  return(longDT)
}


#' Load gxp data
#'
#' \code{loadGXPData} loads monthly NZ Electricity Authority Grid Exit Point data 
#' which has already been downloaded from https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export 
#' and and pre-processed into long form. Also requires a geo-coded look-up table 
#' which is in the repo /data/input folder. Function returns a long form data.table.
#'
#' @param files the gxp files
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' 
#' @import data.table
#' @import here
#' @import lubridate
#' @export
#' 
#' @family emi
#'
loadGXPData <- function(files){
  ## load the EA GXP data
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  
  message("Loading ", nrow(files), " GXP files")
  l <- lapply(files$fullPath, data.table::fread)
  dt <- rbindlist(l)
  # fix dates & times ----
  dt <- dt[!is.na(rTime)] # drop the TP49 & TP50
  dt[, rDateTime := lubridate::as_datetime(rDateTime)] # just to be sure
  dt[, rDateTime := lubridate::force_tz(rDateTime, tzone = "Pacific/Auckland")] # otherwise R thinks it is UTC
  dt[, date := lubridate::date(rDateTime)]
  dt[, month := lubridate::month(rDateTime)]
  dt[, day_of_week := lubridate::wday(rDateTime, label = TRUE)]
  dt[, hms := hms::as_hms(rDateTime)] # h:m:s of the observation (= the start of the half-hour)
  dt[, halfHour := hms::trunc_hms(hms, 30*60)] # truncate to previous half-hour just in case
  
  # Create factor for weekdays/weekends ----
  dt[, weekdays := "Weekdays"]
  dt[, weekdays := ifelse(day_of_week == "Sat" |
                            day_of_week == "Sun", "Weekends", weekdays)]
  
  # load the Transpower GIS table
  # Source: https://data-transpower.opendata.arcgis.com/datasets/e507953ab8934fc3a115b2e79226cbd6_0/data
  f <- paste0(here::here(), "/data/input/gxpGeolookup.csv")
  gxpGeoDT <- data.table::fread(f)
  # note there are differences:
  # gxpGeoDT$MXLOCATION == dt$POC but without the trailing 0331 - whatever this means
  # need to adjust in kWh data for matching
  dt[, MXLOCATION := substr(POC, start = 1, stop = 3)] # note this produces non-unique locations
  # so presumably some gxps share a location where they feed different networks
  # check
  uniqueN(dt$POC)
  uniqueN(dt$MXLOCATION)
  uniqueN(gxpGeoDT$MXLOCATION)
  #dt[, .(nNodes = uniqueN(node)), keyby = .(MXLOCATION)]
  setkey(dt, MXLOCATION)
  setkey(gxpGeoDT, MXLOCATION)
  message("Merging geo-coded data")
  dt <- gxpGeoDT[dt] # merge kWh to geo-coding data
  message("Done")
  return(dt)
}


