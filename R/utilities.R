#' Installs and loads packages
#'
#' \code{loadLibraries} checks whether the package is already installed,
#'   installing those which are not preinstalled. All the libraries are then loaded.
#'
#'   Especially useful when running on virtual machines where package installation
#'   is not persistent (Like UoS sve). It will fail if the packages need to be
#'   installed but there is no internet access.
#'
#'   NB: in R 'require' tries to load a package but throws a warning & moves on if it's not there
#'   whereas 'library' throws an error if it can't load the package. Hence 'loadLibraries'
#'   https://stackoverflow.com/questions/5595512/what-is-the-difference-between-require-and-library
#' @param ... A list of packages
#' @param repo The repository to load functions from. Defaults to "https://cran.rstudio.com"
#' @importFrom  utils install.packages
#'
#' @author Luke Blunden, \email{lsb@@soton.ac.uk} (original)
#' @author Michael Harper \email{m.harper@@soton.ac.uk} (revised version)
#' @export
#'
loadLibraries <- function(..., repo = "https://cran.rstudio.com"){

  packages <- c(...)

  # Check if package is installed
  newPackages <- packages[!(packages %in% utils::installed.packages()[,1])]

  # Install if required
  if (length(newPackages)){utils::install.packages(newPackages, dependencies = TRUE)}

  # Load packages
  sapply(packages, require, character.only = TRUE)
}

#' Gzip a file
#'
#' \code{gzipIt} gzips a file, over-writing automatically.
#'
#' @param file file to gzip
#'
#' @author Michael Harper
#' @author Ben Anderson, \email{banderson@@soton.ac.uk}
#' @export
#' @family Utilities

gzipIt <- function(file) {
  # Path of output file
  f <- path.expand(file) # just in case
  gz <- paste0(f, ".gz")
  message("Gziping ", f)
  # Gzip it
  # in case it fails (it will on windows - you will be left with a .csv file)
  try(system( paste0("gzip -f '", f,"'"))) # include ' or it breaks on spaces
  message("Gzipped ", f)
}

#' Add NZ season to a data.table
#'
#' \code{addNZSeason} returns a dt with SOUTHERN hemisphere season added.
#'
#' @param dt the data table
#' @param date the column in the dt which is a date that lubridate::month() will work on
#' @import lubridate
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#'
addNZSeason <- function(dt, date = date){
  dt <- dt[, tmpM := lubridate::month(date)] # sets 1 (Jan) - 12 (Dec). May already exist but we can't rely on it
  dt <- dt[, season := "Summer"] # easiest to set the default to be the one that bridges years
  dt <- dt[tmpM >= 3 & tmpM <= 5, season := "Autumn"]
  dt <- dt[tmpM >= 6 & tmpM <= 8 , season := "Winter"]
  dt <- dt[tmpM >= 9 & tmpM <= 11, season := "Spring"]
  # re-order to make sense
  dt <- dt[, season := factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))]
  dt$tmpM <- NULL
  return(dt)
}


#' Add peak period to a data.table
#'
#' \code{codePeakPeriod} returns a dt with peakPeriod factor added. Assumes column 
#' hms exists and is in hms form
#'
#' @param dt the data table
#' 
#' @import data.table
#' @import hms
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#'
codePeakPeriod <- function(dt){
  # > defn of peak ----
  amPeakStart <- hms::as_hms("07:00:00")
  amPeakEnd <- hms::as_hms("09:00:00")
  pmPeakStart <- hms::as_hms("17:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
  pmPeakEnd <- hms::as_hms("21:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
  
  # assumes hms exists
  dt[, peakPeriod := NA]
  dt[, peakPeriod := ifelse(hms < amPeakStart, "Early morning", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= amPeakStart & hms < amPeakEnd, "Morning peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= amPeakEnd & hms < pmPeakStart, "Day time", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= pmPeakStart & hms < pmPeakEnd, "Evening peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= pmPeakEnd, "Late evening", peakPeriod)]
  dt[, peakPeriod := forcats::fct_relevel(peakPeriod, 
                                          "Early morning",
                                          "Morning peak",
                                          "Day time",
                                          "Evening peak",
                                          "Late evening")]
  return(dt)
}

#' Gets a list of files by path and pattern
#'
#' \code{getFileList} returns the list of files in a given path which match 
#' a given pattern. Returns a data.table with 3 columns - the file name, it's 
#' full path and it's file size in Mb
#'
#' @param dPath the file path
#' @param pattern the pattern to match
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#'
#'
getFileList <- function(dPath, pattern){
  all.files <- list.files(path = dPath, pattern = pattern)
  dt <- data.table::as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  dt[, fSizeMb := repoParams$bytesToMb * file.size(fullPath)]
  return(dt)
}