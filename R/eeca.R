
#' Code the circuits
#'
#' \code{labelEECACircuits} expects a data.table with a `circuit` column and returns a data.table
#'  with the circuit string split into circuitLabel (for easy string matching) and circuitID.
#'
#' @param dt the data table
#' @param loadVar the label for the total load 'circuit'. Default = `imputedTotalDemand_circuitsToSum_v1.1`
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#'
labelEECACircuits <- function(dt, loadVar = "imputedTotalDemand_circuitsToSum_v1.1"){
  # takes a data.table, splits `circuit` and categorises the label
  # split the circuit label to leave just the string and the id (we need the string)
  dt <- dt[, c("circuitLabel","circuitID") := tstrsplit(circuit,"$", fixed = TRUE)]
  
  # these match exactly to circuit label strings by matching to the string that came before the $
  dt <- dt[, eecaCircuit := ifelse(circuitLabel == "Heat Pump" |
                                     circuitLabel == "Theatre Heat Pump" |
                                     circuitLabel == "Upstairs Heat Pumps" |
                                     circuitLabel == "Heating",
                                   "Heat Pump or Heating",
                                   NA) # could be any other circuit including the incomer
           ]
  
  # %like% matches if the string is in the circuit label (i.e. circuit label could contain other words)
  # Be careful with these
  dt <- dt[, eecaCircuit := ifelse( circuitLabel == "Oven" |
                                      circuitLabel == "Range" |
                                      circuitLabel == "Wall Oven" |
                                      circuitLabel %like% "Oven &" |
                                      circuitLabel %like% "Oven,", # if the circuit = Oven only
                                    "Oven", eecaCircuit)] # Hob is not included
  dt <- dt[, eecaCircuit := ifelse(circuitLabel %like% "Hot water",
                                   "Hot water", eecaCircuit)]
  dt <- dt[, eecaCircuit := ifelse(circuitLabel %like% "Hot Water",
                                   "Hot water", eecaCircuit)]
  dt <- dt[, eecaCircuit := ifelse(circuitLabel %like% "Lighting" |
                                     circuitLabel %like% "Lights",
                                   "Lighting", eecaCircuit)]
  dt <- dt[, eecaCircuit := ifelse(circuitLabel %like% loadVar, #loadVar is a parameter to the function so we can change it
                                   "Calculated_Total", eecaCircuit)]
  return(dt)
}