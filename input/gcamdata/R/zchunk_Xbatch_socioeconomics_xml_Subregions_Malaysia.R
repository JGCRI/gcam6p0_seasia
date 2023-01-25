# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_breakout_Xbatch_socioeconomics_xml_Subregions_Malaysia
#'
#' Construct XML data structure for \code{socioeconomics_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_USA.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_USA.xml} (gcamusa XML).
module_breakout_Xbatch_socioeconomics_xml_Subregions_Malaysia <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X201.Pop_Subregions_Malaysia",
             "X201.BaseGDP_Subregions_Malaysia",
             "X201.LaborForceFillout_Subregions_Malaysia",
             "X201.LaborProductivity_Subregions_Malaysia"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_Subregions_Malaysia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X201.Pop_Subregions_Malaysia <- get_data(all_data, "X201.Pop_Subregions_Malaysia")
    X201.BaseGDP_Subregions_Malaysia <- get_data(all_data, "X201.BaseGDP_Subregions_Malaysia")
    X201.LaborForceFillout_Subregions_Malaysia <- get_data(all_data, "X201.LaborForceFillout_Subregions_Malaysia")
    X201.LaborProductivity_Subregions_Malaysia <- get_data(all_data, "X201.LaborProductivity_Subregions_Malaysia")

    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_Subregions_Malaysia.xml") %>%
      add_xml_data(X201.Pop_Subregions_Malaysia, "Pop") %>%
      add_xml_data(X201.BaseGDP_Subregions_Malaysia, "BaseGDP") %>%
      add_xml_data(X201.LaborForceFillout_Subregions_Malaysia, "LaborForceFillout") %>%
      add_xml_data(X201.LaborProductivity_Subregions_Malaysia, "LaborProductivity") %>%
      add_precursors("X201.Pop_Subregions_Malaysia",
                     "X201.BaseGDP_Subregions_Malaysia",
                     "X201.LaborForceFillout_Subregions_Malaysia",
                     "X201.LaborProductivity_Subregions_Malaysia") ->
      socioeconomics_Subregions_Malaysia.xml

    return_data(socioeconomics_Subregions_Malaysia.xml)
  } else {
    stop("Unknown command")
  }
}
