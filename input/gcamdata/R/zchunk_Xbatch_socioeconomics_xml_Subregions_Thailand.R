# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_breakout_Xbatch_socioeconomics_xml_Subregions_Thailand
#'
#' Construct XML data structure for \code{socioeconomics_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_USA.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_USA.xml} (gcamusa XML).
module_breakout_Xbatch_socioeconomics_xml_Subregions_Thailand <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X201.Pop_Subregions_Thailand",
             "X201.BaseGDP_Subregions_Thailand",
             "X201.LaborForceFillout_Subregions_Thailand",
             "X201.LaborProductivity_Subregions_Thailand"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_Subregions_Thailand.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X201.Pop_Subregions_Thailand <- get_data(all_data, "X201.Pop_Subregions_Thailand")
    X201.BaseGDP_Subregions_Thailand <- get_data(all_data, "X201.BaseGDP_Subregions_Thailand")
    X201.LaborForceFillout_Subregions_Thailand <- get_data(all_data, "X201.LaborForceFillout_Subregions_Thailand")
    X201.LaborProductivity_Subregions_Thailand <- get_data(all_data, "X201.LaborProductivity_Subregions_Thailand")

    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_Subregions_Thailand.xml") %>%
      add_xml_data(X201.Pop_Subregions_Thailand, "Pop") %>%
      add_xml_data(X201.BaseGDP_Subregions_Thailand, "BaseGDP") %>%
      add_xml_data(X201.LaborForceFillout_Subregions_Thailand, "LaborForceFillout") %>%
      add_xml_data(X201.LaborProductivity_Subregions_Thailand, "LaborProductivity") %>%
      add_precursors("X201.Pop_Subregions_Thailand",
                     "X201.BaseGDP_Subregions_Thailand",
                     "X201.LaborForceFillout_Subregions_Thailand",
                     "X201.LaborProductivity_Subregions_Thailand") ->
      socioeconomics_Subregions_Thailand.xml

    return_data(socioeconomics_Subregions_Thailand.xml)
  } else {
    stop("Unknown command")
  }
}
