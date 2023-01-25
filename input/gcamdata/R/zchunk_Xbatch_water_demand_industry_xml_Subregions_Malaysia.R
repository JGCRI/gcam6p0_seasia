# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_water_demand_industry_xml_Subregions_Malaysia
#'
#' Construct XML data structure for \code{water_demand_industry_Subregions_Malaysia.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_industry.xml}. The corresponding file in the
#' original data system was \code{batch_water_demand_industry.xml.R} (water XML).
module_water_batch_water_demand_industry_xml_Subregions_Malaysia <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.TechCoef",
             "X201.Pop_Subregions_Malaysia"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_industry_Subregions_Malaysia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.TechCoef <- get_data(all_data, "L232.TechCoef")
    X201.Pop_Subregions_Malaysia <- get_data(all_data, "X201.Pop_Subregions_Malaysia")

    # gcambreakout Add subregions
    parent_region <- "Malaysia"
    subregions <- X201.Pop_Subregions_Malaysia$region
    subregions <- subregions[!subregions %in% "Malaysia"]
    subregions <- subregions %>% unique(); subregions

    L232.StubTechCoef_subRegion <- data.frame()
    for(i in subregions){

      L232.StubTechCoef_subRegion_x <- L232.TechCoef %>%
        dplyr::filter(region == parent_region) %>%
        dplyr::mutate(region = i) %>%
        dplyr::rename(stub.technology = technology)

      L232.StubTechCoef_subRegion <- rbind(L232.StubTechCoef_subRegion,L232.StubTechCoef_subRegion_x)
    }

    # ===================================================

    # Produce outputs
    create_xml("water_demand_industry_Subregions_Malaysia.xml") %>%
      add_xml_data(L232.StubTechCoef_subRegion, "StubTechCoef") %>%
      add_precursors("L232.TechCoef","X201.Pop_Subregions_Malaysia") ->
      water_demand_industry_Subregions_Malaysia.xml

    return_data(water_demand_industry_Subregions_Malaysia.xml)
  } else {
    stop("Unknown command")
  }
}
