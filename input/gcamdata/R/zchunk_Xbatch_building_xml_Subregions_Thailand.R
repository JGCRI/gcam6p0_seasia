# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_building_xml_Subregions_Thailand
#'
#' Construct XML data structure for \code{building_Subregions_Thailand.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_Subregions_Thailand.xml}. The corresponding file in the
#' original data system was \code{batch_building_Subregions_Thailand.xml} (energy XML).
module_energy_Xbatch_building_xml_Subregions_Thailand <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X244.DeleteThermalService_bld_Subregions_Thailand",
             "X244.DeleteConsumer_bld_Subregions_Thailand",
             "X244.DeleteSupplysector_bld_Subregions_Thailand",
             "X244.SubregionalShares_Subregions_Thailand",
             "X244.PriceExp_IntGains_Subregions_Thailand",
             "X244.Floorspace_Subregions_Thailand",
             "X244.DemandFunction_serv_Subregions_Thailand",
             "X244.DemandFunction_flsp_Subregions_Thailand",
             "X244.Satiation_flsp_Subregions_Thailand",
             "X244.SatiationAdder_Subregions_Thailand",
             "X244.ThermalBaseService_Subregions_Thailand",
             "X244.GenericBaseService_Subregions_Thailand",
             "X244.ThermalServiceSatiation_Subregions_Thailand",
             "X244.GenericServiceSatiation_Subregions_Thailand",
             "X244.Intgains_scalar_Subregions_Thailand",
             "X244.ShellConductance_bld_Subregions_Thailand",
             "X244.Supplysector_bld_Subregions_Thailand",
             "X244.FinalEnergyKeyword_bld_Subregions_Thailand",
             "X244.SubsectorShrwtFllt_bld_Subregions_Thailand",
             "X244.SubsectorInterp_bld_Subregions_Thailand",
             "X244.FuelPrefElast_bld_Subregions_Thailand",
             "X244.StubTech_bld_Subregions_Thailand",
             "X244.StubTechEff_bld_Subregions_Thailand",
             "X244.StubTechCalInput_bld_Subregions_Thailand",
             "X244.SubsectorLogit_bld_Subregions_Thailand",
             "X244.StubTechIntGainOutputRatio_Subregions_Thailand",
             "X244.HDDCDD_constdd_no_GCM_Subregions_Thailand",
             "X244.fgas_all_units_bld_Subregions_Thailand",
             "X244.nonghg_max_reduction_bld_Subregions_Thailand",
             "X244.nonghg_steepness_bld_Subregions_Thailand",
             "X244.hfc_future_bld_Subregions_Thailand",
             "X244.pol_emissions_bld_Subregions_Thailand",
             "X244.ghg_emissions_bld_Subregions_Thailand"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_Subregions_Thailand.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X244.DeleteConsumer_bld_Subregions_Thailand<- get_data(all_data, "X244.DeleteConsumer_bld_Subregions_Thailand")
    X244.DeleteSupplysector_bld_Subregions_Thailand<- get_data(all_data, "X244.DeleteSupplysector_bld_Subregions_Thailand")
    X244.SubregionalShares_Subregions_Thailand<- get_data(all_data, "X244.SubregionalShares_Subregions_Thailand")
    X244.PriceExp_IntGains_Subregions_Thailand<- get_data(all_data, "X244.PriceExp_IntGains_Subregions_Thailand")
    X244.Floorspace_Subregions_Thailand<- get_data(all_data, "X244.Floorspace_Subregions_Thailand")
    X244.DemandFunction_serv_Subregions_Thailand<- get_data(all_data, "X244.DemandFunction_serv_Subregions_Thailand")
    X244.DemandFunction_flsp_Subregions_Thailand<- get_data(all_data, "X244.DemandFunction_flsp_Subregions_Thailand")
    X244.Satiation_flsp_Subregions_Thailand<- get_data(all_data, "X244.Satiation_flsp_Subregions_Thailand")
    X244.SatiationAdder_Subregions_Thailand<- get_data(all_data, "X244.SatiationAdder_Subregions_Thailand")
    X244.ThermalBaseService_Subregions_Thailand<- get_data(all_data, "X244.ThermalBaseService_Subregions_Thailand")
    X244.GenericBaseService_Subregions_Thailand<- get_data(all_data, "X244.GenericBaseService_Subregions_Thailand")
    X244.ThermalServiceSatiation_Subregions_Thailand<- get_data(all_data, "X244.ThermalServiceSatiation_Subregions_Thailand")
    X244.GenericServiceSatiation_Subregions_Thailand<- get_data(all_data, "X244.GenericServiceSatiation_Subregions_Thailand")
    X244.Intgains_scalar_Subregions_Thailand<- get_data(all_data, "X244.Intgains_scalar_Subregions_Thailand")
    X244.ShellConductance_bld_Subregions_Thailand<- get_data(all_data, "X244.ShellConductance_bld_Subregions_Thailand")
    X244.Supplysector_bld_Subregions_Thailand<- get_data(all_data, "X244.Supplysector_bld_Subregions_Thailand")
    X244.FinalEnergyKeyword_bld_Subregions_Thailand<- get_data(all_data, "X244.FinalEnergyKeyword_bld_Subregions_Thailand")
    X244.SubsectorShrwtFllt_bld_Subregions_Thailand<- get_data(all_data, "X244.SubsectorShrwtFllt_bld_Subregions_Thailand")
    X244.SubsectorInterp_bld_Subregions_Thailand<- get_data(all_data, "X244.SubsectorInterp_bld_Subregions_Thailand")
    X244.FuelPrefElast_bld_Subregions_Thailand<- get_data(all_data, "X244.FuelPrefElast_bld_Subregions_Thailand")
    X244.StubTech_bld_Subregions_Thailand<- get_data(all_data, "X244.StubTech_bld_Subregions_Thailand")
    X244.StubTechEff_bld_Subregions_Thailand<- get_data(all_data, "X244.StubTechEff_bld_Subregions_Thailand")
    X244.StubTechCalInput_bld_Subregions_Thailand<- get_data(all_data, "X244.StubTechCalInput_bld_Subregions_Thailand")
    X244.SubsectorLogit_bld_Subregions_Thailand<- get_data(all_data, "X244.SubsectorLogit_bld_Subregions_Thailand")
    X244.StubTechIntGainOutputRatio_Subregions_Thailand<- get_data(all_data, "X244.StubTechIntGainOutputRatio_Subregions_Thailand")
    X244.HDDCDD_constdd_no_GCM_Subregions_Thailand<- get_data(all_data, "X244.HDDCDD_constdd_no_GCM_Subregions_Thailand")
    X244.fgas_all_units_bld_Subregions_Thailand<- get_data(all_data, "X244.fgas_all_units_bld_Subregions_Thailand")
    X244.nonghg_max_reduction_bld_Subregions_Thailand<- get_data(all_data, "X244.nonghg_max_reduction_bld_Subregions_Thailand")
    X244.nonghg_steepness_bld_Subregions_Thailand<- get_data(all_data, "X244.nonghg_steepness_bld_Subregions_Thailand")
    X244.hfc_future_bld_Subregions_Thailand<- get_data(all_data, "X244.hfc_future_bld_Subregions_Thailand")
    X244.pol_emissions_bld_Subregions_Thailand<- get_data(all_data, "X244.pol_emissions_bld_Subregions_Thailand")
    X244.ghg_emissions_bld_Subregions_Thailand<- get_data(all_data, "X244.ghg_emissions_bld_Subregions_Thailand")
    X244.DeleteThermalService_bld_Subregions_Thailand<- get_data(all_data, "X244.DeleteThermalService_bld_Subregions_Thailand")

    # ===================================================

    # Remove fgas for years not in hfc future
    X244.fgas_all_units_bld_Subregions_Thailand %>%
      dplyr::left_join(X244.hfc_future_bld_Subregions_Thailand %>%
                         dplyr::select(Non.CO2,year) %>%
                         unique() %>%
                         dplyr::mutate(keep=1)) %>%
      dplyr::filter(keep==1) %>%
      dplyr::select(-keep)->
      X244.fgas_all_units_bld_Subregions_Thailand

    # Produce outputs
    create_xml("building_Subregions_Thailand.xml") %>%
      add_xml_data(X244.DeleteConsumer_bld_Subregions_Thailand, "DeleteConsumer") %>%
      add_xml_data(X244.DeleteSupplysector_bld_Subregions_Thailand, "DeleteSupplysector") %>%
      add_logit_tables_xml(X244.Supplysector_bld_Subregions_Thailand, "Supplysector") %>%
      add_logit_tables_xml(X244.SubsectorLogit_bld_Subregions_Thailand, "SubsectorLogit") %>%
      add_xml_data(X244.SubregionalShares_Subregions_Thailand, "SubregionalShares") %>%
    add_xml_data(X244.PriceExp_IntGains_Subregions_Thailand, "PriceExp_IntGains") %>%
    add_xml_data(X244.Floorspace_Subregions_Thailand, "Floorspace") %>%
    add_xml_data(X244.DemandFunction_serv_Subregions_Thailand, "DemandFunction_serv") %>%
    add_xml_data(X244.DemandFunction_flsp_Subregions_Thailand, "DemandFunction_flsp") %>%
    add_xml_data(X244.Satiation_flsp_Subregions_Thailand, "Satiation_flsp") %>%
    add_xml_data(X244.SatiationAdder_Subregions_Thailand, "SatiationAdder") %>%
    add_xml_data(X244.ThermalBaseService_Subregions_Thailand, "ThermalBaseService") %>%
    add_xml_data(X244.GenericBaseService_Subregions_Thailand, "GenericBaseService") %>%
    add_xml_data(X244.ThermalServiceSatiation_Subregions_Thailand, "ThermalServiceSatiation") %>%
    add_xml_data(X244.GenericServiceSatiation_Subregions_Thailand, "GenericServiceSatiation") %>%
    add_xml_data(X244.Intgains_scalar_Subregions_Thailand, "Intgains_scalar") %>%
    add_xml_data(X244.ShellConductance_bld_Subregions_Thailand, "ShellConductance") %>%
    add_xml_data(X244.FinalEnergyKeyword_bld_Subregions_Thailand, "FinalEnergyKeyword") %>%
    add_xml_data(X244.SubsectorShrwtFllt_bld_Subregions_Thailand, "SubsectorShrwtFllt") %>%
    add_xml_data(X244.SubsectorInterp_bld_Subregions_Thailand, "SubsectorInterp") %>%
    add_xml_data(X244.FuelPrefElast_bld_Subregions_Thailand, "FuelPrefElast") %>%
    add_xml_data(X244.StubTech_bld_Subregions_Thailand, "StubTech") %>%
    add_xml_data(X244.StubTechEff_bld_Subregions_Thailand, "StubTechEff") %>%
    add_xml_data(X244.StubTechCalInput_bld_Subregions_Thailand, "StubTechCalInput") %>%
    add_xml_data(X244.StubTechIntGainOutputRatio_Subregions_Thailand, "StubTechIntGainOutputRatio") %>%
    add_xml_data(X244.HDDCDD_constdd_no_GCM_Subregions_Thailand, "HDDCDD") %>%
      add_xml_data(X244.fgas_all_units_bld_Subregions_Thailand, "StubTechEmissUnits") %>%
      add_xml_data(X244.nonghg_max_reduction_bld_Subregions_Thailand, "GDPCtrlMax") %>%
      add_xml_data(X244.nonghg_steepness_bld_Subregions_Thailand, "GDPCtrlSteep") %>%
      add_xml_data(X244.hfc_future_bld_Subregions_Thailand, "OutputEmissCoeff") %>%
      add_xml_data(X244.pol_emissions_bld_Subregions_Thailand, "InputEmissions") %>%
      add_xml_data(X244.ghg_emissions_bld_Subregions_Thailand, "InputEmissions") %>%
      add_precursors("X244.DeleteConsumer_bld_Subregions_Thailand",
                     "X244.DeleteSupplysector_bld_Subregions_Thailand",
                     "X244.SubregionalShares_Subregions_Thailand",
                     "X244.PriceExp_IntGains_Subregions_Thailand",
                     "X244.Floorspace_Subregions_Thailand",
                     "X244.DemandFunction_serv_Subregions_Thailand",
                     "X244.DemandFunction_flsp_Subregions_Thailand",
                     "X244.Satiation_flsp_Subregions_Thailand",
                     "X244.SatiationAdder_Subregions_Thailand",
                     "X244.ThermalBaseService_Subregions_Thailand",
                     "X244.GenericBaseService_Subregions_Thailand",
                     "X244.ThermalServiceSatiation_Subregions_Thailand",
                     "X244.GenericServiceSatiation_Subregions_Thailand",
                     "X244.Intgains_scalar_Subregions_Thailand",
                     "X244.ShellConductance_bld_Subregions_Thailand",
                     "X244.Supplysector_bld_Subregions_Thailand",
                     "X244.FinalEnergyKeyword_bld_Subregions_Thailand",
                     "X244.SubsectorShrwtFllt_bld_Subregions_Thailand",
                     "X244.SubsectorInterp_bld_Subregions_Thailand",
                     "X244.FuelPrefElast_bld_Subregions_Thailand",
                     "X244.StubTech_bld_Subregions_Thailand",
                     "X244.StubTechEff_bld_Subregions_Thailand",
                     "X244.StubTechCalInput_bld_Subregions_Thailand",
                     "X244.SubsectorLogit_bld_Subregions_Thailand",
                     "X244.StubTechIntGainOutputRatio_Subregions_Thailand",
                     "X244.HDDCDD_constdd_no_GCM_Subregions_Thailand",
                     "X244.fgas_all_units_bld_Subregions_Thailand",
                     "X244.nonghg_max_reduction_bld_Subregions_Thailand",
                     "X244.nonghg_steepness_bld_Subregions_Thailand",
                     "X244.hfc_future_bld_Subregions_Thailand",
                     "X244.pol_emissions_bld_Subregions_Thailand",
                     "X244.ghg_emissions_bld_Subregions_Thailand",
                     "X244.DeleteThermalService_bld_Subregions_Thailand") ->
      building_Subregions_Thailand.xml

    if(nrow(X244.DeleteThermalService_bld_Subregions_Thailand) > 0) {
      building_Subregions_Thailand.xml %>%
        add_xml_data(X244.DeleteThermalService_bld_Subregions_Thailand, "DeleteThermalService") ->
        building_Subregions_Thailand.xml
    }


    return_data(building_Subregions_Thailand.xml)
  } else {
    stop("Unknown command")
  }
}
