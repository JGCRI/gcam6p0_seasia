# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_building_xml_Subregions_Malaysia
#'
#' Construct XML data structure for \code{building_Subregions_Malaysia.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_Subregions_Malaysia.xml}. The corresponding file in the
#' original data system was \code{batch_building_Subregions_Malaysia.xml} (energy XML).
module_energy_Xbatch_building_xml_Subregions_Malaysia <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X244.DeleteThermalService_bld_Subregions_Malaysia",
             "X244.DeleteConsumer_bld_Subregions_Malaysia",
             "X244.DeleteSupplysector_bld_Subregions_Malaysia",
             "X244.SubregionalShares_Subregions_Malaysia",
             "X244.PriceExp_IntGains_Subregions_Malaysia",
             "X244.Floorspace_Subregions_Malaysia",
             "X244.DemandFunction_serv_Subregions_Malaysia",
             "X244.DemandFunction_flsp_Subregions_Malaysia",
             "X244.Satiation_flsp_Subregions_Malaysia",
             "X244.SatiationAdder_Subregions_Malaysia",
             "X244.ThermalBaseService_Subregions_Malaysia",
             "X244.GenericBaseService_Subregions_Malaysia",
             "X244.ThermalServiceSatiation_Subregions_Malaysia",
             "X244.GenericServiceSatiation_Subregions_Malaysia",
             "X244.Intgains_scalar_Subregions_Malaysia",
             "X244.ShellConductance_bld_Subregions_Malaysia",
             "X244.Supplysector_bld_Subregions_Malaysia",
             "X244.FinalEnergyKeyword_bld_Subregions_Malaysia",
             "X244.SubsectorShrwtFllt_bld_Subregions_Malaysia",
             "X244.SubsectorInterp_bld_Subregions_Malaysia",
             "X244.FuelPrefElast_bld_Subregions_Malaysia",
             "X244.StubTech_bld_Subregions_Malaysia",
             "X244.StubTechEff_bld_Subregions_Malaysia",
             "X244.StubTechCalInput_bld_Subregions_Malaysia",
             "X244.SubsectorLogit_bld_Subregions_Malaysia",
             "X244.StubTechIntGainOutputRatio_Subregions_Malaysia",
             "X244.HDDCDD_constdd_no_GCM_Subregions_Malaysia",
             "X244.fgas_all_units_bld_Subregions_Malaysia",
             "X244.nonghg_max_reduction_bld_Subregions_Malaysia",
             "X244.nonghg_steepness_bld_Subregions_Malaysia",
             "X244.hfc_future_bld_Subregions_Malaysia",
             "X244.pol_emissions_bld_Subregions_Malaysia",
             "X244.ghg_emissions_bld_Subregions_Malaysia"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_Subregions_Malaysia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X244.DeleteConsumer_bld_Subregions_Malaysia<- get_data(all_data, "X244.DeleteConsumer_bld_Subregions_Malaysia")
    X244.DeleteSupplysector_bld_Subregions_Malaysia<- get_data(all_data, "X244.DeleteSupplysector_bld_Subregions_Malaysia")
    X244.SubregionalShares_Subregions_Malaysia<- get_data(all_data, "X244.SubregionalShares_Subregions_Malaysia")
    X244.PriceExp_IntGains_Subregions_Malaysia<- get_data(all_data, "X244.PriceExp_IntGains_Subregions_Malaysia")
    X244.Floorspace_Subregions_Malaysia<- get_data(all_data, "X244.Floorspace_Subregions_Malaysia")
    X244.DemandFunction_serv_Subregions_Malaysia<- get_data(all_data, "X244.DemandFunction_serv_Subregions_Malaysia")
    X244.DemandFunction_flsp_Subregions_Malaysia<- get_data(all_data, "X244.DemandFunction_flsp_Subregions_Malaysia")
    X244.Satiation_flsp_Subregions_Malaysia<- get_data(all_data, "X244.Satiation_flsp_Subregions_Malaysia")
    X244.SatiationAdder_Subregions_Malaysia<- get_data(all_data, "X244.SatiationAdder_Subregions_Malaysia")
    X244.ThermalBaseService_Subregions_Malaysia<- get_data(all_data, "X244.ThermalBaseService_Subregions_Malaysia")
    X244.GenericBaseService_Subregions_Malaysia<- get_data(all_data, "X244.GenericBaseService_Subregions_Malaysia")
    X244.ThermalServiceSatiation_Subregions_Malaysia<- get_data(all_data, "X244.ThermalServiceSatiation_Subregions_Malaysia")
    X244.GenericServiceSatiation_Subregions_Malaysia<- get_data(all_data, "X244.GenericServiceSatiation_Subregions_Malaysia")
    X244.Intgains_scalar_Subregions_Malaysia<- get_data(all_data, "X244.Intgains_scalar_Subregions_Malaysia")
    X244.ShellConductance_bld_Subregions_Malaysia<- get_data(all_data, "X244.ShellConductance_bld_Subregions_Malaysia")
    X244.Supplysector_bld_Subregions_Malaysia<- get_data(all_data, "X244.Supplysector_bld_Subregions_Malaysia")
    X244.FinalEnergyKeyword_bld_Subregions_Malaysia<- get_data(all_data, "X244.FinalEnergyKeyword_bld_Subregions_Malaysia")
    X244.SubsectorShrwtFllt_bld_Subregions_Malaysia<- get_data(all_data, "X244.SubsectorShrwtFllt_bld_Subregions_Malaysia")
    X244.SubsectorInterp_bld_Subregions_Malaysia<- get_data(all_data, "X244.SubsectorInterp_bld_Subregions_Malaysia")
    X244.FuelPrefElast_bld_Subregions_Malaysia<- get_data(all_data, "X244.FuelPrefElast_bld_Subregions_Malaysia")
    X244.StubTech_bld_Subregions_Malaysia<- get_data(all_data, "X244.StubTech_bld_Subregions_Malaysia")
    X244.StubTechEff_bld_Subregions_Malaysia<- get_data(all_data, "X244.StubTechEff_bld_Subregions_Malaysia")
    X244.StubTechCalInput_bld_Subregions_Malaysia<- get_data(all_data, "X244.StubTechCalInput_bld_Subregions_Malaysia")
    X244.SubsectorLogit_bld_Subregions_Malaysia<- get_data(all_data, "X244.SubsectorLogit_bld_Subregions_Malaysia")
    X244.StubTechIntGainOutputRatio_Subregions_Malaysia<- get_data(all_data, "X244.StubTechIntGainOutputRatio_Subregions_Malaysia")
    X244.HDDCDD_constdd_no_GCM_Subregions_Malaysia<- get_data(all_data, "X244.HDDCDD_constdd_no_GCM_Subregions_Malaysia")
    X244.fgas_all_units_bld_Subregions_Malaysia<- get_data(all_data, "X244.fgas_all_units_bld_Subregions_Malaysia")
    X244.nonghg_max_reduction_bld_Subregions_Malaysia<- get_data(all_data, "X244.nonghg_max_reduction_bld_Subregions_Malaysia")
    X244.nonghg_steepness_bld_Subregions_Malaysia<- get_data(all_data, "X244.nonghg_steepness_bld_Subregions_Malaysia")
    X244.hfc_future_bld_Subregions_Malaysia<- get_data(all_data, "X244.hfc_future_bld_Subregions_Malaysia")
    X244.pol_emissions_bld_Subregions_Malaysia<- get_data(all_data, "X244.pol_emissions_bld_Subregions_Malaysia")
    X244.ghg_emissions_bld_Subregions_Malaysia<- get_data(all_data, "X244.ghg_emissions_bld_Subregions_Malaysia")
    X244.DeleteThermalService_bld_Subregions_Malaysia<- get_data(all_data, "X244.DeleteThermalService_bld_Subregions_Malaysia")

    # ===================================================

    # Remove fgas for years not in hfc future
    X244.fgas_all_units_bld_Subregions_Malaysia %>%
      dplyr::left_join(X244.hfc_future_bld_Subregions_Malaysia %>%
                         dplyr::select(Non.CO2,year) %>%
                         unique() %>%
                         dplyr::mutate(keep=1)) %>%
      dplyr::filter(keep==1) %>%
      dplyr::select(-keep)->
      X244.fgas_all_units_bld_Subregions_Malaysia

    # Produce outputs
    create_xml("building_Subregions_Malaysia.xml") %>%
      add_xml_data(X244.DeleteConsumer_bld_Subregions_Malaysia, "DeleteConsumer") %>%
      add_xml_data(X244.DeleteSupplysector_bld_Subregions_Malaysia, "DeleteSupplysector") %>%
      add_logit_tables_xml(X244.Supplysector_bld_Subregions_Malaysia, "Supplysector") %>%
      add_logit_tables_xml(X244.SubsectorLogit_bld_Subregions_Malaysia, "SubsectorLogit") %>%
      add_xml_data(X244.SubregionalShares_Subregions_Malaysia, "SubregionalShares") %>%
    add_xml_data(X244.PriceExp_IntGains_Subregions_Malaysia, "PriceExp_IntGains") %>%
    add_xml_data(X244.Floorspace_Subregions_Malaysia, "Floorspace") %>%
    add_xml_data(X244.DemandFunction_serv_Subregions_Malaysia, "DemandFunction_serv") %>%
    add_xml_data(X244.DemandFunction_flsp_Subregions_Malaysia, "DemandFunction_flsp") %>%
    add_xml_data(X244.Satiation_flsp_Subregions_Malaysia, "Satiation_flsp") %>%
    add_xml_data(X244.SatiationAdder_Subregions_Malaysia, "SatiationAdder") %>%
    add_xml_data(X244.ThermalBaseService_Subregions_Malaysia, "ThermalBaseService") %>%
    add_xml_data(X244.GenericBaseService_Subregions_Malaysia, "GenericBaseService") %>%
    add_xml_data(X244.ThermalServiceSatiation_Subregions_Malaysia, "ThermalServiceSatiation") %>%
    add_xml_data(X244.GenericServiceSatiation_Subregions_Malaysia, "GenericServiceSatiation") %>%
    add_xml_data(X244.Intgains_scalar_Subregions_Malaysia, "Intgains_scalar") %>%
    add_xml_data(X244.ShellConductance_bld_Subregions_Malaysia, "ShellConductance") %>%
    add_xml_data(X244.FinalEnergyKeyword_bld_Subregions_Malaysia, "FinalEnergyKeyword") %>%
    add_xml_data(X244.SubsectorShrwtFllt_bld_Subregions_Malaysia, "SubsectorShrwtFllt") %>%
    add_xml_data(X244.SubsectorInterp_bld_Subregions_Malaysia, "SubsectorInterp") %>%
    add_xml_data(X244.FuelPrefElast_bld_Subregions_Malaysia, "FuelPrefElast") %>%
    add_xml_data(X244.StubTech_bld_Subregions_Malaysia, "StubTech") %>%
    add_xml_data(X244.StubTechEff_bld_Subregions_Malaysia, "StubTechEff") %>%
    add_xml_data(X244.StubTechCalInput_bld_Subregions_Malaysia, "StubTechCalInput") %>%
    add_xml_data(X244.StubTechIntGainOutputRatio_Subregions_Malaysia, "StubTechIntGainOutputRatio") %>%
    add_xml_data(X244.HDDCDD_constdd_no_GCM_Subregions_Malaysia, "HDDCDD") %>%
      add_xml_data(X244.fgas_all_units_bld_Subregions_Malaysia, "StubTechEmissUnits") %>%
      add_xml_data(X244.nonghg_max_reduction_bld_Subregions_Malaysia, "GDPCtrlMax") %>%
      add_xml_data(X244.nonghg_steepness_bld_Subregions_Malaysia, "GDPCtrlSteep") %>%
      add_xml_data(X244.hfc_future_bld_Subregions_Malaysia, "OutputEmissCoeff") %>%
      add_xml_data(X244.pol_emissions_bld_Subregions_Malaysia, "InputEmissions") %>%
      add_xml_data(X244.ghg_emissions_bld_Subregions_Malaysia, "InputEmissions") %>%
      add_precursors("X244.DeleteConsumer_bld_Subregions_Malaysia",
                     "X244.DeleteSupplysector_bld_Subregions_Malaysia",
                     "X244.SubregionalShares_Subregions_Malaysia",
                     "X244.PriceExp_IntGains_Subregions_Malaysia",
                     "X244.Floorspace_Subregions_Malaysia",
                     "X244.DemandFunction_serv_Subregions_Malaysia",
                     "X244.DemandFunction_flsp_Subregions_Malaysia",
                     "X244.Satiation_flsp_Subregions_Malaysia",
                     "X244.SatiationAdder_Subregions_Malaysia",
                     "X244.ThermalBaseService_Subregions_Malaysia",
                     "X244.GenericBaseService_Subregions_Malaysia",
                     "X244.ThermalServiceSatiation_Subregions_Malaysia",
                     "X244.GenericServiceSatiation_Subregions_Malaysia",
                     "X244.Intgains_scalar_Subregions_Malaysia",
                     "X244.ShellConductance_bld_Subregions_Malaysia",
                     "X244.Supplysector_bld_Subregions_Malaysia",
                     "X244.FinalEnergyKeyword_bld_Subregions_Malaysia",
                     "X244.SubsectorShrwtFllt_bld_Subregions_Malaysia",
                     "X244.SubsectorInterp_bld_Subregions_Malaysia",
                     "X244.FuelPrefElast_bld_Subregions_Malaysia",
                     "X244.StubTech_bld_Subregions_Malaysia",
                     "X244.StubTechEff_bld_Subregions_Malaysia",
                     "X244.StubTechCalInput_bld_Subregions_Malaysia",
                     "X244.SubsectorLogit_bld_Subregions_Malaysia",
                     "X244.StubTechIntGainOutputRatio_Subregions_Malaysia",
                     "X244.HDDCDD_constdd_no_GCM_Subregions_Malaysia",
                     "X244.fgas_all_units_bld_Subregions_Malaysia",
                     "X244.nonghg_max_reduction_bld_Subregions_Malaysia",
                     "X244.nonghg_steepness_bld_Subregions_Malaysia",
                     "X244.hfc_future_bld_Subregions_Malaysia",
                     "X244.pol_emissions_bld_Subregions_Malaysia",
                     "X244.ghg_emissions_bld_Subregions_Malaysia",
                     "X244.DeleteThermalService_bld_Subregions_Malaysia") ->
      building_Subregions_Malaysia.xml

    if(nrow(X244.DeleteThermalService_bld_Subregions_Malaysia) > 0) {
      building_Subregions_Malaysia.xml %>%
        add_xml_data(X244.DeleteThermalService_bld_Subregions_Malaysia, "DeleteThermalService") ->
        building_Subregions_Malaysia.xml
    }


    return_data(building_Subregions_Malaysia.xml)
  } else {
    stop("Unknown command")
  }
}
