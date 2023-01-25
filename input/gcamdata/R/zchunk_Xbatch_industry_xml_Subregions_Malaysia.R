# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_industry_xml_Subregions_Malaysia
#'
#' Construct XML data structure for \code{industry_Subregions_Malaysia}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_Subregions_Malaysia}. The corresponding file in the
#' original data system was \code{batch_industry_Subregions_Malaysia} (energy XML).
module_energy_Xbatch_industry_xml_Subregions_Malaysia<- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X232.DeleteFinalDemand_ind_Subregions_Malaysia",
             "X232.DeleteSupplysector_ind_Subregions_Malaysia",
             "X232.Supplysector_ind_Subregions_Malaysia",
             "X232.FinalEnergyKeyword_ind_Subregions_Malaysia",
             "X232.SubsectorLogit_ind_Subregions_Malaysia",
             "X232.SubsectorShrwtFllt_ind_Subregions_Malaysia",
             "X232.SubsectorInterp_ind_Subregions_Malaysia",
             "X232.FuelPrefElast_indenergy_Subregions_Malaysia",
             "X232.StubTech_ind_Subregions_Malaysia",
             "X232.StubTechInterp_ind_Subregions_Malaysia",
             "X232.StubTechCalInput_indenergy_Subregions_Malaysia",
             "X232.StubTechCalInput_indfeed_Subregions_Malaysia",
             "X232.StubTechProd_industry_Subregions_Malaysia",
             "X232.StubTechCoef_industry_Subregions_Malaysia",
             "X232.PerCapitaBased_ind_Subregions_Malaysia",
             "X232.IncomeElasticity_ind_gssp2_Subregions_Malaysia",
             "X232.PriceElasticity_ind_Subregions_Malaysia",
             "X232.BaseService_ind_Subregions_Malaysia",
             "X232.UnlimitRsrc_Subregions_Malaysia",
             "X232.UnlimitRsrcPrice_Subregions_Malaysia",
             "X232.Supplysector_urb_indproc_Subregions_Malaysia",
             "X232.SubsectorLogit_urb_ind_Subregions_Malaysia",
             "X232.SubsectorShrwtFllt_urb_ind_Subregions_Malaysia",
             "X232.SubsectorInterp_urb_ind_Subregions_Malaysia",
             "X232.StubTech_urb_ind_Subregions_Malaysia",
             "X232.StubTechCoef_indproc_Subregions_Malaysia",
             "X232.MAC_indproc_Subregions_Malaysia",
             "X232.fgas_all_units_ind_Subregions_Malaysia",
             "X232.nonghg_max_reduction_ind_Subregions_Malaysia",
             "X232.nonghg_steepness_ind_Subregions_Malaysia",
             "X232.hfc_future_ind_Subregions_Malaysia",
             "X232.MAC_higwp_ind_Subregions_Malaysia",
             "X232.nonco2_max_reduction_indproc_Subregions_Malaysia",
             "X232.nonco2_steepness_indproc_Subregions_Malaysia",
             "X232.pol_emissions_ind_Subregions_Malaysia",
             "X232.ghg_emissions_ind_Subregions_Malaysia",
             "X232.StubTechMarket_ind_Subregions_Malaysia",
             "X232.StubTechSecMarket_ind_Subregions_Malaysia",
             "X232.nonco2_indproc_Subregions_Malaysia",
             "X232.hfc_all_indproc_Subregions_Malaysia",
             "X232.pfc_all_indproc_Subregions_Malaysia"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_Subregions_Malaysia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X232.DeleteFinalDemand_ind_Subregions_Malaysia<- get_data(all_data, "X232.DeleteFinalDemand_ind_Subregions_Malaysia")
    X232.DeleteSupplysector_ind_Subregions_Malaysia<- get_data(all_data, "X232.DeleteSupplysector_ind_Subregions_Malaysia")
    X232.Supplysector_ind_Subregions_Malaysia<- get_data(all_data, "X232.Supplysector_ind_Subregions_Malaysia")
    X232.FinalEnergyKeyword_ind_Subregions_Malaysia<- get_data(all_data, "X232.FinalEnergyKeyword_ind_Subregions_Malaysia")
    X232.SubsectorLogit_ind_Subregions_Malaysia<- get_data(all_data, "X232.SubsectorLogit_ind_Subregions_Malaysia")
    X232.SubsectorShrwtFllt_ind_Subregions_Malaysia<- get_data(all_data, "X232.SubsectorShrwtFllt_ind_Subregions_Malaysia")
    X232.SubsectorInterp_ind_Subregions_Malaysia<- get_data(all_data, "X232.SubsectorInterp_ind_Subregions_Malaysia")
    X232.FuelPrefElast_indenergy_Subregions_Malaysia<- get_data(all_data, "X232.FuelPrefElast_indenergy_Subregions_Malaysia")
    X232.StubTech_ind_Subregions_Malaysia<- get_data(all_data, "X232.StubTech_ind_Subregions_Malaysia")
    X232.StubTechInterp_ind_Subregions_Malaysia<- get_data(all_data, "X232.StubTechInterp_ind_Subregions_Malaysia")
    X232.StubTechCalInput_indenergy_Subregions_Malaysia<- get_data(all_data, "X232.StubTechCalInput_indenergy_Subregions_Malaysia")
    X232.StubTechCalInput_indfeed_Subregions_Malaysia<- get_data(all_data, "X232.StubTechCalInput_indfeed_Subregions_Malaysia")
    X232.StubTechProd_industry_Subregions_Malaysia<- get_data(all_data, "X232.StubTechProd_industry_Subregions_Malaysia")
    X232.StubTechCoef_industry_Subregions_Malaysia<- get_data(all_data, "X232.StubTechCoef_industry_Subregions_Malaysia")
    X232.PerCapitaBased_ind_Subregions_Malaysia<- get_data(all_data, "X232.PerCapitaBased_ind_Subregions_Malaysia")
    X232.IncomeElasticity_ind_gssp2_Subregions_Malaysia<- get_data(all_data, "X232.IncomeElasticity_ind_gssp2_Subregions_Malaysia")
    X232.PriceElasticity_ind_Subregions_Malaysia<- get_data(all_data, "X232.PriceElasticity_ind_Subregions_Malaysia")
    X232.BaseService_ind_Subregions_Malaysia<- get_data(all_data, "X232.BaseService_ind_Subregions_Malaysia")
    X232.UnlimitRsrc_Subregions_Malaysia<- get_data(all_data, "X232.UnlimitRsrc_Subregions_Malaysia")
    X232.UnlimitRsrcPrice_Subregions_Malaysia<- get_data(all_data, "X232.UnlimitRsrcPrice_Subregions_Malaysia")
    X232.Supplysector_urb_indproc_Subregions_Malaysia<- get_data(all_data, "X232.Supplysector_urb_indproc_Subregions_Malaysia")
    X232.SubsectorLogit_urb_ind_Subregions_Malaysia<- get_data(all_data, "X232.SubsectorLogit_urb_ind_Subregions_Malaysia")
    X232.SubsectorShrwtFllt_urb_ind_Subregions_Malaysia<- get_data(all_data, "X232.SubsectorShrwtFllt_urb_ind_Subregions_Malaysia")
    X232.SubsectorInterp_urb_ind_Subregions_Malaysia<- get_data(all_data, "X232.SubsectorInterp_urb_ind_Subregions_Malaysia")
    X232.StubTech_urb_ind_Subregions_Malaysia<- get_data(all_data, "X232.StubTech_urb_ind_Subregions_Malaysia")
    X232.StubTechCoef_indproc_Subregions_Malaysia<- get_data(all_data, "X232.StubTechCoef_indproc_Subregions_Malaysia")
    X232.MAC_indproc_Subregions_Malaysia<- get_data(all_data, "X232.MAC_indproc_Subregions_Malaysia")
    X232.fgas_all_units_ind_Subregions_Malaysia<- get_data(all_data, "X232.fgas_all_units_ind_Subregions_Malaysia")
    X232.nonghg_max_reduction_ind_Subregions_Malaysia<- get_data(all_data, "X232.nonghg_max_reduction_ind_Subregions_Malaysia")
    X232.nonghg_steepness_ind_Subregions_Malaysia<- get_data(all_data, "X232.nonghg_steepness_ind_Subregions_Malaysia")
    X232.hfc_future_ind_Subregions_Malaysia<- get_data(all_data, "X232.hfc_future_ind_Subregions_Malaysia")
    X232.MAC_higwp_ind_Subregions_Malaysia <- get_data(all_data, "X232.MAC_higwp_ind_Subregions_Malaysia")
    X232.nonco2_max_reduction_indproc_Subregions_Malaysia<- get_data(all_data, "X232.nonco2_max_reduction_indproc_Subregions_Malaysia")
    X232.nonco2_steepness_indproc_Subregions_Malaysia<- get_data(all_data, "X232.nonco2_steepness_indproc_Subregions_Malaysia")
    X232.pol_emissions_ind_Subregions_Malaysia<- get_data(all_data, "X232.pol_emissions_ind_Subregions_Malaysia")
    X232.ghg_emissions_ind_Subregions_Malaysia<- get_data(all_data, "X232.ghg_emissions_ind_Subregions_Malaysia")
    X232.StubTechMarket_ind_Subregions_Malaysia<- get_data(all_data, "X232.StubTechMarket_ind_Subregions_Malaysia")
    X232.StubTechSecMarket_ind_Subregions_Malaysia<- get_data(all_data, "X232.StubTechSecMarket_ind_Subregions_Malaysia")
    X232.nonco2_indproc_Subregions_Malaysia<- get_data(all_data, "X232.nonco2_indproc_Subregions_Malaysia")
    X232.hfc_all_indproc_Subregions_Malaysia <- get_data(all_data, "X232.hfc_all_indproc_Subregions_Malaysia")
    X232.pfc_all_indproc_Subregions_Malaysia <- get_data(all_data, "X232.pfc_all_indproc_Subregions_Malaysia")

    # ===================================================

    # Produce outputs
    create_xml("industry_Subregions_Malaysia.xml") %>%
      add_xml_data(X232.DeleteFinalDemand_ind_Subregions_Malaysia, "DeleteFinalDemand") %>%
      add_xml_data(X232.DeleteSupplysector_ind_Subregions_Malaysia, "DeleteSupplysector") %>%
      add_logit_tables_xml(X232.Supplysector_ind_Subregions_Malaysia, "Supplysector") %>%
      add_xml_data(X232.FinalEnergyKeyword_ind_Subregions_Malaysia, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(X232.SubsectorLogit_ind_Subregions_Malaysia, "SubsectorLogit") %>%
      add_xml_data(X232.SubsectorShrwtFllt_ind_Subregions_Malaysia, "SubsectorShrwtFllt") %>%
      add_xml_data(X232.SubsectorInterp_ind_Subregions_Malaysia, "SubsectorInterp") %>%
      add_xml_data(X232.FuelPrefElast_indenergy_Subregions_Malaysia, "FuelPrefElast") %>%
      add_xml_data(X232.StubTech_ind_Subregions_Malaysia, "StubTech") %>%
      add_xml_data(X232.StubTechInterp_ind_Subregions_Malaysia, "StubTechInterp") %>%
      add_xml_data(X232.StubTechCalInput_indenergy_Subregions_Malaysia, "StubTechCalInput") %>%
      add_xml_data(X232.StubTechCalInput_indfeed_Subregions_Malaysia, "StubTechCalInput") %>%
      add_xml_data(X232.StubTechProd_industry_Subregions_Malaysia, "StubTechProd") %>%
      add_xml_data(X232.StubTechCoef_industry_Subregions_Malaysia, "StubTechCoef") %>%
      add_xml_data(X232.PerCapitaBased_ind_Subregions_Malaysia, "PerCapitaBased") %>%
      add_xml_data(X232.IncomeElasticity_ind_gssp2_Subregions_Malaysia, "IncomeElasticity") %>%
      add_xml_data(X232.PriceElasticity_ind_Subregions_Malaysia, "PriceElasticity") %>%
      add_xml_data(X232.BaseService_ind_Subregions_Malaysia, "BaseService") %>%
      add_xml_data(X232.UnlimitRsrc_Subregions_Malaysia, "UnlimitRsrc") %>%
      add_xml_data(X232.UnlimitRsrcPrice_Subregions_Malaysia, "UnlimitRsrcPrice") %>%
      add_logit_tables_xml(X232.Supplysector_urb_indproc_Subregions_Malaysia, "Supplysector") %>%
      add_logit_tables_xml(X232.SubsectorLogit_urb_ind_Subregions_Malaysia, "SubsectorLogit") %>%
      add_xml_data(X232.SubsectorShrwtFllt_urb_ind_Subregions_Malaysia, "SubsectorShrwtFllt") %>%
      add_xml_data(X232.SubsectorInterp_urb_ind_Subregions_Malaysia, "SubsectorInterp") %>%
      add_xml_data(X232.StubTech_urb_ind_Subregions_Malaysia, "StubTech") %>%
      add_xml_data(X232.StubTechCoef_indproc_Subregions_Malaysia, "StubTechCoef") %>%
      add_xml_data(X232.MAC_indproc_Subregions_Malaysia, "MAC") %>%
      add_xml_data(X232.fgas_all_units_ind_Subregions_Malaysia, "StubTechEmissUnits") %>%
      add_xml_data(X232.nonghg_max_reduction_ind_Subregions_Malaysia, "GDPCtrlMax") %>%
      add_xml_data(X232.nonghg_steepness_ind_Subregions_Malaysia, "GDPCtrlSteep") %>%
      add_xml_data(X232.hfc_future_ind_Subregions_Malaysia, "OutputEmissCoeff") %>%
      add_xml_data(X232.MAC_higwp_ind_Subregions_Malaysia, "MAC") %>%
      add_xml_data(X232.nonco2_max_reduction_indproc_Subregions_Malaysia, "GDPCtrlMax") %>%
      add_xml_data(X232.nonco2_steepness_indproc_Subregions_Malaysia, "GDPCtrlSteep") %>%
      add_xml_data(X232.pol_emissions_ind_Subregions_Malaysia, "InputEmissions") %>%
      add_xml_data(X232.ghg_emissions_ind_Subregions_Malaysia, "InputEmissions") %>%
      add_xml_data(X232.StubTechMarket_ind_Subregions_Malaysia, "StubTechMarket") %>%
      add_xml_data(X232.StubTechSecMarket_ind_Subregions_Malaysia, "StubTechSecMarket") %>%
      add_xml_data(X232.nonco2_indproc_Subregions_Malaysia, "StbTechOutputEmissions") %>%
      add_xml_data(X232.hfc_all_indproc_Subregions_Malaysia, "StbTechOutputEmissions") %>%
      add_xml_data(X232.pfc_all_indproc_Subregions_Malaysia, "StbTechOutputEmissions") %>%
      add_precursors("X232.DeleteFinalDemand_ind_Subregions_Malaysia",
                     "X232.DeleteSupplysector_ind_Subregions_Malaysia",
                     "X232.Supplysector_ind_Subregions_Malaysia",
                     "X232.FinalEnergyKeyword_ind_Subregions_Malaysia",
                     "X232.SubsectorLogit_ind_Subregions_Malaysia",
                     "X232.SubsectorShrwtFllt_ind_Subregions_Malaysia",
                     "X232.SubsectorInterp_ind_Subregions_Malaysia",
                     "X232.FuelPrefElast_indenergy_Subregions_Malaysia",
                     "X232.StubTech_ind_Subregions_Malaysia",
                     "X232.StubTechInterp_ind_Subregions_Malaysia",
                     "X232.StubTechCalInput_indenergy_Subregions_Malaysia",
                     "X232.StubTechCalInput_indfeed_Subregions_Malaysia",
                     "X232.StubTechProd_industry_Subregions_Malaysia",
                     "X232.StubTechCoef_industry_Subregions_Malaysia",
                     "X232.PerCapitaBased_ind_Subregions_Malaysia",
                     "X232.IncomeElasticity_ind_gssp2_Subregions_Malaysia",
                     "X232.PriceElasticity_ind_Subregions_Malaysia",
                     "X232.BaseService_ind_Subregions_Malaysia",
                     "X232.UnlimitRsrc_Subregions_Malaysia",
                     "X232.UnlimitRsrcPrice_Subregions_Malaysia",
                     "X232.Supplysector_urb_indproc_Subregions_Malaysia",
                     "X232.SubsectorLogit_urb_ind_Subregions_Malaysia",
                     "X232.SubsectorShrwtFllt_urb_ind_Subregions_Malaysia",
                     "X232.SubsectorInterp_urb_ind_Subregions_Malaysia",
                     "X232.StubTech_urb_ind_Subregions_Malaysia",
                     "X232.StubTechCoef_indproc_Subregions_Malaysia",
                     "X232.MAC_indproc_Subregions_Malaysia",
                     "X232.fgas_all_units_ind_Subregions_Malaysia",
                     "X232.nonghg_max_reduction_ind_Subregions_Malaysia",
                     "X232.nonghg_steepness_ind_Subregions_Malaysia",
                     "X232.hfc_future_ind_Subregions_Malaysia",
                     "X232.MAC_higwp_ind_Subregions_Malaysia",
                     "X232.nonco2_max_reduction_indproc_Subregions_Malaysia",
                     "X232.nonco2_steepness_indproc_Subregions_Malaysia",
                     "X232.pol_emissions_ind_Subregions_Malaysia",
                     "X232.ghg_emissions_ind_Subregions_Malaysia",
                     "X232.StubTechMarket_ind_Subregions_Malaysia",
                     "X232.StubTechSecMarket_ind_Subregions_Malaysia",
                     "X232.nonco2_indproc_Subregions_Malaysia",
                     "X232.hfc_all_indproc_Subregions_Malaysia",
                     "X232.pfc_all_indproc_Subregions_Malaysia") ->
      industry_Subregions_Malaysia.xml

    return_data(industry_Subregions_Malaysia.xml)
  } else {
    stop("Unknown command")
  }
}
