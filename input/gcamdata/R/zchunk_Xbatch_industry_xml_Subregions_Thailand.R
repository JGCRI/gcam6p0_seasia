# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_industry_xml_Subregions_Thailand
#'
#' Construct XML data structure for \code{industry_Subregions_Thailand}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_Subregions_Thailand}. The corresponding file in the
#' original data system was \code{batch_industry_Subregions_Thailand} (energy XML).
module_energy_Xbatch_industry_xml_Subregions_Thailand<- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X232.DeleteFinalDemand_ind_Subregions_Thailand",
             "X232.DeleteSupplysector_ind_Subregions_Thailand",
             "X232.Supplysector_ind_Subregions_Thailand",
             "X232.FinalEnergyKeyword_ind_Subregions_Thailand",
             "X232.SubsectorLogit_ind_Subregions_Thailand",
             "X232.SubsectorShrwtFllt_ind_Subregions_Thailand",
             "X232.SubsectorInterp_ind_Subregions_Thailand",
             "X232.FuelPrefElast_indenergy_Subregions_Thailand",
             "X232.StubTech_ind_Subregions_Thailand",
             "X232.StubTechInterp_ind_Subregions_Thailand",
             "X232.StubTechCalInput_indenergy_Subregions_Thailand",
             "X232.StubTechCalInput_indfeed_Subregions_Thailand",
             "X232.StubTechProd_industry_Subregions_Thailand",
             "X232.StubTechCoef_industry_Subregions_Thailand",
             "X232.PerCapitaBased_ind_Subregions_Thailand",
             "X232.IncomeElasticity_ind_gssp2_Subregions_Thailand",
             "X232.PriceElasticity_ind_Subregions_Thailand",
             "X232.BaseService_ind_Subregions_Thailand",
             "X232.UnlimitRsrc_Subregions_Thailand",
             "X232.UnlimitRsrcPrice_Subregions_Thailand",
             "X232.Supplysector_urb_indproc_Subregions_Thailand",
             "X232.SubsectorLogit_urb_ind_Subregions_Thailand",
             "X232.SubsectorShrwtFllt_urb_ind_Subregions_Thailand",
             "X232.SubsectorInterp_urb_ind_Subregions_Thailand",
             "X232.StubTech_urb_ind_Subregions_Thailand",
             "X232.StubTechCoef_indproc_Subregions_Thailand",
             "X232.MAC_indproc_Subregions_Thailand",
             "X232.fgas_all_units_ind_Subregions_Thailand",
             "X232.nonghg_max_reduction_ind_Subregions_Thailand",
             "X232.nonghg_steepness_ind_Subregions_Thailand",
             "X232.hfc_future_ind_Subregions_Thailand",
             "X232.MAC_higwp_ind_Subregions_Thailand",
             "X232.nonco2_max_reduction_indproc_Subregions_Thailand",
             "X232.nonco2_steepness_indproc_Subregions_Thailand",
             "X232.pol_emissions_ind_Subregions_Thailand",
             "X232.ghg_emissions_ind_Subregions_Thailand",
             "X232.StubTechMarket_ind_Subregions_Thailand",
             "X232.StubTechSecMarket_ind_Subregions_Thailand",
             "X232.nonco2_indproc_Subregions_Thailand",
             "X232.hfc_all_indproc_Subregions_Thailand",
             "X232.pfc_all_indproc_Subregions_Thailand"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_Subregions_Thailand.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X232.DeleteFinalDemand_ind_Subregions_Thailand<- get_data(all_data, "X232.DeleteFinalDemand_ind_Subregions_Thailand")
    X232.DeleteSupplysector_ind_Subregions_Thailand<- get_data(all_data, "X232.DeleteSupplysector_ind_Subregions_Thailand")
    X232.Supplysector_ind_Subregions_Thailand<- get_data(all_data, "X232.Supplysector_ind_Subregions_Thailand")
    X232.FinalEnergyKeyword_ind_Subregions_Thailand<- get_data(all_data, "X232.FinalEnergyKeyword_ind_Subregions_Thailand")
    X232.SubsectorLogit_ind_Subregions_Thailand<- get_data(all_data, "X232.SubsectorLogit_ind_Subregions_Thailand")
    X232.SubsectorShrwtFllt_ind_Subregions_Thailand<- get_data(all_data, "X232.SubsectorShrwtFllt_ind_Subregions_Thailand")
    X232.SubsectorInterp_ind_Subregions_Thailand<- get_data(all_data, "X232.SubsectorInterp_ind_Subregions_Thailand")
    X232.FuelPrefElast_indenergy_Subregions_Thailand<- get_data(all_data, "X232.FuelPrefElast_indenergy_Subregions_Thailand")
    X232.StubTech_ind_Subregions_Thailand<- get_data(all_data, "X232.StubTech_ind_Subregions_Thailand")
    X232.StubTechInterp_ind_Subregions_Thailand<- get_data(all_data, "X232.StubTechInterp_ind_Subregions_Thailand")
    X232.StubTechCalInput_indenergy_Subregions_Thailand<- get_data(all_data, "X232.StubTechCalInput_indenergy_Subregions_Thailand")
    X232.StubTechCalInput_indfeed_Subregions_Thailand<- get_data(all_data, "X232.StubTechCalInput_indfeed_Subregions_Thailand")
    X232.StubTechProd_industry_Subregions_Thailand<- get_data(all_data, "X232.StubTechProd_industry_Subregions_Thailand")
    X232.StubTechCoef_industry_Subregions_Thailand<- get_data(all_data, "X232.StubTechCoef_industry_Subregions_Thailand")
    X232.PerCapitaBased_ind_Subregions_Thailand<- get_data(all_data, "X232.PerCapitaBased_ind_Subregions_Thailand")
    X232.IncomeElasticity_ind_gssp2_Subregions_Thailand<- get_data(all_data, "X232.IncomeElasticity_ind_gssp2_Subregions_Thailand")
    X232.PriceElasticity_ind_Subregions_Thailand<- get_data(all_data, "X232.PriceElasticity_ind_Subregions_Thailand")
    X232.BaseService_ind_Subregions_Thailand<- get_data(all_data, "X232.BaseService_ind_Subregions_Thailand")
    X232.UnlimitRsrc_Subregions_Thailand<- get_data(all_data, "X232.UnlimitRsrc_Subregions_Thailand")
    X232.UnlimitRsrcPrice_Subregions_Thailand<- get_data(all_data, "X232.UnlimitRsrcPrice_Subregions_Thailand")
    X232.Supplysector_urb_indproc_Subregions_Thailand<- get_data(all_data, "X232.Supplysector_urb_indproc_Subregions_Thailand")
    X232.SubsectorLogit_urb_ind_Subregions_Thailand<- get_data(all_data, "X232.SubsectorLogit_urb_ind_Subregions_Thailand")
    X232.SubsectorShrwtFllt_urb_ind_Subregions_Thailand<- get_data(all_data, "X232.SubsectorShrwtFllt_urb_ind_Subregions_Thailand")
    X232.SubsectorInterp_urb_ind_Subregions_Thailand<- get_data(all_data, "X232.SubsectorInterp_urb_ind_Subregions_Thailand")
    X232.StubTech_urb_ind_Subregions_Thailand<- get_data(all_data, "X232.StubTech_urb_ind_Subregions_Thailand")
    X232.StubTechCoef_indproc_Subregions_Thailand<- get_data(all_data, "X232.StubTechCoef_indproc_Subregions_Thailand")
    X232.MAC_indproc_Subregions_Thailand<- get_data(all_data, "X232.MAC_indproc_Subregions_Thailand")
    X232.fgas_all_units_ind_Subregions_Thailand<- get_data(all_data, "X232.fgas_all_units_ind_Subregions_Thailand")
    X232.nonghg_max_reduction_ind_Subregions_Thailand<- get_data(all_data, "X232.nonghg_max_reduction_ind_Subregions_Thailand")
    X232.nonghg_steepness_ind_Subregions_Thailand<- get_data(all_data, "X232.nonghg_steepness_ind_Subregions_Thailand")
    X232.hfc_future_ind_Subregions_Thailand<- get_data(all_data, "X232.hfc_future_ind_Subregions_Thailand")
    X232.MAC_higwp_ind_Subregions_Thailand <- get_data(all_data, "X232.MAC_higwp_ind_Subregions_Thailand")
    X232.nonco2_max_reduction_indproc_Subregions_Thailand<- get_data(all_data, "X232.nonco2_max_reduction_indproc_Subregions_Thailand")
    X232.nonco2_steepness_indproc_Subregions_Thailand<- get_data(all_data, "X232.nonco2_steepness_indproc_Subregions_Thailand")
    X232.pol_emissions_ind_Subregions_Thailand<- get_data(all_data, "X232.pol_emissions_ind_Subregions_Thailand")
    X232.ghg_emissions_ind_Subregions_Thailand<- get_data(all_data, "X232.ghg_emissions_ind_Subregions_Thailand")
    X232.StubTechMarket_ind_Subregions_Thailand<- get_data(all_data, "X232.StubTechMarket_ind_Subregions_Thailand")
    X232.StubTechSecMarket_ind_Subregions_Thailand<- get_data(all_data, "X232.StubTechSecMarket_ind_Subregions_Thailand")
    X232.nonco2_indproc_Subregions_Thailand<- get_data(all_data, "X232.nonco2_indproc_Subregions_Thailand")
    X232.hfc_all_indproc_Subregions_Thailand <- get_data(all_data, "X232.hfc_all_indproc_Subregions_Thailand")
    X232.pfc_all_indproc_Subregions_Thailand <- get_data(all_data, "X232.pfc_all_indproc_Subregions_Thailand")

    # ===================================================

    # Produce outputs
    create_xml("industry_Subregions_Thailand.xml") %>%
      add_xml_data(X232.DeleteFinalDemand_ind_Subregions_Thailand, "DeleteFinalDemand") %>%
      add_xml_data(X232.DeleteSupplysector_ind_Subregions_Thailand, "DeleteSupplysector") %>%
      add_logit_tables_xml(X232.Supplysector_ind_Subregions_Thailand, "Supplysector") %>%
      add_xml_data(X232.FinalEnergyKeyword_ind_Subregions_Thailand, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(X232.SubsectorLogit_ind_Subregions_Thailand, "SubsectorLogit") %>%
      add_xml_data(X232.SubsectorShrwtFllt_ind_Subregions_Thailand, "SubsectorShrwtFllt") %>%
      add_xml_data(X232.SubsectorInterp_ind_Subregions_Thailand, "SubsectorInterp") %>%
      add_xml_data(X232.FuelPrefElast_indenergy_Subregions_Thailand, "FuelPrefElast") %>%
      add_xml_data(X232.StubTech_ind_Subregions_Thailand, "StubTech") %>%
      add_xml_data(X232.StubTechInterp_ind_Subregions_Thailand, "StubTechInterp") %>%
      add_xml_data(X232.StubTechCalInput_indenergy_Subregions_Thailand, "StubTechCalInput") %>%
      add_xml_data(X232.StubTechCalInput_indfeed_Subregions_Thailand, "StubTechCalInput") %>%
      add_xml_data(X232.StubTechProd_industry_Subregions_Thailand, "StubTechProd") %>%
      add_xml_data(X232.StubTechCoef_industry_Subregions_Thailand, "StubTechCoef") %>%
      add_xml_data(X232.PerCapitaBased_ind_Subregions_Thailand, "PerCapitaBased") %>%
      add_xml_data(X232.IncomeElasticity_ind_gssp2_Subregions_Thailand, "IncomeElasticity") %>%
      add_xml_data(X232.PriceElasticity_ind_Subregions_Thailand, "PriceElasticity") %>%
      add_xml_data(X232.BaseService_ind_Subregions_Thailand, "BaseService") %>%
      add_xml_data(X232.UnlimitRsrc_Subregions_Thailand, "UnlimitRsrc") %>%
      add_xml_data(X232.UnlimitRsrcPrice_Subregions_Thailand, "UnlimitRsrcPrice") %>%
      add_logit_tables_xml(X232.Supplysector_urb_indproc_Subregions_Thailand, "Supplysector") %>%
      add_logit_tables_xml(X232.SubsectorLogit_urb_ind_Subregions_Thailand, "SubsectorLogit") %>%
      add_xml_data(X232.SubsectorShrwtFllt_urb_ind_Subregions_Thailand, "SubsectorShrwtFllt") %>%
      add_xml_data(X232.SubsectorInterp_urb_ind_Subregions_Thailand, "SubsectorInterp") %>%
      add_xml_data(X232.StubTech_urb_ind_Subregions_Thailand, "StubTech") %>%
      add_xml_data(X232.StubTechCoef_indproc_Subregions_Thailand, "StubTechCoef") %>%
      add_xml_data(X232.MAC_indproc_Subregions_Thailand, "MAC") %>%
      add_xml_data(X232.fgas_all_units_ind_Subregions_Thailand, "StubTechEmissUnits") %>%
      add_xml_data(X232.nonghg_max_reduction_ind_Subregions_Thailand, "GDPCtrlMax") %>%
      add_xml_data(X232.nonghg_steepness_ind_Subregions_Thailand, "GDPCtrlSteep") %>%
      add_xml_data(X232.hfc_future_ind_Subregions_Thailand, "OutputEmissCoeff") %>%
      add_xml_data(X232.MAC_higwp_ind_Subregions_Thailand, "MAC") %>%
      add_xml_data(X232.nonco2_max_reduction_indproc_Subregions_Thailand, "GDPCtrlMax") %>%
      add_xml_data(X232.nonco2_steepness_indproc_Subregions_Thailand, "GDPCtrlSteep") %>%
      add_xml_data(X232.pol_emissions_ind_Subregions_Thailand, "InputEmissions") %>%
      add_xml_data(X232.ghg_emissions_ind_Subregions_Thailand, "InputEmissions") %>%
      add_xml_data(X232.StubTechMarket_ind_Subregions_Thailand, "StubTechMarket") %>%
      add_xml_data(X232.StubTechSecMarket_ind_Subregions_Thailand, "StubTechSecMarket") %>%
      add_xml_data(X232.nonco2_indproc_Subregions_Thailand, "StbTechOutputEmissions") %>%
      add_xml_data(X232.hfc_all_indproc_Subregions_Thailand, "StbTechOutputEmissions") %>%
      add_xml_data(X232.pfc_all_indproc_Subregions_Thailand, "StbTechOutputEmissions") %>%
      add_precursors("X232.DeleteFinalDemand_ind_Subregions_Thailand",
                     "X232.DeleteSupplysector_ind_Subregions_Thailand",
                     "X232.Supplysector_ind_Subregions_Thailand",
                     "X232.FinalEnergyKeyword_ind_Subregions_Thailand",
                     "X232.SubsectorLogit_ind_Subregions_Thailand",
                     "X232.SubsectorShrwtFllt_ind_Subregions_Thailand",
                     "X232.SubsectorInterp_ind_Subregions_Thailand",
                     "X232.FuelPrefElast_indenergy_Subregions_Thailand",
                     "X232.StubTech_ind_Subregions_Thailand",
                     "X232.StubTechInterp_ind_Subregions_Thailand",
                     "X232.StubTechCalInput_indenergy_Subregions_Thailand",
                     "X232.StubTechCalInput_indfeed_Subregions_Thailand",
                     "X232.StubTechProd_industry_Subregions_Thailand",
                     "X232.StubTechCoef_industry_Subregions_Thailand",
                     "X232.PerCapitaBased_ind_Subregions_Thailand",
                     "X232.IncomeElasticity_ind_gssp2_Subregions_Thailand",
                     "X232.PriceElasticity_ind_Subregions_Thailand",
                     "X232.BaseService_ind_Subregions_Thailand",
                     "X232.UnlimitRsrc_Subregions_Thailand",
                     "X232.UnlimitRsrcPrice_Subregions_Thailand",
                     "X232.Supplysector_urb_indproc_Subregions_Thailand",
                     "X232.SubsectorLogit_urb_ind_Subregions_Thailand",
                     "X232.SubsectorShrwtFllt_urb_ind_Subregions_Thailand",
                     "X232.SubsectorInterp_urb_ind_Subregions_Thailand",
                     "X232.StubTech_urb_ind_Subregions_Thailand",
                     "X232.StubTechCoef_indproc_Subregions_Thailand",
                     "X232.MAC_indproc_Subregions_Thailand",
                     "X232.fgas_all_units_ind_Subregions_Thailand",
                     "X232.nonghg_max_reduction_ind_Subregions_Thailand",
                     "X232.nonghg_steepness_ind_Subregions_Thailand",
                     "X232.hfc_future_ind_Subregions_Thailand",
                     "X232.MAC_higwp_ind_Subregions_Thailand",
                     "X232.nonco2_max_reduction_indproc_Subregions_Thailand",
                     "X232.nonco2_steepness_indproc_Subregions_Thailand",
                     "X232.pol_emissions_ind_Subregions_Thailand",
                     "X232.ghg_emissions_ind_Subregions_Thailand",
                     "X232.StubTechMarket_ind_Subregions_Thailand",
                     "X232.StubTechSecMarket_ind_Subregions_Thailand",
                     "X232.nonco2_indproc_Subregions_Thailand",
                     "X232.hfc_all_indproc_Subregions_Thailand",
                     "X232.pfc_all_indproc_Subregions_Thailand") ->
      industry_Subregions_Thailand.xml

    return_data(industry_Subregions_Thailand.xml)
  } else {
    stop("Unknown command")
  }
}
