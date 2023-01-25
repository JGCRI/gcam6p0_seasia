# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_transportation_xml_Subregions_Thailand
#'
#' Construct XML data structure for \code{transportation_Subregions_Thailand}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_Subregions_Thailand.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_Subregions_Thailand} (energy XML).
module_energy_Xbatch_transportation_xml_Subregions_Thailand <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X254.DeleteFinalDemand_trn_Subregions_Thailand",
             "X254.DeleteSupplysector_trn_Subregions_Thailand",
             "X254.Supplysector_trn_Subregions_Thailand",
             "X254.FinalEnergyKeyword_trn_Subregions_Thailand",
             "X254.tranSubsectorLogit_trn_Subregions_Thailand",
             "X254.tranSubsectorShrwt_trn_Subregions_Thailand",
             "X254.tranSubsectorShrwtFllt_trn_Subregions_Thailand",
             "X254.tranSubsectorInterp_trn_Subregions_Thailand",
             "X254.tranSubsectorSpeed_trn_Subregions_Thailand",
             "X254.tranSubsectorSpeed_passthru_trn_Subregions_Thailand",
             "X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Thailand",
             "X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Thailand",
             "X254.tranSubsectorVOTT_trn_Subregions_Thailand",
             "X254.tranSubsectorFuelPref_trn_Subregions_Thailand",
             "X254.StubTranTech_trn_Subregions_Thailand",
             "X254.StubTranTech_passthru_trn_Subregions_Thailand",
             "X254.StubTranTech_nonmotor_trn_Subregions_Thailand",
             "X254.StubTranTechCalInput_trn_Subregions_Thailand",
             "X254.StubTranTechLoadFactor_trn_Subregions_Thailand",
             "X254.StubTranTechCost_trn_Subregions_Thailand",
             "X254.StubTranTechCoef_trn_Subregions_Thailand",
             "X254.StubTechCalInput_passthru_trn_Subregions_Thailand",
             "X254.StubTechProd_nonmotor_trn_Subregions_Thailand",
             "X254.PerCapitaBased_trn_Subregions_Thailand",
             "X254.PriceElasticity_trn_Subregions_Thailand",
             "X254.IncomeElasticity_trn_Subregions_Thailand",
             "X254.BaseService_trn_Subregions_Thailand",
             "X254.fgas_all_units_trn_Subregions_Thailand",
             "X254.nonghg_max_reduction_trn_Subregions_Thailand",
             "X254.nonghg_steepness_trn_Subregions_Thailand",
             "X254.hfc_future_trn_Subregions_Thailand",
             "X254.pol_emissions_trn_Subregions_Thailand",
             "X254.ghg_emissions_trn_Subregions_Thailand"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_Subregions_Thailand.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X254.DeleteFinalDemand_trn_Subregions_Thailand <- get_data(all_data, "X254.DeleteFinalDemand_trn_Subregions_Thailand")
    X254.DeleteSupplysector_trn_Subregions_Thailand <- get_data(all_data, "X254.DeleteSupplysector_trn_Subregions_Thailand")
    X254.Supplysector_trn_Subregions_Thailand <- get_data(all_data, "X254.Supplysector_trn_Subregions_Thailand")
    X254.FinalEnergyKeyword_trn_Subregions_Thailand <- get_data(all_data, "X254.FinalEnergyKeyword_trn_Subregions_Thailand")
    X254.tranSubsectorLogit_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorLogit_trn_Subregions_Thailand")
    X254.tranSubsectorShrwt_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorShrwt_trn_Subregions_Thailand")
    X254.tranSubsectorShrwtFllt_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorShrwtFllt_trn_Subregions_Thailand")
    X254.tranSubsectorInterp_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorInterp_trn_Subregions_Thailand")
    X254.tranSubsectorSpeed_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorSpeed_trn_Subregions_Thailand")
    X254.tranSubsectorSpeed_passthru_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorSpeed_passthru_trn_Subregions_Thailand")
    X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Thailand")
    X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Thailand")
    X254.tranSubsectorVOTT_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorVOTT_trn_Subregions_Thailand")
    X254.tranSubsectorFuelPref_trn_Subregions_Thailand <- get_data(all_data, "X254.tranSubsectorFuelPref_trn_Subregions_Thailand")
    X254.StubTranTech_trn_Subregions_Thailand <- get_data(all_data, "X254.StubTranTech_trn_Subregions_Thailand")
    X254.StubTranTech_passthru_trn_Subregions_Thailand <- get_data(all_data, "X254.StubTranTech_passthru_trn_Subregions_Thailand")
    X254.StubTranTech_nonmotor_trn_Subregions_Thailand <- get_data(all_data, "X254.StubTranTech_nonmotor_trn_Subregions_Thailand")
    X254.StubTranTechCalInput_trn_Subregions_Thailand <- get_data(all_data, "X254.StubTranTechCalInput_trn_Subregions_Thailand")
    X254.StubTranTechLoadFactor_trn_Subregions_Thailand <- get_data(all_data, "X254.StubTranTechLoadFactor_trn_Subregions_Thailand")
    X254.StubTranTechCost_trn_Subregions_Thailand <- get_data(all_data, "X254.StubTranTechCost_trn_Subregions_Thailand")
    X254.StubTranTechCoef_trn_Subregions_Thailand <- get_data(all_data, "X254.StubTranTechCoef_trn_Subregions_Thailand")
    X254.StubTechCalInput_passthru_trn_Subregions_Thailand <- get_data(all_data, "X254.StubTechCalInput_passthru_trn_Subregions_Thailand")
    X254.StubTechProd_nonmotor_trn_Subregions_Thailand <- get_data(all_data, "X254.StubTechProd_nonmotor_trn_Subregions_Thailand")
    X254.PerCapitaBased_trn_Subregions_Thailand <- get_data(all_data, "X254.PerCapitaBased_trn_Subregions_Thailand")
    X254.PriceElasticity_trn_Subregions_Thailand <- get_data(all_data, "X254.PriceElasticity_trn_Subregions_Thailand")
    X254.IncomeElasticity_trn_Subregions_Thailand <- get_data(all_data, "X254.IncomeElasticity_trn_Subregions_Thailand")
    X254.BaseService_trn_Subregions_Thailand <- get_data(all_data, "X254.BaseService_trn_Subregions_Thailand")
    X254.fgas_all_units_trn_Subregions_Thailand <- get_data(all_data, "X254.fgas_all_units_trn_Subregions_Thailand")
    X254.nonghg_max_reduction_trn_Subregions_Thailand <- get_data(all_data, "X254.nonghg_max_reduction_trn_Subregions_Thailand")
    X254.nonghg_steepness_trn_Subregions_Thailand <- get_data(all_data, "X254.nonghg_steepness_trn_Subregions_Thailand")
    X254.hfc_future_trn_Subregions_Thailand <- get_data(all_data, "X254.hfc_future_trn_Subregions_Thailand")
    X254.pol_emissions_trn_Subregions_Thailand <- get_data(all_data, "X254.pol_emissions_trn_Subregions_Thailand")
    X254.ghg_emissions_trn_Subregions_Thailand <- get_data(all_data, "X254.ghg_emissions_trn_Subregions_Thailand")

    # ===================================================

    # Produce outputs
    create_xml("transportation_Subregions_Thailand.xml") %>%
      add_xml_data(X254.DeleteFinalDemand_trn_Subregions_Thailand, "DeleteFinalDemand") %>%
      add_xml_data(X254.DeleteSupplysector_trn_Subregions_Thailand, "DeleteSupplysector") %>%
      add_logit_tables_xml(X254.Supplysector_trn_Subregions_Thailand, "Supplysector") %>%
      add_xml_data(X254.FinalEnergyKeyword_trn_Subregions_Thailand, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(X254.tranSubsectorLogit_trn_Subregions_Thailand, "tranSubsectorLogit", "tranSubsector") %>%
      add_xml_data(X254.tranSubsectorShrwt_trn_Subregions_Thailand, "tranSubsectorShrwt") %>%
      add_xml_data(X254.tranSubsectorShrwtFllt_trn_Subregions_Thailand, "tranSubsectorShrwtFllt") %>%
      add_xml_data(X254.tranSubsectorInterp_trn_Subregions_Thailand, "tranSubsectorInterp") %>%
      add_xml_data(X254.tranSubsectorSpeed_trn_Subregions_Thailand, "tranSubsectorSpeed") %>%
      add_xml_data(X254.tranSubsectorSpeed_passthru_trn_Subregions_Thailand, "tranSubsectorSpeed") %>%
      add_xml_data(X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Thailand, "tranSubsectorSpeed") %>%
      add_xml_data(X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Thailand, "tranSubsectorSpeed") %>%
      add_xml_data(X254.tranSubsectorVOTT_trn_Subregions_Thailand, "tranSubsectorVOTT") %>%
      add_xml_data(X254.tranSubsectorFuelPref_trn_Subregions_Thailand, "tranSubsectorFuelPref") %>%
      add_xml_data(X254.StubTranTech_trn_Subregions_Thailand, "StubTranTech") %>%
      add_xml_data(X254.StubTranTech_passthru_trn_Subregions_Thailand, "StubTranTech") %>%
      add_xml_data(X254.StubTranTech_nonmotor_trn_Subregions_Thailand, "StubTranTech") %>%
      add_xml_data(X254.StubTranTechCalInput_trn_Subregions_Thailand, "StubTranTechCalInput") %>%
      add_xml_data(X254.StubTranTechLoadFactor_trn_Subregions_Thailand, "StubTranTechLoadFactor") %>%
      add_xml_data(X254.StubTranTechCost_trn_Subregions_Thailand, "StubTranTechCost") %>%
      add_xml_data(X254.StubTranTechCoef_trn_Subregions_Thailand, "StubTranTechCoef") %>%
      add_xml_data(X254.StubTechCalInput_passthru_trn_Subregions_Thailand, "StubTranTechCalInput") %>%
      add_xml_data(X254.StubTechProd_nonmotor_trn_Subregions_Thailand, "StubTranTechProd") %>%
      add_xml_data(X254.PerCapitaBased_trn_Subregions_Thailand, "PerCapitaBased") %>%
      add_xml_data(X254.PriceElasticity_trn_Subregions_Thailand, "PriceElasticity") %>%
      add_xml_data(X254.IncomeElasticity_trn_Subregions_Thailand, "IncomeElasticity") %>%
      add_xml_data(X254.BaseService_trn_Subregions_Thailand, "BaseService") %>%
      add_xml_data(X254.fgas_all_units_trn_Subregions_Thailand, "StubTechEmissUnits") %>%
      add_xml_data(X254.nonghg_max_reduction_trn_Subregions_Thailand, "GDPCtrlMax") %>%
      add_xml_data(X254.nonghg_steepness_trn_Subregions_Thailand, "GDPCtrlSteep") %>%
      add_xml_data(X254.hfc_future_trn_Subregions_Thailand, "OutputEmissCoeff") %>%
      add_xml_data(X254.pol_emissions_trn_Subregions_Thailand, "InputEmissions") %>%
      add_xml_data(X254.ghg_emissions_trn_Subregions_Thailand, "InputEmissions") %>%
      add_precursors("X254.DeleteFinalDemand_trn_Subregions_Thailand",
                     "X254.DeleteSupplysector_trn_Subregions_Thailand",
                     "X254.Supplysector_trn_Subregions_Thailand",
                     "X254.FinalEnergyKeyword_trn_Subregions_Thailand",
                     "X254.tranSubsectorLogit_trn_Subregions_Thailand",
                     "X254.tranSubsectorShrwt_trn_Subregions_Thailand",
                     "X254.tranSubsectorShrwtFllt_trn_Subregions_Thailand",
                     "X254.tranSubsectorInterp_trn_Subregions_Thailand",
                     "X254.tranSubsectorSpeed_trn_Subregions_Thailand",
                     "X254.tranSubsectorSpeed_passthru_trn_Subregions_Thailand",
                     "X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Thailand",
                     "X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Thailand",
                     "X254.tranSubsectorVOTT_trn_Subregions_Thailand",
                     "X254.tranSubsectorFuelPref_trn_Subregions_Thailand",
                     "X254.StubTranTech_trn_Subregions_Thailand",
                     "X254.StubTranTech_passthru_trn_Subregions_Thailand",
                     "X254.StubTranTech_nonmotor_trn_Subregions_Thailand",
                     "X254.StubTranTechCalInput_trn_Subregions_Thailand",
                     "X254.StubTranTechLoadFactor_trn_Subregions_Thailand",
                     "X254.StubTranTechCost_trn_Subregions_Thailand",
                     "X254.StubTranTechCoef_trn_Subregions_Thailand",
                     "X254.StubTechCalInput_passthru_trn_Subregions_Thailand",
                     "X254.StubTechProd_nonmotor_trn_Subregions_Thailand",
                     "X254.PerCapitaBased_trn_Subregions_Thailand",
                     "X254.PriceElasticity_trn_Subregions_Thailand",
                     "X254.IncomeElasticity_trn_Subregions_Thailand",
                     "X254.BaseService_trn_Subregions_Thailand",
                     "X254.fgas_all_units_trn_Subregions_Thailand",
                     "X254.nonghg_max_reduction_trn_Subregions_Thailand",
                     "X254.nonghg_steepness_trn_Subregions_Thailand",
                     "X254.hfc_future_trn_Subregions_Thailand",
                     "X254.pol_emissions_trn_Subregions_Thailand",
                     "X254.ghg_emissions_trn_Subregions_Thailand") ->
      transportation_Subregions_Thailand.xml

    return_data(transportation_Subregions_Thailand.xml)
  } else {
    stop("Unknown command")
  }
}
