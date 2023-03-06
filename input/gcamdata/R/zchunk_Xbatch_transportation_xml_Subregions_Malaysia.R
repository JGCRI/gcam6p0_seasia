# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_transportation_xml_Subregions_Malaysia
#'
#' Construct XML data structure for \code{transportation_Subregions_Malaysia}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_Subregions_Malaysia.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_Subregions_Malaysia} (energy XML).
module_energy_Xbatch_transportation_xml_Subregions_Malaysia <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X254.DeleteFinalDemand_trn_Subregions_Malaysia",
             "X254.DeleteSupplysector_trn_Subregions_Malaysia",
             "X254.Supplysector_trn_Subregions_Malaysia",
             "X254.FinalEnergyKeyword_trn_Subregions_Malaysia",
             "X254.tranSubsectorLogit_trn_Subregions_Malaysia",
             "X254.tranSubsectorShrwt_trn_Subregions_Malaysia",
             "X254.tranSubsectorShrwtFllt_trn_Subregions_Malaysia",
             "X254.tranSubsectorInterp_trn_Subregions_Malaysia",
             "X254.tranSubsectorSpeed_trn_Subregions_Malaysia",
             "X254.tranSubsectorSpeed_passthru_trn_Subregions_Malaysia",
             "X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Malaysia",
             "X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Malaysia",
             "X254.tranSubsectorVOTT_trn_Subregions_Malaysia",
             "X254.tranSubsectorFuelPref_trn_Subregions_Malaysia",
             "X254.StubTranTech_trn_Subregions_Malaysia",
             "X254.StubTranTech_passthru_trn_Subregions_Malaysia",
             "X254.StubTranTech_nonmotor_trn_Subregions_Malaysia",
             "X254.StubTranTechCalInput_trn_Subregions_Malaysia",
             "X254.StubTranTechLoadFactor_trn_Subregions_Malaysia",
             "X254.StubTranTechCost_trn_Subregions_Malaysia",
             "X254.StubTranTechCoef_trn_Subregions_Malaysia",
             "X254.StubTechCalInput_passthru_trn_Subregions_Malaysia",
             "X254.StubTechProd_nonmotor_trn_Subregions_Malaysia",
             "X254.PerCapitaBased_trn_Subregions_Malaysia",
             "X254.PriceElasticity_trn_Subregions_Malaysia",
             "X254.IncomeElasticity_trn_Subregions_Malaysia",
             "X254.BaseService_trn_Subregions_Malaysia",
             "X254.fgas_all_units_trn_Subregions_Malaysia",
             "X254.nonghg_max_reduction_trn_Subregions_Malaysia",
             "X254.nonghg_steepness_trn_Subregions_Malaysia",
             "X254.hfc_future_trn_Subregions_Malaysia",
             "X254.pol_emissions_trn_Subregions_Malaysia",
             "X254.ghg_emissions_trn_Subregions_Malaysia"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_Subregions_Malaysia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X254.DeleteFinalDemand_trn_Subregions_Malaysia <- get_data(all_data, "X254.DeleteFinalDemand_trn_Subregions_Malaysia")
    X254.DeleteSupplysector_trn_Subregions_Malaysia <- get_data(all_data, "X254.DeleteSupplysector_trn_Subregions_Malaysia")
    X254.Supplysector_trn_Subregions_Malaysia <- get_data(all_data, "X254.Supplysector_trn_Subregions_Malaysia")
    X254.FinalEnergyKeyword_trn_Subregions_Malaysia <- get_data(all_data, "X254.FinalEnergyKeyword_trn_Subregions_Malaysia")
    X254.tranSubsectorLogit_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorLogit_trn_Subregions_Malaysia")
    X254.tranSubsectorShrwt_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorShrwt_trn_Subregions_Malaysia")
    X254.tranSubsectorShrwtFllt_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorShrwtFllt_trn_Subregions_Malaysia")
    X254.tranSubsectorInterp_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorInterp_trn_Subregions_Malaysia") %>%
      # filter out modes we will be changing shareweights for
      filter(tranSubsector != "HSR",
             tranSubsector != "Passenger Rail",
             tranSubsector != "Bus")
    X254.tranSubsectorInterp_trn_Subregions_Malaysia_overwrite <- get_data(all_data, "X254.tranSubsectorInterp_trn_Subregions_Malaysia") %>%
      # filter for modes we will be changing shareweights for
      filter(tranSubsector == "HSR" |
             tranSubsector == "Passenger Rail" |
             tranSubsector == "Bus") %>%
      mutate(overwrite.policy = "NEVER")
    X254.tranSubsectorSpeed_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorSpeed_trn_Subregions_Malaysia")
    X254.tranSubsectorSpeed_passthru_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorSpeed_passthru_trn_Subregions_Malaysia")
    X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Malaysia")
    X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Malaysia")
    X254.tranSubsectorVOTT_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorVOTT_trn_Subregions_Malaysia")
    X254.tranSubsectorFuelPref_trn_Subregions_Malaysia <- get_data(all_data, "X254.tranSubsectorFuelPref_trn_Subregions_Malaysia")
    X254.StubTranTech_trn_Subregions_Malaysia <- get_data(all_data, "X254.StubTranTech_trn_Subregions_Malaysia")
    X254.StubTranTech_passthru_trn_Subregions_Malaysia <- get_data(all_data, "X254.StubTranTech_passthru_trn_Subregions_Malaysia")
    X254.StubTranTech_nonmotor_trn_Subregions_Malaysia <- get_data(all_data, "X254.StubTranTech_nonmotor_trn_Subregions_Malaysia")
    X254.StubTranTechCalInput_trn_Subregions_Malaysia <- get_data(all_data, "X254.StubTranTechCalInput_trn_Subregions_Malaysia")
    X254.StubTranTechLoadFactor_trn_Subregions_Malaysia <- get_data(all_data, "X254.StubTranTechLoadFactor_trn_Subregions_Malaysia")
    X254.StubTranTechCost_trn_Subregions_Malaysia <- get_data(all_data, "X254.StubTranTechCost_trn_Subregions_Malaysia")
    X254.StubTranTechCoef_trn_Subregions_Malaysia <- get_data(all_data, "X254.StubTranTechCoef_trn_Subregions_Malaysia")
    X254.StubTechCalInput_passthru_trn_Subregions_Malaysia <- get_data(all_data, "X254.StubTechCalInput_passthru_trn_Subregions_Malaysia")
    X254.StubTechProd_nonmotor_trn_Subregions_Malaysia <- get_data(all_data, "X254.StubTechProd_nonmotor_trn_Subregions_Malaysia")
    X254.PerCapitaBased_trn_Subregions_Malaysia <- get_data(all_data, "X254.PerCapitaBased_trn_Subregions_Malaysia")
    X254.PriceElasticity_trn_Subregions_Malaysia <- get_data(all_data, "X254.PriceElasticity_trn_Subregions_Malaysia")
    X254.IncomeElasticity_trn_Subregions_Malaysia <- get_data(all_data, "X254.IncomeElasticity_trn_Subregions_Malaysia")
    X254.BaseService_trn_Subregions_Malaysia <- get_data(all_data, "X254.BaseService_trn_Subregions_Malaysia")
    X254.fgas_all_units_trn_Subregions_Malaysia <- get_data(all_data, "X254.fgas_all_units_trn_Subregions_Malaysia")
    X254.nonghg_max_reduction_trn_Subregions_Malaysia <- get_data(all_data, "X254.nonghg_max_reduction_trn_Subregions_Malaysia")
    X254.nonghg_steepness_trn_Subregions_Malaysia <- get_data(all_data, "X254.nonghg_steepness_trn_Subregions_Malaysia")
    X254.hfc_future_trn_Subregions_Malaysia <- get_data(all_data, "X254.hfc_future_trn_Subregions_Malaysia")
    X254.pol_emissions_trn_Subregions_Malaysia <- get_data(all_data, "X254.pol_emissions_trn_Subregions_Malaysia")
    X254.ghg_emissions_trn_Subregions_Malaysia <- get_data(all_data, "X254.ghg_emissions_trn_Subregions_Malaysia")

    # ===================================================

    # Produce outputs
    create_xml("transportation_Subregions_Malaysia.xml") %>%
      add_xml_data(X254.DeleteFinalDemand_trn_Subregions_Malaysia, "DeleteFinalDemand") %>%
      add_xml_data(X254.DeleteSupplysector_trn_Subregions_Malaysia, "DeleteSupplysector") %>%
      add_logit_tables_xml(X254.Supplysector_trn_Subregions_Malaysia, "Supplysector") %>%
      add_xml_data(X254.FinalEnergyKeyword_trn_Subregions_Malaysia, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(X254.tranSubsectorLogit_trn_Subregions_Malaysia, "tranSubsectorLogit", "tranSubsector") %>%
      add_xml_data(X254.tranSubsectorShrwt_trn_Subregions_Malaysia, "tranSubsectorShrwt") %>%
      add_xml_data(X254.tranSubsectorShrwtFllt_trn_Subregions_Malaysia, "tranSubsectorShrwtFllt") %>%
      add_xml_data(X254.tranSubsectorInterp_trn_Subregions_Malaysia, "tranSubsectorInterp") %>%
      add_xml_data(X254.tranSubsectorInterp_trn_Subregions_Malaysia_overwrite, "tranSubsectorInterp_overwrite") %>%
      add_xml_data(X254.tranSubsectorSpeed_trn_Subregions_Malaysia, "tranSubsectorSpeed") %>%
      add_xml_data(X254.tranSubsectorSpeed_passthru_trn_Subregions_Malaysia, "tranSubsectorSpeed") %>%
      add_xml_data(X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Malaysia, "tranSubsectorSpeed") %>%
      add_xml_data(X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Malaysia, "tranSubsectorSpeed") %>%
      add_xml_data(X254.tranSubsectorVOTT_trn_Subregions_Malaysia, "tranSubsectorVOTT") %>%
      add_xml_data(X254.tranSubsectorFuelPref_trn_Subregions_Malaysia, "tranSubsectorFuelPref") %>%
      add_xml_data(X254.StubTranTech_trn_Subregions_Malaysia, "StubTranTech") %>%
      add_xml_data(X254.StubTranTech_passthru_trn_Subregions_Malaysia, "StubTranTech") %>%
      add_xml_data(X254.StubTranTech_nonmotor_trn_Subregions_Malaysia, "StubTranTech") %>%
      add_xml_data(X254.StubTranTechCalInput_trn_Subregions_Malaysia, "StubTranTechCalInput") %>%
      add_xml_data(X254.StubTranTechLoadFactor_trn_Subregions_Malaysia, "StubTranTechLoadFactor") %>%
      add_xml_data(X254.StubTranTechCost_trn_Subregions_Malaysia, "StubTranTechCost") %>%
      add_xml_data(X254.StubTranTechCoef_trn_Subregions_Malaysia, "StubTranTechCoef") %>%
      add_xml_data(X254.StubTechCalInput_passthru_trn_Subregions_Malaysia, "StubTranTechCalInput") %>%
      add_xml_data(X254.StubTechProd_nonmotor_trn_Subregions_Malaysia, "StubTranTechProd") %>%
      add_xml_data(X254.PerCapitaBased_trn_Subregions_Malaysia, "PerCapitaBased") %>%
      add_xml_data(X254.PriceElasticity_trn_Subregions_Malaysia, "PriceElasticity") %>%
      add_xml_data(X254.IncomeElasticity_trn_Subregions_Malaysia, "IncomeElasticity") %>%
      add_xml_data(X254.BaseService_trn_Subregions_Malaysia, "BaseService") %>%
      add_xml_data(X254.fgas_all_units_trn_Subregions_Malaysia, "StubTechEmissUnits") %>%
      add_xml_data(X254.nonghg_max_reduction_trn_Subregions_Malaysia, "GDPCtrlMax") %>%
      add_xml_data(X254.nonghg_steepness_trn_Subregions_Malaysia, "GDPCtrlSteep") %>%
      add_xml_data(X254.hfc_future_trn_Subregions_Malaysia, "OutputEmissCoeff") %>%
      add_xml_data(X254.pol_emissions_trn_Subregions_Malaysia, "InputEmissions") %>%
      add_xml_data(X254.ghg_emissions_trn_Subregions_Malaysia, "InputEmissions") %>%
      add_precursors("X254.DeleteFinalDemand_trn_Subregions_Malaysia",
                     "X254.DeleteSupplysector_trn_Subregions_Malaysia",
                     "X254.Supplysector_trn_Subregions_Malaysia",
                     "X254.FinalEnergyKeyword_trn_Subregions_Malaysia",
                     "X254.tranSubsectorLogit_trn_Subregions_Malaysia",
                     "X254.tranSubsectorShrwt_trn_Subregions_Malaysia",
                     "X254.tranSubsectorShrwtFllt_trn_Subregions_Malaysia",
                     "X254.tranSubsectorInterp_trn_Subregions_Malaysia",
                     "X254.tranSubsectorSpeed_trn_Subregions_Malaysia",
                     "X254.tranSubsectorSpeed_passthru_trn_Subregions_Malaysia",
                     "X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Malaysia",
                     "X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Malaysia",
                     "X254.tranSubsectorVOTT_trn_Subregions_Malaysia",
                     "X254.tranSubsectorFuelPref_trn_Subregions_Malaysia",
                     "X254.StubTranTech_trn_Subregions_Malaysia",
                     "X254.StubTranTech_passthru_trn_Subregions_Malaysia",
                     "X254.StubTranTech_nonmotor_trn_Subregions_Malaysia",
                     "X254.StubTranTechCalInput_trn_Subregions_Malaysia",
                     "X254.StubTranTechLoadFactor_trn_Subregions_Malaysia",
                     "X254.StubTranTechCost_trn_Subregions_Malaysia",
                     "X254.StubTranTechCoef_trn_Subregions_Malaysia",
                     "X254.StubTechCalInput_passthru_trn_Subregions_Malaysia",
                     "X254.StubTechProd_nonmotor_trn_Subregions_Malaysia",
                     "X254.PerCapitaBased_trn_Subregions_Malaysia",
                     "X254.PriceElasticity_trn_Subregions_Malaysia",
                     "X254.IncomeElasticity_trn_Subregions_Malaysia",
                     "X254.BaseService_trn_Subregions_Malaysia",
                     "X254.fgas_all_units_trn_Subregions_Malaysia",
                     "X254.nonghg_max_reduction_trn_Subregions_Malaysia",
                     "X254.nonghg_steepness_trn_Subregions_Malaysia",
                     "X254.hfc_future_trn_Subregions_Malaysia",
                     "X254.pol_emissions_trn_Subregions_Malaysia",
                     "X254.ghg_emissions_trn_Subregions_Malaysia") ->
      transportation_Subregions_Malaysia.xml

    return_data(transportation_Subregions_Malaysia.xml)
  } else {
    stop("Unknown command")
  }
}
