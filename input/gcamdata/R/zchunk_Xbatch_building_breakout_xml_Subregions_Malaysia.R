# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamseasia_Xbatch_building_breakout_xml_Subregions_Malaysia
#'
#' Construct XML data structure for \code{building_breakout_Subregions_Malaysia.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_SEAsia.xml}. The corresponding file in the
#' original data system was \code{batch_building_SEAsia.xml} (gcamseasia XML).
module_gcamseasia_Xbatch_building_breakout_xml_Subregions_Malaysia <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X245.DeleteConsumer_bld_Subregions_Malaysia",
             "X245.DeleteSupplysector_bld_Subregions_Malaysia",
             "X245.SubregionalShares_bld_Subregions_Malaysia",
             "X245.PriceExp_IntGains_bld_Subregions_Malaysia",
             "X245.Floorspace_bld_Subregions_Malaysia",
             "X245.DemandFunction_serv_bld_Subregions_Malaysia",
             "X245.DemandFunction_flsp_bld_Subregions_Malaysia",
             "X245.Satiation_flsp_bld_Subregions_Malaysia",
             "X245.SatiationAdder_bld_Subregions_Malaysia",
             "X245.ThermalBaseService_bld_Subregions_Malaysia",
             "X245.GenericBaseService_bld_Subregions_Malaysia",
             "X245.ThermalServiceSatiation_bld_Subregions_Malaysia",
             "X245.GenericServiceSatiation_bld_Subregions_Malaysia",
             "X245.Intgains_scalar_bld_Subregions_Malaysia",
             "X245.ShellConductance_bld_Subregions_Malaysia",
             "X245.Supplysector_bld_Subregions_Malaysia",
             "X245.FinalEnergyKeyword_bld_Subregions_Malaysia",
             "X245.SubsectorShrwtFllt_bld_Subregions_Malaysia",
             "X245.SubsectorInterp_bld_Subregions_Malaysia",
             "X245.SubsectorInterpTo_bld_Subregions_Malaysia",
             "X245.SubsectorLogit_bld_Subregions_Malaysia",
             "X245.StubTech_bld_Subregions_Malaysia",
             "X245.StubTechCalInput_bld_Subregions_Malaysia",
             "X245.StubTechMarket_bld_Subregions_Malaysia",
             "X245.GlobalTechIntGainOutputRatio_bld_Subregions_Malaysia",
             "X245.GlobalTechInterpTo_bld_Subregions_Malaysia",
             "X245.GlobalTechEff_bld_Subregions_Malaysia",
             "X245.GlobalTechShrwt_bld_Subregions_Malaysia",
             "X245.GlobalTechCost_bld_Subregions_Malaysia",
             "X245.GlobalTechSCurve_bld_Subregions_Malaysia",
             "X245.HDDCDD_constdds_bld_Subregions_Malaysia"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_breakout_Subregions_Malaysia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X245.DeleteConsumer_bld_Subregions_Malaysia <- get_data(all_data, "X245.DeleteConsumer_bld_Subregions_Malaysia")
    X245.DeleteSupplysector_bld_Subregions_Malaysia <- get_data(all_data, "X245.DeleteSupplysector_bld_Subregions_Malaysia")
    X245.SubregionalShares_bld_Subregions_Malaysia <- get_data(all_data, "X245.SubregionalShares_bld_Subregions_Malaysia")
    X245.PriceExp_IntGains_bld_Subregions_Malaysia <- get_data(all_data, "X245.PriceExp_IntGains_bld_Subregions_Malaysia")
    X245.Floorspace_bld_Subregions_Malaysia <- get_data(all_data, "X245.Floorspace_bld_Subregions_Malaysia")
    X245.DemandFunction_serv_bld_Subregions_Malaysia <- get_data(all_data, "X245.DemandFunction_serv_bld_Subregions_Malaysia")
    X245.DemandFunction_flsp_bld_Subregions_Malaysia <- get_data(all_data, "X245.DemandFunction_flsp_bld_Subregions_Malaysia")
    X245.Satiation_flsp_bld_Subregions_Malaysia <- get_data(all_data, "X245.Satiation_flsp_bld_Subregions_Malaysia")
    X245.SatiationAdder_bld_Subregions_Malaysia <- get_data(all_data, "X245.SatiationAdder_bld_Subregions_Malaysia")
    X245.ThermalBaseService_bld_Subregions_Malaysia <- get_data(all_data, "X245.ThermalBaseService_bld_Subregions_Malaysia")
    X245.GenericBaseService_bld_Subregions_Malaysia <- get_data(all_data, "X245.GenericBaseService_bld_Subregions_Malaysia")
    X245.ThermalServiceSatiation_bld_Subregions_Malaysia <- get_data(all_data, "X245.ThermalServiceSatiation_bld_Subregions_Malaysia")
    X245.GenericServiceSatiation_bld_Subregions_Malaysia <- get_data(all_data, "X245.GenericServiceSatiation_bld_Subregions_Malaysia")
    X245.Intgains_scalar_bld_Subregions_Malaysia <- get_data(all_data, "X245.Intgains_scalar_bld_Subregions_Malaysia")
    X245.ShellConductance_bld_Subregions_Malaysia <- get_data(all_data, "X245.ShellConductance_bld_Subregions_Malaysia")
    X245.Supplysector_bld_Subregions_Malaysia <- get_data(all_data, "X245.Supplysector_bld_Subregions_Malaysia")
    X245.FinalEnergyKeyword_bld_Subregions_Malaysia <- get_data(all_data, "X245.FinalEnergyKeyword_bld_Subregions_Malaysia")
    X245.SubsectorShrwtFllt_bld_Subregions_Malaysia <- get_data(all_data, "X245.SubsectorShrwtFllt_bld_Subregions_Malaysia")
    X245.SubsectorInterp_bld_Subregions_Malaysia <- get_data(all_data, "X245.SubsectorInterp_bld_Subregions_Malaysia")
    X245.SubsectorInterpTo_bld_Subregions_Malaysia <- get_data(all_data, "X245.SubsectorInterpTo_bld_Subregions_Malaysia")
    X245.SubsectorLogit_bld_Subregions_Malaysia <- get_data(all_data, "X245.SubsectorLogit_bld_Subregions_Malaysia")
    X245.StubTech_bld_Subregions_Malaysia <- get_data(all_data, "X245.StubTech_bld_Subregions_Malaysia")
    X245.StubTechCalInput_bld_Subregions_Malaysia <- get_data(all_data, "X245.StubTechCalInput_bld_Subregions_Malaysia")
    X245.StubTechMarket_bld_Subregions_Malaysia <- get_data(all_data, "X245.StubTechMarket_bld_Subregions_Malaysia")
    X245.GlobalTechIntGainOutputRatio_bld_Subregions_Malaysia <- get_data(all_data, "X245.GlobalTechIntGainOutputRatio_bld_Subregions_Malaysia")
    X245.GlobalTechInterpTo_bld_Subregions_Malaysia <- get_data(all_data, "X245.GlobalTechInterpTo_bld_Subregions_Malaysia")
    X245.GlobalTechEff_bld_Subregions_Malaysia <- get_data(all_data, "X245.GlobalTechEff_bld_Subregions_Malaysia")
    X245.GlobalTechShrwt_bld_Subregions_Malaysia <- get_data(all_data, "X245.GlobalTechShrwt_bld_Subregions_Malaysia")
    X245.GlobalTechCost_bld_Subregions_Malaysia <- get_data(all_data, "X245.GlobalTechCost_bld_Subregions_Malaysia")
    X245.GlobalTechSCurve_bld_Subregions_Malaysia <- get_data(all_data, "X245.GlobalTechSCurve_bld_Subregions_Malaysia")
    X245.HDDCDD_constdds_bld_Subregions_Malaysia <- get_data(all_data, "X245.HDDCDD_constdds_bld_Subregions_Malaysia")



    # ===================================================

    # Produce outputs
    create_xml("building_breakout_Subregions_Malaysia.xml") %>%
      add_xml_data(X245.DeleteConsumer_bld_Subregions_Malaysia, "DeleteConsumer") %>%
      add_xml_data(X245.DeleteSupplysector_bld_Subregions_Malaysia, "DeleteSupplysector") %>%
      add_xml_data(X245.SubregionalShares_bld_Subregions_Malaysia, "SubregionalShares") %>%
      add_xml_data(X245.PriceExp_IntGains_bld_Subregions_Malaysia, "PriceExp_IntGains") %>%
      add_xml_data(X245.Floorspace_bld_Subregions_Malaysia, "Floorspace") %>%
      add_xml_data(X245.DemandFunction_serv_bld_Subregions_Malaysia, "DemandFunction_serv") %>%
      add_xml_data(X245.DemandFunction_flsp_bld_Subregions_Malaysia, "DemandFunction_flsp") %>%
      add_xml_data(X245.Satiation_flsp_bld_Subregions_Malaysia, "Satiation_flsp") %>%
      add_xml_data(X245.SatiationAdder_bld_Subregions_Malaysia, "SatiationAdder") %>%
      add_xml_data(X245.ThermalBaseService_bld_Subregions_Malaysia, "ThermalBaseService") %>%
      add_xml_data(X245.GenericBaseService_bld_Subregions_Malaysia, "GenericBaseService") %>%
      add_xml_data(X245.ThermalServiceSatiation_bld_Subregions_Malaysia, "ThermalServiceSatiation") %>%
      add_xml_data(X245.GenericServiceSatiation_bld_Subregions_Malaysia, "GenericServiceSatiation") %>%
      add_xml_data(X245.Intgains_scalar_bld_Subregions_Malaysia, "Intgains_scalar") %>%
      add_xml_data(X245.ShellConductance_bld_Subregions_Malaysia, "ShellConductance") %>%
      add_logit_tables_xml(X245.Supplysector_bld_Subregions_Malaysia, "Supplysector") %>%
      add_xml_data(X245.FinalEnergyKeyword_bld_Subregions_Malaysia, "FinalEnergyKeyword") %>%
      add_xml_data(X245.SubsectorShrwtFllt_bld_Subregions_Malaysia, "SubsectorShrwtFllt") %>%
      add_xml_data(X245.SubsectorInterp_bld_Subregions_Malaysia, "SubsectorInterp") %>%
      add_logit_tables_xml(X245.SubsectorLogit_bld_Subregions_Malaysia, "SubsectorLogit") %>%
      add_xml_data(X245.StubTech_bld_Subregions_Malaysia, "StubTech") %>%
      add_xml_data(X245.StubTechCalInput_bld_Subregions_Malaysia, "StubTechCalInput") %>%
      add_xml_data(X245.StubTechMarket_bld_Subregions_Malaysia, "StubTechMarket") %>%
      add_xml_data(X245.GlobalTechIntGainOutputRatio_bld_Subregions_Malaysia, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(X245.GlobalTechInterpTo_bld_Subregions_Malaysia, "GlobalTechInterpTo") %>%
      add_xml_data(X245.GlobalTechEff_bld_Subregions_Malaysia, "GlobalTechEff") %>%
      add_xml_data(X245.GlobalTechShrwt_bld_Subregions_Malaysia, "GlobalTechShrwt") %>%
      add_xml_data(X245.GlobalTechCost_bld_Subregions_Malaysia, "GlobalTechCost") %>%
      add_xml_data(X245.GlobalTechSCurve_bld_Subregions_Malaysia, "GlobalTechSCurve")  %>%
      add_xml_data(X245.HDDCDD_constdds_bld_Subregions_Malaysia, "HDDCDD") %>%
      add_precursors("X245.DeleteConsumer_bld_Subregions_Malaysia",
                     "X245.DeleteSupplysector_bld_Subregions_Malaysia",
                     "X245.SubregionalShares_bld_Subregions_Malaysia",
                     "X245.PriceExp_IntGains_bld_Subregions_Malaysia",
                     "X245.Floorspace_bld_Subregions_Malaysia",
                     "X245.DemandFunction_serv_bld_Subregions_Malaysia",
                     "X245.DemandFunction_flsp_bld_Subregions_Malaysia",
                     "X245.Satiation_flsp_bld_Subregions_Malaysia",
                     "X245.SatiationAdder_bld_Subregions_Malaysia",
                     "X245.ThermalBaseService_bld_Subregions_Malaysia",
                     "X245.GenericBaseService_bld_Subregions_Malaysia",
                     "X245.ThermalServiceSatiation_bld_Subregions_Malaysia",
                     "X245.GenericServiceSatiation_bld_Subregions_Malaysia",
                     "X245.Intgains_scalar_bld_Subregions_Malaysia",
                     "X245.ShellConductance_bld_Subregions_Malaysia",
                     "X245.Supplysector_bld_Subregions_Malaysia",
                     "X245.FinalEnergyKeyword_bld_Subregions_Malaysia",
                     "X245.SubsectorShrwtFllt_bld_Subregions_Malaysia",
                     "X245.SubsectorInterp_bld_Subregions_Malaysia",
                     "X245.SubsectorInterpTo_bld_Subregions_Malaysia",
                     "X245.SubsectorLogit_bld_Subregions_Malaysia",
                     "X245.StubTech_bld_Subregions_Malaysia",
                     "X245.StubTechCalInput_bld_Subregions_Malaysia",
                     "X245.StubTechMarket_bld_Subregions_Malaysia",
                     "X245.GlobalTechIntGainOutputRatio_bld_Subregions_Malaysia",
                     "X245.GlobalTechInterpTo_bld_Subregions_Malaysia",
                     "X245.GlobalTechEff_bld_Subregions_Malaysia",
                     "X245.GlobalTechShrwt_bld_Subregions_Malaysia",
                     "X245.GlobalTechCost_bld_Subregions_Malaysia",
                     "X245.GlobalTechSCurve_bld_Subregions_Malaysia",
                     "X245.HDDCDD_constdds_bld_Subregions_Malaysia") ->
      building_breakout_Subregions_Malaysia.xml

    # # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(X245.SubsectorInterpTo_bld_Subregions_Malaysia)) {

      building_breakout_Subregions_Malaysia.xml %>%
        add_xml_data(X245.SubsectorInterpTo_bld_Subregions_Malaysia, "SubsectorInterpTo") ->
        building_breakout_Subregions_Malaysia.xml

      }

    return_data(building_breakout_Subregions_Malaysia.xml)
  } else {
    stop("Unknown command")
  }
}
