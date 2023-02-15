# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamseasia_batch_building_breakout_SEAsia_xml
#'
#' Construct XML data structure for \code{building_breakout_SEAsia.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_breakout_SEAsia.xml}. The corresponding file in the
#' original data system was \code{batch_building_breakout_SEAsia.xml} (gcamseasia XML).
module_gcamseasia_batch_building_breakout_SEAsia_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.DeleteConsumer_bld_gcamSEA",
             "L244.DeleteSupplysector_bld_gcamSEA",
             "L244.SubregionalShares_bld_gcamSEA",
             "L244.PriceExp_IntGains_bld_gcamSEA",
             "L244.Floorspace_bld_gcamSEA",
             "L244.DemandFunction_serv_bld_gcamSEA",
             "L244.DemandFunction_flsp_bld_gcamSEA",
             "L244.Satiation_flsp_bld_gcamSEA",
             "L244.SatiationAdder_bld_gcamSEA",
             "L244.ThermalBaseService_bld_gcamSEA",
             "L244.GenericBaseService_bld_gcamSEA",
             "L244.ThermalServiceSatiation_bld_gcamSEA",
             "L244.GenericServiceSatiation_bld_gcamSEA",
             "L244.Intgains_scalar_bld_gcamSEA",
             "L244.ShellConductance_bld_gcamSEA",
             "L244.Supplysector_bld_gcamSEA",
             "L244.FinalEnergyKeyword_bld_gcamSEA",
             "L244.SubsectorShrwtFllt_bld_gcamSEA",
             "L244.SubsectorInterp_bld_gcamSEA",
             "L244.SubsectorInterpTo_bld_gcamSEA",
             "L244.SubsectorLogit_bld_gcamSEA",
             "L244.StubTech_bld_gcamSEA",
             "L244.StubTechCalInput_bld_gcamSEA",
             "L244.StubTechMarket_bld_gcamSEA",
             "L244.GlobalTechIntGainOutputRatio_bld_gcamSEA",
             "L244.GlobalTechInterpTo_bld_gcamSEA",
             "L244.GlobalTechEff_bld_gcamSEA",
             "L244.GlobalTechShrwt_bld_gcamSEA",
             "L244.GlobalTechCost_bld_gcamSEA",
             "L244.GlobalTechSCurve_bld_gcamSEA",
             "L244.HDDCDD_constdds_bld_gcamSEA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_breakout_SEAsia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.DeleteConsumer_bld_gcamSEA <- get_data(all_data, "L244.DeleteConsumer_bld_gcamSEA")
    L244.DeleteSupplysector_bld_gcamSEA <- get_data(all_data, "L244.DeleteSupplysector_bld_gcamSEA")
    L244.SubregionalShares_bld_gcamSEA <- get_data(all_data, "L244.SubregionalShares_bld_gcamSEA")
    L244.PriceExp_IntGains_bld_gcamSEA <- get_data(all_data, "L244.PriceExp_IntGains_bld_gcamSEA")
    L244.Floorspace_bld_gcamSEA <- get_data(all_data, "L244.Floorspace_bld_gcamSEA")
    L244.DemandFunction_serv_bld_gcamSEA <- get_data(all_data, "L244.DemandFunction_serv_bld_gcamSEA")
    L244.DemandFunction_flsp_bld_gcamSEA <- get_data(all_data, "L244.DemandFunction_flsp_bld_gcamSEA")
    L244.Satiation_flsp_bld_gcamSEA <- get_data(all_data, "L244.Satiation_flsp_bld_gcamSEA")
    L244.SatiationAdder_bld_gcamSEA <- get_data(all_data, "L244.SatiationAdder_bld_gcamSEA")
    L244.ThermalBaseService_bld_gcamSEA <- get_data(all_data, "L244.ThermalBaseService_bld_gcamSEA")
    L244.GenericBaseService_bld_gcamSEA <- get_data(all_data, "L244.GenericBaseService_bld_gcamSEA")
    L244.ThermalServiceSatiation_bld_gcamSEA <- get_data(all_data, "L244.ThermalServiceSatiation_bld_gcamSEA")
    L244.GenericServiceSatiation_bld_gcamSEA <- get_data(all_data, "L244.GenericServiceSatiation_bld_gcamSEA")
    L244.Intgains_scalar_bld_gcamSEA <- get_data(all_data, "L244.Intgains_scalar_bld_gcamSEA")
    L244.ShellConductance_bld_gcamSEA <- get_data(all_data, "L244.ShellConductance_bld_gcamSEA")
    L244.Supplysector_bld_gcamSEA <- get_data(all_data, "L244.Supplysector_bld_gcamSEA")
    L244.FinalEnergyKeyword_bld_gcamSEA <- get_data(all_data, "L244.FinalEnergyKeyword_bld_gcamSEA")
    L244.SubsectorShrwtFllt_bld_gcamSEA <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_gcamSEA")
    L244.SubsectorInterp_bld_gcamSEA <- get_data(all_data, "L244.SubsectorInterp_bld_gcamSEA")
    L244.SubsectorInterpTo_bld_gcamSEA <- get_data(all_data, "L244.SubsectorInterpTo_bld_gcamSEA")
    L244.SubsectorLogit_bld_gcamSEA <- get_data(all_data, "L244.SubsectorLogit_bld_gcamSEA")
    L244.StubTech_bld_gcamSEA <- get_data(all_data, "L244.StubTech_bld_gcamSEA")
    L244.StubTechCalInput_bld_gcamSEA <- get_data(all_data, "L244.StubTechCalInput_bld_gcamSEA")
    L244.StubTechMarket_bld_gcamSEA <- get_data(all_data, "L244.StubTechMarket_bld_gcamSEA")
    L244.GlobalTechIntGainOutputRatio_bld_gcamSEA <- get_data(all_data, "L244.GlobalTechIntGainOutputRatio_bld_gcamSEA")
    L244.GlobalTechInterpTo_bld_gcamSEA <- get_data(all_data, "L244.GlobalTechInterpTo_bld_gcamSEA")
    L244.GlobalTechEff_bld_gcamSEA <- get_data(all_data, "L244.GlobalTechEff_bld_gcamSEA")
    L244.GlobalTechShrwt_bld_gcamSEA <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamSEA")
    L244.GlobalTechCost_bld_gcamSEA <- get_data(all_data, "L244.GlobalTechCost_bld_gcamSEA")
    L244.GlobalTechSCurve_bld_gcamSEA <- get_data(all_data, "L244.GlobalTechSCurve_bld_gcamSEA")
    L244.HDDCDD_constdds_bld_gcamSEA <- get_data(all_data, "L244.HDDCDD_constdds_bld_gcamSEA")

    # ===================================================

    # Produce outputs
    create_xml("building_breakout_SEAsia.xml") %>%
      add_xml_data(L244.DeleteConsumer_bld_gcamSEA, "DeleteConsumer") %>%
      add_xml_data(L244.DeleteSupplysector_bld_gcamSEA, "DeleteSupplysector") %>%
      add_xml_data(L244.SubregionalShares_bld_gcamSEA, "SubregionalShares") %>%
      add_xml_data(L244.PriceExp_IntGains_bld_gcamSEA, "PriceExp_IntGains") %>%
      add_xml_data(L244.Floorspace_bld_gcamSEA, "Floorspace") %>%
      add_xml_data(L244.DemandFunction_serv_bld_gcamSEA, "DemandFunction_serv") %>%
      add_xml_data(L244.DemandFunction_flsp_bld_gcamSEA, "DemandFunction_flsp") %>%
      add_xml_data(L244.Satiation_flsp_bld_gcamSEA, "Satiation_flsp") %>%
      add_xml_data(L244.SatiationAdder_bld_gcamSEA, "SatiationAdder") %>%
      add_xml_data(L244.ThermalBaseService_bld_gcamSEA, "ThermalBaseService") %>%
      add_xml_data(L244.GenericBaseService_bld_gcamSEA, "GenericBaseService") %>%
      add_xml_data(L244.ThermalServiceSatiation_bld_gcamSEA, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericServiceSatiation_bld_gcamSEA, "GenericServiceSatiation") %>%
      add_xml_data(L244.Intgains_scalar_bld_gcamSEA, "Intgains_scalar") %>%
      add_xml_data(L244.ShellConductance_bld_gcamSEA, "ShellConductance") %>%
      add_logit_tables_xml(L244.Supplysector_bld_gcamSEA, "Supplysector") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld_gcamSEA, "FinalEnergyKeyword") %>%
      add_xml_data(L244.SubsectorShrwtFllt_bld_gcamSEA, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.SubsectorInterp_bld_gcamSEA, "SubsectorInterp") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_gcamSEA, "SubsectorLogit") %>%
      add_xml_data(L244.StubTech_bld_gcamSEA, "StubTech") %>%
      add_xml_data(L244.StubTechCalInput_bld_gcamSEA, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechMarket_bld_gcamSEA, "StubTechMarket") %>%
      add_xml_data(L244.GlobalTechIntGainOutputRatio_bld_gcamSEA, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(L244.GlobalTechInterpTo_bld_gcamSEA, "GlobalTechInterpTo") %>%
      add_xml_data(L244.GlobalTechEff_bld_gcamSEA, "GlobalTechEff") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_gcamSEA, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_gcamSEA, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechSCurve_bld_gcamSEA, "GlobalTechSCurve")  %>%
      add_xml_data(L244.HDDCDD_constdds_bld_gcamSEA, "HDDCDD") %>%
      add_precursors("L244.DeleteConsumer_bld_gcamSEA",
                     "L244.DeleteSupplysector_bld_gcamSEA",
                     "L244.SubregionalShares_bld_gcamSEA",
                     "L244.PriceExp_IntGains_bld_gcamSEA",
                     "L244.Floorspace_bld_gcamSEA",
                     "L244.DemandFunction_serv_bld_gcamSEA",
                     "L244.DemandFunction_flsp_bld_gcamSEA",
                     "L244.Satiation_flsp_bld_gcamSEA",
                     "L244.SatiationAdder_bld_gcamSEA",
                     "L244.ThermalBaseService_bld_gcamSEA",
                     "L244.GenericBaseService_bld_gcamSEA",
                     "L244.ThermalServiceSatiation_bld_gcamSEA",
                     "L244.GenericServiceSatiation_bld_gcamSEA",
                     "L244.Intgains_scalar_bld_gcamSEA",
                     "L244.ShellConductance_bld_gcamSEA",
                     "L244.Supplysector_bld_gcamSEA",
                     "L244.FinalEnergyKeyword_bld_gcamSEA",
                     "L244.SubsectorShrwtFllt_bld_gcamSEA",
                     "L244.SubsectorInterp_bld_gcamSEA",
                     "L244.SubsectorInterpTo_bld_gcamSEA",
                     "L244.SubsectorLogit_bld_gcamSEA",
                     "L244.StubTech_bld_gcamSEA",
                     "L244.StubTechCalInput_bld_gcamSEA",
                     "L244.StubTechMarket_bld_gcamSEA",
                     "L244.GlobalTechIntGainOutputRatio_bld_gcamSEA",
                     "L244.GlobalTechInterpTo_bld_gcamSEA",
                     "L244.GlobalTechEff_bld_gcamSEA",
                     "L244.GlobalTechShrwt_bld_gcamSEA",
                     "L244.GlobalTechCost_bld_gcamSEA",
                     "L244.GlobalTechSCurve_bld_gcamSEA",
                     "L244.HDDCDD_constdds_bld_gcamSEA") ->
      building_breakout_SEAsia.xml

    # # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L244.SubsectorInterpTo_bld_gcamSEA)) {

      building_breakout_SEAsia.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld_gcamSEA, "SubsectorInterpTo") ->
        building_breakout_SEAsia.xml

      }

    return_data(building_breakout_SEAsia.xml)
  } else {
    stop("Unknown command")
  }
}
