# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamseasia_Xbatch_building_breakout_xml_Subregions_Thailand
#'
#' Construct XML data structure for \code{building_breakout_Subregions_Thailand.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_SEAsia.xml}. The corresponding file in the
#' original data system was \code{batch_building_SEAsia.xml} (gcamseasia XML).
module_gcamseasia_Xbatch_building_breakout_xml_Subregions_Thailand <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X245.DeleteConsumer_bld_Subregions_Thailand",
             "X245.DeleteSupplysector_bld_Subregions_Thailand",
             "X245.SubregionalShares_bld_Subregions_Thailand",
             "X245.PriceExp_IntGains_bld_Subregions_Thailand",
             "X245.Floorspace_bld_Subregions_Thailand",
             "X245.DemandFunction_serv_bld_Subregions_Thailand",
             "X245.DemandFunction_flsp_bld_Subregions_Thailand",
             "X245.Satiation_flsp_bld_Subregions_Thailand",
             "X245.SatiationAdder_bld_Subregions_Thailand",
             "X245.ThermalBaseService_bld_Subregions_Thailand",
             "X245.GenericBaseService_bld_Subregions_Thailand",
             "X245.ThermalServiceSatiation_bld_Subregions_Thailand",
             "X245.GenericServiceSatiation_bld_Subregions_Thailand",
             "X245.Intgains_scalar_bld_Subregions_Thailand",
             "X245.ShellConductance_bld_Subregions_Thailand",
             "X245.Supplysector_bld_Subregions_Thailand",
             "X245.FinalEnergyKeyword_bld_Subregions_Thailand",
             "X245.SubsectorShrwtFllt_bld_Subregions_Thailand",
             "X245.SubsectorInterp_bld_Subregions_Thailand",
             "X245.SubsectorInterpTo_bld_Subregions_Thailand",
             "X245.SubsectorLogit_bld_Subregions_Thailand",
             "X245.StubTech_bld_Subregions_Thailand",
             "X245.StubTechCalInput_bld_Subregions_Thailand",
             "X245.StubTechMarket_bld_Subregions_Thailand",
             "X245.GlobalTechIntGainOutputRatio_bld_Subregions_Thailand",
             "X245.GlobalTechInterpTo_bld_Subregions_Thailand",
             "X245.GlobalTechEff_bld_Subregions_Thailand",
             "X245.GlobalTechShrwt_bld_Subregions_Thailand",
             "X245.GlobalTechCost_bld_Subregions_Thailand",
             "X245.GlobalTechSCurve_bld_Subregions_Thailand",
             "X245.HDDCDD_constdds_bld_Subregions_Thailand"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_breakout_Subregions_Thailand.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X245.DeleteConsumer_bld_Subregions_Thailand <- get_data(all_data, "X245.DeleteConsumer_bld_Subregions_Thailand")
    X245.DeleteSupplysector_bld_Subregions_Thailand <- get_data(all_data, "X245.DeleteSupplysector_bld_Subregions_Thailand")
    X245.SubregionalShares_bld_Subregions_Thailand <- get_data(all_data, "X245.SubregionalShares_bld_Subregions_Thailand")
    X245.PriceExp_IntGains_bld_Subregions_Thailand <- get_data(all_data, "X245.PriceExp_IntGains_bld_Subregions_Thailand")
    X245.Floorspace_bld_Subregions_Thailand <- get_data(all_data, "X245.Floorspace_bld_Subregions_Thailand")
    X245.DemandFunction_serv_bld_Subregions_Thailand <- get_data(all_data, "X245.DemandFunction_serv_bld_Subregions_Thailand")
    X245.DemandFunction_flsp_bld_Subregions_Thailand <- get_data(all_data, "X245.DemandFunction_flsp_bld_Subregions_Thailand")
    X245.Satiation_flsp_bld_Subregions_Thailand <- get_data(all_data, "X245.Satiation_flsp_bld_Subregions_Thailand")
    X245.SatiationAdder_bld_Subregions_Thailand <- get_data(all_data, "X245.SatiationAdder_bld_Subregions_Thailand")
    X245.ThermalBaseService_bld_Subregions_Thailand <- get_data(all_data, "X245.ThermalBaseService_bld_Subregions_Thailand")
    X245.GenericBaseService_bld_Subregions_Thailand <- get_data(all_data, "X245.GenericBaseService_bld_Subregions_Thailand")
    X245.ThermalServiceSatiation_bld_Subregions_Thailand <- get_data(all_data, "X245.ThermalServiceSatiation_bld_Subregions_Thailand")
    X245.GenericServiceSatiation_bld_Subregions_Thailand <- get_data(all_data, "X245.GenericServiceSatiation_bld_Subregions_Thailand")
    X245.Intgains_scalar_bld_Subregions_Thailand <- get_data(all_data, "X245.Intgains_scalar_bld_Subregions_Thailand")
    X245.ShellConductance_bld_Subregions_Thailand <- get_data(all_data, "X245.ShellConductance_bld_Subregions_Thailand")
    X245.Supplysector_bld_Subregions_Thailand <- get_data(all_data, "X245.Supplysector_bld_Subregions_Thailand")
    X245.FinalEnergyKeyword_bld_Subregions_Thailand <- get_data(all_data, "X245.FinalEnergyKeyword_bld_Subregions_Thailand")
    X245.SubsectorShrwtFllt_bld_Subregions_Thailand <- get_data(all_data, "X245.SubsectorShrwtFllt_bld_Subregions_Thailand")
    X245.SubsectorInterp_bld_Subregions_Thailand <- get_data(all_data, "X245.SubsectorInterp_bld_Subregions_Thailand")
    X245.SubsectorInterpTo_bld_Subregions_Thailand <- get_data(all_data, "X245.SubsectorInterpTo_bld_Subregions_Thailand")
    X245.SubsectorLogit_bld_Subregions_Thailand <- get_data(all_data, "X245.SubsectorLogit_bld_Subregions_Thailand")
    X245.StubTech_bld_Subregions_Thailand <- get_data(all_data, "X245.StubTech_bld_Subregions_Thailand")
    X245.StubTechCalInput_bld_Subregions_Thailand <- get_data(all_data, "X245.StubTechCalInput_bld_Subregions_Thailand")
    X245.StubTechMarket_bld_Subregions_Thailand <- get_data(all_data, "X245.StubTechMarket_bld_Subregions_Thailand")
    X245.GlobalTechIntGainOutputRatio_bld_Subregions_Thailand <- get_data(all_data, "X245.GlobalTechIntGainOutputRatio_bld_Subregions_Thailand")
    X245.GlobalTechInterpTo_bld_Subregions_Thailand <- get_data(all_data, "X245.GlobalTechInterpTo_bld_Subregions_Thailand")
    X245.GlobalTechEff_bld_Subregions_Thailand <- get_data(all_data, "X245.GlobalTechEff_bld_Subregions_Thailand")
    X245.GlobalTechShrwt_bld_Subregions_Thailand <- get_data(all_data, "X245.GlobalTechShrwt_bld_Subregions_Thailand")
    X245.GlobalTechCost_bld_Subregions_Thailand <- get_data(all_data, "X245.GlobalTechCost_bld_Subregions_Thailand")
    X245.GlobalTechSCurve_bld_Subregions_Thailand <- get_data(all_data, "X245.GlobalTechSCurve_bld_Subregions_Thailand")
    X245.HDDCDD_constdds_bld_Subregions_Thailand <- get_data(all_data, "X245.HDDCDD_constdds_bld_Subregions_Thailand")



    # ===================================================

    # Produce outputs
    create_xml("building_breakout_Subregions_Thailand.xml") %>%
      add_xml_data(X245.DeleteConsumer_bld_Subregions_Thailand, "DeleteConsumer") %>%
      add_xml_data(X245.DeleteSupplysector_bld_Subregions_Thailand, "DeleteSupplysector") %>%
      add_xml_data(X245.SubregionalShares_bld_Subregions_Thailand, "SubregionalShares") %>%
      add_xml_data(X245.PriceExp_IntGains_bld_Subregions_Thailand, "PriceExp_IntGains") %>%
      add_xml_data(X245.Floorspace_bld_Subregions_Thailand, "Floorspace") %>%
      add_xml_data(X245.DemandFunction_serv_bld_Subregions_Thailand, "DemandFunction_serv") %>%
      add_xml_data(X245.DemandFunction_flsp_bld_Subregions_Thailand, "DemandFunction_flsp") %>%
      add_xml_data(X245.Satiation_flsp_bld_Subregions_Thailand, "Satiation_flsp") %>%
      add_xml_data(X245.SatiationAdder_bld_Subregions_Thailand, "SatiationAdder") %>%
      add_xml_data(X245.ThermalBaseService_bld_Subregions_Thailand, "ThermalBaseService") %>%
      add_xml_data(X245.GenericBaseService_bld_Subregions_Thailand, "GenericBaseService") %>%
      add_xml_data(X245.ThermalServiceSatiation_bld_Subregions_Thailand, "ThermalServiceSatiation") %>%
      add_xml_data(X245.GenericServiceSatiation_bld_Subregions_Thailand, "GenericServiceSatiation") %>%
      add_xml_data(X245.Intgains_scalar_bld_Subregions_Thailand, "Intgains_scalar") %>%
      add_xml_data(X245.ShellConductance_bld_Subregions_Thailand, "ShellConductance") %>%
      add_logit_tables_xml(X245.Supplysector_bld_Subregions_Thailand, "Supplysector") %>%
      add_xml_data(X245.FinalEnergyKeyword_bld_Subregions_Thailand, "FinalEnergyKeyword") %>%
      add_xml_data(X245.SubsectorShrwtFllt_bld_Subregions_Thailand, "SubsectorShrwtFllt") %>%
      add_xml_data(X245.SubsectorInterp_bld_Subregions_Thailand, "SubsectorInterp") %>%
      add_logit_tables_xml(X245.SubsectorLogit_bld_Subregions_Thailand, "SubsectorLogit") %>%
      add_xml_data(X245.StubTech_bld_Subregions_Thailand, "StubTech") %>%
      add_xml_data(X245.StubTechCalInput_bld_Subregions_Thailand, "StubTechCalInput") %>%
      add_xml_data(X245.StubTechMarket_bld_Subregions_Thailand, "StubTechMarket") %>%
      add_xml_data(X245.GlobalTechIntGainOutputRatio_bld_Subregions_Thailand, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(X245.GlobalTechInterpTo_bld_Subregions_Thailand, "GlobalTechInterpTo") %>%
      add_xml_data(X245.GlobalTechEff_bld_Subregions_Thailand, "GlobalTechEff") %>%
      add_xml_data(X245.GlobalTechShrwt_bld_Subregions_Thailand, "GlobalTechShrwt") %>%
      add_xml_data(X245.GlobalTechCost_bld_Subregions_Thailand, "GlobalTechCost") %>%
      add_xml_data(X245.GlobalTechSCurve_bld_Subregions_Thailand, "GlobalTechSCurve")  %>%
      add_xml_data(X245.HDDCDD_constdds_bld_Subregions_Thailand, "HDDCDD") %>%
      add_precursors("X245.DeleteConsumer_bld_Subregions_Thailand",
                     "X245.DeleteSupplysector_bld_Subregions_Thailand",
                     "X245.SubregionalShares_bld_Subregions_Thailand",
                     "X245.PriceExp_IntGains_bld_Subregions_Thailand",
                     "X245.Floorspace_bld_Subregions_Thailand",
                     "X245.DemandFunction_serv_bld_Subregions_Thailand",
                     "X245.DemandFunction_flsp_bld_Subregions_Thailand",
                     "X245.Satiation_flsp_bld_Subregions_Thailand",
                     "X245.SatiationAdder_bld_Subregions_Thailand",
                     "X245.ThermalBaseService_bld_Subregions_Thailand",
                     "X245.GenericBaseService_bld_Subregions_Thailand",
                     "X245.ThermalServiceSatiation_bld_Subregions_Thailand",
                     "X245.GenericServiceSatiation_bld_Subregions_Thailand",
                     "X245.Intgains_scalar_bld_Subregions_Thailand",
                     "X245.ShellConductance_bld_Subregions_Thailand",
                     "X245.Supplysector_bld_Subregions_Thailand",
                     "X245.FinalEnergyKeyword_bld_Subregions_Thailand",
                     "X245.SubsectorShrwtFllt_bld_Subregions_Thailand",
                     "X245.SubsectorInterp_bld_Subregions_Thailand",
                     "X245.SubsectorInterpTo_bld_Subregions_Thailand",
                     "X245.SubsectorLogit_bld_Subregions_Thailand",
                     "X245.StubTech_bld_Subregions_Thailand",
                     "X245.StubTechCalInput_bld_Subregions_Thailand",
                     "X245.StubTechMarket_bld_Subregions_Thailand",
                     "X245.GlobalTechIntGainOutputRatio_bld_Subregions_Thailand",
                     "X245.GlobalTechInterpTo_bld_Subregions_Thailand",
                     "X245.GlobalTechEff_bld_Subregions_Thailand",
                     "X245.GlobalTechShrwt_bld_Subregions_Thailand",
                     "X245.GlobalTechCost_bld_Subregions_Thailand",
                     "X245.GlobalTechSCurve_bld_Subregions_Thailand",
                     "X245.HDDCDD_constdds_bld_Subregions_Thailand") ->
      building_breakout_Subregions_Thailand.xml

    # # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(X245.SubsectorInterpTo_bld_Subregions_Thailand)) {

      building_breakout_Subregions_Thailand.xml %>%
        add_xml_data(X245.SubsectorInterpTo_bld_Subregions_Thailand, "SubsectorInterpTo") ->
        building_breakout_Subregions_Thailand.xml

      }

    return_data(building_breakout_Subregions_Thailand.xml)
  } else {
    stop("Unknown command")
  }
}
