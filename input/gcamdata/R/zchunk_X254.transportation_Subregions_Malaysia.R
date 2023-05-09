# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_X254.transportation_Subregions_Malaysia
#'
#' Creates level2 data for the building sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: check them out.
#' The corresponding file in the original data system was \code{L244.building_det.R} (energy level2).
#' @details Creates level2 data for the building sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete gather nesting unite
#' @author ZK 11 June 2021

module_energy_X254.transportation_Subregions_Malaysia <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X201.Pop_Subregions_Malaysia",
             "X201.GDP_Subregions_Malaysia",
             "L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwt",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.StubTranTechCalInput",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTranTechCoef",
             "L254.StubTechCalInput_passthru",
             "L254.StubTechProd_nonmotor",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L254.BaseService_trn",
             "L241.fgas_all_units",
             "L201.nonghg_max_reduction",
             "L201.nonghg_steepness",
             "L241.hfc_future",
             "L201.en_pol_emissions",
             "L201.en_ghg_emissions",
             FILE = "breakout/Subregions_Malaysia_trn_shares",
             FILE = "energy/A54.sector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
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
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    X201.Pop_Subregions_Malaysia <- get_data(all_data, "X201.Pop_Subregions_Malaysia", strip_attributes = TRUE)
    X201.GDP_Subregions_Malaysia <- get_data(all_data, "X201.GDP_Subregions_Malaysia", strip_attributes = TRUE)
    Subregions_Malaysia_trn_shares <- get_data(all_data, "breakout/Subregions_Malaysia_trn_shares")
    A54.sector <- get_data(all_data, "energy/A54.sector")
    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn", strip_attributes = TRUE)
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn", strip_attributes = TRUE)
    L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit", strip_attributes = TRUE)
    L254.tranSubsectorShrwt <- get_data(all_data, "L254.tranSubsectorShrwt", strip_attributes = TRUE)
    L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt", strip_attributes = TRUE)
    L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp", strip_attributes = TRUE)
    L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed", strip_attributes = TRUE)
    L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru", strip_attributes = TRUE)
    L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT", strip_attributes = TRUE)
    L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor", strip_attributes = TRUE)
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT", strip_attributes = TRUE)
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref", strip_attributes = TRUE)
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech", strip_attributes = TRUE)
    L254.StubTech_passthru <- get_data(all_data, "L254.StubTech_passthru", strip_attributes = TRUE)
    L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor", strip_attributes = TRUE)
    L254.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput", strip_attributes = TRUE)
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor", strip_attributes = TRUE)
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost", strip_attributes = TRUE)
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef", strip_attributes = TRUE)
    L254.StubTechCalInput_passthru <- get_data(all_data, "L254.StubTechCalInput_passthru", strip_attributes = TRUE)
    L254.StubTechProd_nonmotor <- get_data(all_data, "L254.StubTechProd_nonmotor", strip_attributes = TRUE)
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn", strip_attributes = TRUE)
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn", strip_attributes = TRUE)
    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn", strip_attributes = TRUE)
    L254.BaseService_trn <- get_data(all_data, "L254.BaseService_trn", strip_attributes = TRUE)

    L241.fgas_all_units <- get_data(all_data, "L241.fgas_all_units", strip_attributes = TRUE)
    L201.nonghg_max_reduction <- get_data(all_data, "L201.nonghg_max_reduction", strip_attributes = TRUE)
    L201.nonghg_steepness <- get_data(all_data, "L201.nonghg_steepness", strip_attributes = TRUE)
    L241.hfc_future <- get_data(all_data, "L241.hfc_future", strip_attributes = TRUE)
    L201.en_pol_emissions <- get_data(all_data, "L201.en_pol_emissions", strip_attributes = TRUE)
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions", strip_attributes = TRUE)

    # ===================================================

    Transport_sectors <- unique(L254.Supplysector_trn$supplysector)
    Transport_subsectors <- c(unique(L254.StubTranTechCalInput$tranSubsector),
                              unique(L254.StubTechCalInput_passthru$tranSubsector))
    Transport_sector_subsectors <- rbind(L254.StubTranTechCalInput %>%
                                           select(supplysector, tranSubsector) %>%
                                           unique(),
                                         L254.StubTechCalInput_passthru %>%
                                           select(supplysector, tranSubsector) %>%
                                           unique()) %>%
      mutate(supplysector.subsector = paste(supplysector, tranSubsector, sep = "."))

    X254.DeleteFinalDemand_trn_Subregions_Malaysia <- tibble(region = "Malaysia",
                                                  energy.final.demand = unique(L254.PerCapitaBased_trn$energy.final.demand))
    X254.DeleteSupplysector_trn_Subregions_Malaysia <- tibble(region = "Malaysia",
                                             supplysector = Transport_sectors)

    # First, subset the nonco2 tables to only transportation technologies
    L241.fgas_all_units <- filter(L241.fgas_all_units, supplysector %in% Transport_sectors)
    L201.nonghg_max_reduction <- filter(L201.nonghg_max_reduction, supplysector %in% Transport_sectors)
    L201.nonghg_steepness <- filter(L201.nonghg_steepness, supplysector %in% Transport_sectors)
    L241.hfc_future <- filter(L241.hfc_future, supplysector %in% Transport_sectors)

    # These tables should be split into 2 lists: one for tables with generic info that gets written to Subregions
    # and rest-of-Malaysia equivalently, and another that will apply shares to the data (e.g., energy consumption,
    # floorspace, etc) to parse the Malaysia total to the 2 disaggregated regions

    X254.list_nochange_data_Subregions_Malaysia <- list(
      L254.Supplysector_trn = L254.Supplysector_trn,
      L254.FinalEnergyKeyword_trn = L254.FinalEnergyKeyword_trn,
      L254.tranSubsectorLogit = L254.tranSubsectorLogit,
      L254.tranSubsectorShrwt = L254.tranSubsectorShrwt,
      L254.tranSubsectorShrwtFllt = L254.tranSubsectorShrwtFllt,
      L254.tranSubsectorInterp = L254.tranSubsectorInterp,
      L254.tranSubsectorSpeed = L254.tranSubsectorSpeed,
      L254.tranSubsectorSpeed_passthru = L254.tranSubsectorSpeed_passthru,
      L254.tranSubsectorSpeed_noVOTT = L254.tranSubsectorSpeed_noVOTT,
      L254.tranSubsectorSpeed_nonmotor = L254.tranSubsectorSpeed_nonmotor,
      L254.tranSubsectorVOTT = L254.tranSubsectorVOTT,
      L254.tranSubsectorFuelPref = L254.tranSubsectorFuelPref,
      L254.StubTranTech = L254.StubTranTech,
      L254.StubTech_passthru = L254.StubTech_passthru,
      L254.StubTech_nonmotor = L254.StubTech_nonmotor,
      L254.StubTranTechLoadFactor = L254.StubTranTechLoadFactor,
      L254.StubTranTechCost = L254.StubTranTechCost,
      L254.StubTranTechCoef = L254.StubTranTechCoef,
      L254.PerCapitaBased_trn = L254.PerCapitaBased_trn,
      L254.PriceElasticity_trn = L254.PriceElasticity_trn,
      L254.IncomeElasticity_trn = L254.IncomeElasticity_trn,
      L241.fgas_all_units = L241.fgas_all_units,
      L201.nonghg_max_reduction = L201.nonghg_max_reduction,
      L201.nonghg_steepness = L201.nonghg_steepness,
      L241.hfc_future = L241.hfc_future
      )

    subregions <- unique(X201.Pop_Subregions_Malaysia$region)
    subregions <- subregions[!subregions %in% c("Malaysia")]

    filter_to_core_scenario <- function(data){
      if("sce" %in% names(data)){
        data_new <- filter(data, sce == "CORE")
      } else {
        data_new <- data
      }
      return(data_new)
    }
    X254.list_nochange_data_Subregions_Malaysia <- lapply(X254.list_nochange_data_Subregions_Malaysia, FUN = filter_to_core_scenario)
    X254.list_nochange_data_Subregions_Malaysia <- lapply(X254.list_nochange_data_Subregions_Malaysia,
                                      FUN = write_to_breakout_regions,
                                      composite_region = "Malaysia",
                                      disag_regions = c(subregions))

    # get population and gdp shares of subregions

    X254.pop_gdp_share_Subregions_Malaysia <- X201.Pop_Subregions_Malaysia %>%
      left_join_error_no_match(X201.GDP_Subregions_Malaysia, by = c("region", "year")) %>%
      group_by(year) %>%
      dplyr::filter(region!="Malaysia") %>%
      mutate(popshare = totalPop / sum(totalPop),
             gdpshare = GDP / sum(GDP)) %>%
      ungroup() %>%
      select(region, year, popshare, gdpshare)


    # exogenous trn shares are by supplysector/subsector (but not all
    # supplysector/ subsector combinations may be included). Need to
    # write exogenous trn share data to all historical years and fill in
    # the supplysector/subsector combinations not included with population share

    trn_shares <- Subregions_Malaysia_trn_shares %>%
      mutate(year = min(MODEL_BASE_YEARS)) %>%
      rbind(Subregions_Malaysia_trn_shares %>% mutate(year = max(MODEL_BASE_YEARS))) %>%
      complete(nesting(region, supplysector, tranSubsector),
               year = MODEL_BASE_YEARS) %>%
      group_by(region, supplysector, tranSubsector) %>%
      mutate(share = approx_fun(year, share, rule = 2)) %>%
      ungroup() %>%
      # fill in missing supplysector- subsector combinations
      mutate(supplysector.subsector = paste(supplysector, tranSubsector, sep = ".")) %>%
      complete(nesting(region, year),
               supplysector.subsector = Transport_sector_subsectors$supplysector.subsector) %>%
      left_join(Transport_sector_subsectors,
                by = c("supplysector.subsector")) %>%
      mutate(supplysector = supplysector.y, tranSubsector = tranSubsector.y) %>%
      select(-c(supplysector.x, supplysector.y, tranSubsector.x, tranSubsector.y, supplysector.subsector)) %>%
      # fill in missing shares with population share
      left_join_error_no_match(X254.pop_gdp_share_Subregions_Malaysia,
                               by = c("region", "year")) %>%
      mutate(share = case_when(is.na(share) ~ popshare, T ~ share)) %>%
      select(-c(popshare, gdpshare))

    # get subregional shares of calibrated inputs using the new trn shares

    X254.StubTranTechCalInput_trn_Subregions_Malaysia <- L254.StubTranTechCalInput %>%
      filter(sce == "CORE") %>%
      downscale_to_breakout_regions_by_sector(data = .,
                                              composite_region = "Malaysia",
                                              disag_regions = c(subregions),
                                              share_data = trn_shares,
                                              value.column = "calibrated.value",
                                              share.column = "share",
                                              sector.columns = c("supplysector", "tranSubsector"))


    # nonmotor transportation modes can just be downscaled by population

    X254.StubTechProd_nonmotor <- L254.StubTechProd_nonmotor %>%
      filter(sce == "CORE") %>%
      downscale_to_breakout_regions(data = .,
                                    composite_region = "Malaysia",
                                    disag_regions = c(subregions),
                                    share_data = X254.pop_gdp_share_Subregions_Malaysia, value.column = "calOutputValue", share.column = "popshare",
                                    ndigits = energy.DIGITS_MPKM)

    # since shares differ by supplysector/subsector and X254.BaseService is by
    # energy.final.demand (more aggregated level), we need to recalculate
    # base service from calibrated inputs, intensities, and load factors

    X254.StubTranTechCalInput_trn_Subregions_Malaysia %>%
      select(-contains("share")) %>%
      left_join(X254.list_nochange_data_Subregions_Malaysia[["L254.StubTranTechLoadFactor"]],
                by = c("region", "supplysector", "tranSubsector",
                       "stub.technology", "year", "sce")) %>%
      left_join(X254.list_nochange_data_Subregions_Malaysia[["L254.StubTranTechCoef"]],
                by = c("region", "supplysector", "tranSubsector",
                       "stub.technology", "minicam.energy.input", "year","sce")) %>%
      mutate(loadFactor=if_else(is.na(loadFactor),0,loadFactor),
             coefficient=if_else(is.na(coefficient),0,coefficient),
             output = calibrated.value * loadFactor * CONV_EJ_GJ / (coefficient * CONV_BTU_KJ),
             output = if_else(is.na(output),0,output)) %>%
      select(region, supplysector, tranSubsector, stub.technology, year, minicam.energy.input,
             calibrated.value, loadFactor, coefficient, output,sce) ->
      L254.StubTranTechOutput_Subregions_Malaysia

    # aggregate outputs to energy.final.demand level
    L254.StubTranTechOutput_Subregions_Malaysia %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, output,sce) %>%
      bind_rows(
        select(X254.StubTechProd_nonmotor, one_of(LEVEL2_DATA_NAMES[["StubTranTech"]]), year, calOutputValue,sce)) %>%
      mutate(base.service = if_else(!is.na(output), output, calOutputValue)) %>%
      # Match in energy.final.demand from transportation supplysector information
      # NAs will be introduced, so use left-join
      left_join(A54.sector, by = "supplysector") %>%
      # Aggregate base-year service output to region, energy.final.demand, and year
      group_by(region, energy.final.demand, year,sce) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      filter(sce=="CORE") ->
      #kbn 2020-06-02 Base service values only needed for CORE.
      X254.BaseService_trn_Subregions_Malaysia # OUTPUT


    # need to recalculate pass thru sector calibrated inputs since these are combinations
    # of various sector-subsector combinations. The units are service output,
    # so we will start from the outputs calculated above
    X254.StubTechCalInput_passthru_trn_Subregions_Malaysia <- L254.StubTranTechOutput_Subregions_Malaysia %>%
      mutate(
        trn_pass_road_LDV.4W = case_when(tranSubsector %in% c("Car", "Large Car and Truck", "Mini Car") ~ 1, T ~ 0),
        trn_pass_road.LDV = case_when(trn_pass_road_LDV.4W | tranSubsector == "2W and 3W" ~ 1, T ~ 0),
        trn_pass.road = case_when(trn_pass_road.LDV | tranSubsector == "Bus" ~ 1, T ~ 0),
        trn_freight.road = case_when(grepl("truck", tranSubsector) ~ 1, T ~ 0)) %>%
      filter(trn_pass_road_LDV.4W | trn_pass_road.LDV | trn_pass.road | trn_freight.road) %>%
      tidyr::pivot_longer(c(trn_pass_road_LDV.4W, trn_pass_road.LDV, trn_pass.road, trn_freight.road),
                          names_to = "pass.thru.sector.subsector", values_to = "in.pass.thru.sector.subsector") %>%
      filter(in.pass.thru.sector.subsector == 1) %>%
      group_by(region, year, sce, pass.thru.sector.subsector) %>%
      summarize(calibrated.value = sum(output)) %>%
      ungroup() %>%
      separate(pass.thru.sector.subsector, into = c("supplysector", "tranSubsector"), sep = "\\.") %>%
      mutate(stub.technology = tranSubsector,
             minicam.energy.input = paste0(supplysector, "_", tranSubsector)) %>%
      left_join_error_no_match(unique(select(L254.StubTechCalInput_passthru,
                                             -c(region, calibrated.value))))


    # emissions inputs are by supplysector and subsector, so we can use the
    # same trn_shares data that we used for calibrated inputs (just need to
    # rename tranSubsector to subsector)

    X254.pol_emissions_trn_Subregions_Malaysia <- L201.en_pol_emissions %>%
      filter(supplysector %in% Transport_sectors) %>%
      downscale_to_breakout_regions_by_sector(data = .,
                                              composite_region = "Malaysia",
                                              disag_regions = c(subregions),
                                              share_data = rename(trn_shares, subsector = tranSubsector),
                                              value.column = "input.emissions",
                                              share.column = "share",
                                              sector.columns = c("supplysector", "subsector"))

    X254.ghg_emissions_trn_Subregions_Malaysia <- L201.en_ghg_emissions %>%
      filter(supplysector %in% Transport_sectors) %>%
      downscale_to_breakout_regions_by_sector(data = .,
                                              composite_region = "Malaysia",
                                              disag_regions = c(subregions),
                                              share_data = rename(trn_shares, subsector = tranSubsector),
                                              value.column = "input.emissions",
                                              share.column = "share",
                                              sector.columns = c("supplysector", "subsector"))


    # ===================================================
    # Produce outputs

    X254.DeleteFinalDemand_trn_Subregions_Malaysia %>%
      add_title("Delete energy-final-demand objects for industry sector in Malaysia") %>%
      add_units("Unitless") %>%
      add_comments("Malaysia region name hard-wired") %>%
      add_precursors("L254.PerCapitaBased_trn") ->
      X254.DeleteFinalDemand_trn_Subregions_Malaysia

    X254.DeleteSupplysector_trn_Subregions_Malaysia %>%
      add_title("Delete supplysector objects for industry sector in Malaysia") %>%
      add_units("Unitless") %>%
      add_comments("Malaysia region name hard-wired") %>%
      add_precursors("L254.Supplysector_trn") ->
      X254.DeleteSupplysector_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.Supplysector_trn"]] %>%
      add_title("Supplysector info for transportation") %>%
      add_units("NA") %>%
      add_comments("From A54.sector") %>%
      add_precursors("L254.Supplysector_trn") ->
      X254.Supplysector_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.FinalEnergyKeyword_trn"]] %>%
      add_title("Supply sector keywords for detailed industry sector") %>%
      add_units("NA") %>%
      add_comments("From A54.sector") %>%
      add_precursors("L254.FinalEnergyKeyword_trn") ->
      X254.FinalEnergyKeyword_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorLogit"]] %>%
      add_title("Subsector logit info for transportation") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorLogit") ->
      X254.tranSubsectorLogit_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorShrwt"]] %>%
      add_title("Subsector share-weight info for transportation") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorShrwt") ->
      X254.tranSubsectorShrwt_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorShrwtFllt"]] %>%
      add_title("Subsector share-weight fill-out info for transportation") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorShrwtFllt") ->
      X254.tranSubsectorShrwtFllt_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorInterp"]] %>%
      add_title("Subsector share-weight interpolation for transportation") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorInterp") ->
      X254.tranSubsectorInterp_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorSpeed"]] %>%
      add_title("Transportation speeds") %>%
      add_units("km/hr") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorSpeed") ->
      X254.tranSubsectorSpeed_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorSpeed_passthru"]] %>%
      add_title("Transportation speeds - pass-through subsectors") %>%
      add_units("km/hr") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorSpeed_passthru") ->
      X254.tranSubsectorSpeed_passthru_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorSpeed_noVOTT"]] %>%
      add_title("Default speeds for modes for which value of time in transport is not considered") %>%
      add_units("km/hr") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorSpeed_noVOTT") ->
      X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorSpeed_nonmotor"]] %>%
      add_title("Speeds of non-motorized modes") %>%
      add_units("km/yr") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorSpeed_nonmotor") ->
      X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorVOTT"]] %>%
      add_title("Value of time in transport multipliers for transportation modes") %>%
      add_units("unitless") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorVOTT") ->
      X254.tranSubsectorVOTT_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.tranSubsectorFuelPref"]] %>%
      add_title("Fuel preference elasticities by mode") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.tranSubsectorFuelPref") ->
      X254.tranSubsectorFuelPref_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.StubTranTech"]] %>%
      add_title("Stub technologies of transportation") %>%
      add_units("NA") %>%
      add_comments("From A54.sector") %>%
      add_precursors("L254.StubTranTech") ->
      X254.StubTranTech_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.StubTech_passthru"]] %>%
      add_title("Passthru stub technologies of transportation") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.StubTech_passthru") ->
      X254.StubTranTech_passthru_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.StubTech_nonmotor"]] %>%
      add_title("Stub technologies of non-motorized modes") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.StubTech_nonmotor") ->
      X254.StubTranTech_nonmotor_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.StubTranTechLoadFactor"]] %>%
      add_title("Load factors of transportation technologies") %>%
      add_units("persons/veh, tonnes/veh") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.StubTranTechLoadFactor") ->
      X254.StubTranTechLoadFactor_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.StubTranTechCost"]] %>%
      add_title("Costs of transportation technologies") %>%
      add_units("1990$/veh-km") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.StubTranTechCost") ->
      X254.StubTranTechCost_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.StubTranTechCoef"]] %>%
      add_title("Transportation technology input-output coefficients") %>%
      add_units("btu/veh-km") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.StubTranTechCoef") ->
      X254.StubTranTechCoef_trn_Subregions_Malaysia

    X254.StubTechProd_nonmotor %>%
      add_title("Calibrated output of non-motorized modes") %>%
      add_units("NA") %>%
      add_comments("Downscaled from parent region on the basis of population") %>%
      add_precursors("L254.StubTechProd_nonmotor") ->
      X254.StubTechProd_nonmotor_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.PerCapitaBased_trn"]] %>%
      add_title("Per-capita based demand flag") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.PerCapitaBased_trn") ->
      X254.PerCapitaBased_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.PriceElasticity_trn"]] %>%
      add_title("Price elasticity of transportation services") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.PriceElasticity_trn") ->
      X254.PriceElasticity_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L254.IncomeElasticity_trn"]] %>%
      add_title("Income elasticity of transportation services") %>%
      add_units("NA") %>%
      add_comments("Copied from parent region") %>%
      add_precursors("L254.IncomeElasticity_trn") ->
      X254.IncomeElasticity_trn_Subregions_Malaysia

    X254.StubTranTechCalInput_trn_Subregions_Malaysia %>%
      add_title("calibrated energy consumption by transportation") %>%
      add_units("EJ/yr") %>%
      add_comments("points to fuel markets in parent region") %>%
      add_precursors("L254.StubTranTechCalInput", "X201.Pop_Subregions_Malaysia", "X201.GDP_Subregions_Malaysia") ->
      X254.StubTranTechCalInput_trn_Subregions_Malaysia

    X254.StubTechCalInput_passthru_trn_Subregions_Malaysia %>%
      add_title("pass-thru transportation technology calibrated flows") %>%
      add_units("mpkm/yr, mtkm/yr") %>%
      add_comments("defaults to markets in own region") %>%
      add_precursors("L254.StubTechCalInput_passthru", "X201.Pop_Subregions_Malaysia", "X201.GDP_Subregions_Malaysia") ->
      X254.StubTechCalInput_passthru_trn_Subregions_Malaysia

    X254.BaseService_trn_Subregions_Malaysia %>%
      add_title("transportation base-service") %>%
      add_units("mpkm/yr, mtkm/yr") %>%
      add_comments("downscaled from parent region") %>%
      add_precursors("L254.BaseService_trn", "X201.Pop_Subregions_Malaysia", "X201.GDP_Subregions_Malaysia") ->
      X254.BaseService_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L241.fgas_all_units"]] %>%
      add_title("Units of f-gas emissions from transportation in Subregions_Malaysia") %>%
      add_units("Specified in table") %>%
      add_comments("Copied") %>%
      add_precursors("L241.fgas_all_units") ->
      X254.fgas_all_units_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L201.nonghg_max_reduction"]] %>%
      add_title("max-reduction of pollutant emissions from transportation in Subregions_Malaysia") %>%
      add_units("Percent") %>%
      add_comments("Copied") %>%
      add_precursors("L201.nonghg_max_reduction") ->
      X254.nonghg_max_reduction_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L201.nonghg_steepness"]] %>%
      add_title("steepness of pollutant emissions from transportation in Subregions_Malaysia") %>%
      add_units("Unitless") %>%
      add_comments("Copied") %>%
      add_precursors("L201.nonghg_steepness") ->
      X254.nonghg_steepness_trn_Subregions_Malaysia

    X254.list_nochange_data_Subregions_Malaysia[["L241.hfc_future"]] %>%
      add_title("emissions coefficients of f-gases from transportation in Subregions_Malaysia") %>%
      add_units("g f-gas per GJ of building energy service") %>%
      add_comments("Copied") %>%
      add_precursors("L241.hfc_future") ->
      X254.hfc_future_trn_Subregions_Malaysia

    X254.pol_emissions_trn_Subregions_Malaysia %>%
      add_title("pollutant emissions from Subregions_Malaysia buildings") %>%
      add_units("Tg/yr") %>%
      add_comments("downscaled from Malaysia using GDP shares") %>%
      add_precursors("L201.en_pol_emissions") ->
      X254.pol_emissions_trn_Subregions_Malaysia

    X254.ghg_emissions_trn_Subregions_Malaysia %>%
      add_title("greenhouse gas emissions from Subregions_Malaysia buildings") %>%
      add_units("Tg/yr") %>%
      add_comments("downscaled from Malaysia using GDP shares") %>%
      add_precursors("L201.en_ghg_emissions") ->
      X254.ghg_emissions_trn_Subregions_Malaysia

    return_data(X254.DeleteFinalDemand_trn_Subregions_Malaysia,
                X254.DeleteSupplysector_trn_Subregions_Malaysia,
                X254.Supplysector_trn_Subregions_Malaysia,
                X254.FinalEnergyKeyword_trn_Subregions_Malaysia,
                X254.tranSubsectorLogit_trn_Subregions_Malaysia,
                X254.tranSubsectorShrwt_trn_Subregions_Malaysia,
                X254.tranSubsectorShrwtFllt_trn_Subregions_Malaysia,
                X254.tranSubsectorInterp_trn_Subregions_Malaysia,
                X254.tranSubsectorSpeed_trn_Subregions_Malaysia,
                X254.tranSubsectorSpeed_passthru_trn_Subregions_Malaysia,
                X254.tranSubsectorSpeed_noVOTT_trn_Subregions_Malaysia,
                X254.tranSubsectorSpeed_nonmotor_trn_Subregions_Malaysia,
                X254.tranSubsectorVOTT_trn_Subregions_Malaysia,
                X254.tranSubsectorFuelPref_trn_Subregions_Malaysia,
                X254.StubTranTech_trn_Subregions_Malaysia,
                X254.StubTranTech_passthru_trn_Subregions_Malaysia,
                X254.StubTranTech_nonmotor_trn_Subregions_Malaysia,
                X254.StubTranTechCalInput_trn_Subregions_Malaysia,
                X254.StubTranTechLoadFactor_trn_Subregions_Malaysia,
                X254.StubTranTechCost_trn_Subregions_Malaysia,
                X254.StubTranTechCoef_trn_Subregions_Malaysia,
                X254.StubTechCalInput_passthru_trn_Subregions_Malaysia,
                X254.StubTechProd_nonmotor_trn_Subregions_Malaysia,
                X254.PerCapitaBased_trn_Subregions_Malaysia,
                X254.PriceElasticity_trn_Subregions_Malaysia,
                X254.IncomeElasticity_trn_Subregions_Malaysia,
                X254.BaseService_trn_Subregions_Malaysia,
                X254.fgas_all_units_trn_Subregions_Malaysia,
                X254.nonghg_max_reduction_trn_Subregions_Malaysia,
                X254.nonghg_steepness_trn_Subregions_Malaysia,
                X254.hfc_future_trn_Subregions_Malaysia,
                X254.pol_emissions_trn_Subregions_Malaysia,
                X254.ghg_emissions_trn_Subregions_Malaysia)
  } else {
    stop("Unknown command")
  }
}
