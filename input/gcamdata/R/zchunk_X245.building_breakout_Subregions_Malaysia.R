# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamseasia_X245.building_breakout_Subregions_Malaysia
#'
#' Creates GCAM-SEAsia building output files for writing to xml.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{X245.DeleteConsumer_bld_Subregions_Malaysia}, \code{X245.DeleteSupplysector_bld_Subregions_Malaysia}, \code{X245.SubregionalShares_bld_Subregions_Malaysia},
#' \code{X245.PriceExp_IntGains_bld_Subregions_Malaysia}, \code{X245.Floorspace_bld_Subregions_Malaysia}, \code{X245.DemandFunction_serv_bld_Subregions_Malaysia}, \code{X245.DemandFunction_flsp_bld_Subregions_Malaysia},
#' \code{X245.Satiation_flsp_bld_Subregions_Malaysia}, \code{X245.SatiationAdder_bld_Subregions_Malaysia}, \code{X245.ThermalBaseService_bld_Subregions_Malaysia}, \code{X245.GenericBaseService_bld_Subregions_Malaysia},
#' \code{X245.ThermalServiceSatiation_bld_Subregions_Malaysia}, \code{X245.GenericServiceSatiation_bld_Subregions_Malaysia}, \code{X245.Intgains_scalar_bld_Subregions_Malaysia},
#' \code{X245.ShellConductance_bld_Subregions_Malaysia}, \code{X245.Supplysector_bld_Subregions_Malaysia}, \code{X245.FinalEnergyKeyword_bld_Subregions_Malaysia}, \code{X245.SubsectorShrwt_bld_Subregions_Malaysia},
#' \code{X245.SubsectorShrwtFllt_bld_Subregions_Malaysia}, \code{X245.SubsectorInterp_bld_Subregions_Malaysia}, \code{X245.SubsectorInterpTo_bld_Subregions_Malaysia},
#' \code{X245.SubsectorLogit_bld_Subregions_Malaysia}, \code{X245.StubTech_bld_Subregions_Malaysia}, \code{X245.StubTechCalInput_bld_Subregions_Malaysia}, \code{X245.StubTechMarket_bld_Subregions_Malaysia},
#' \code{X245.GlobalTechIntGainOutputRatio_bld_Subregions_Malaysia}, \code{X245.GlobalTechInterpTo_bld_Subregions_Malaysia}, \code{X245.GlobalTechEff_bld_Subregions_Malaysia},
#' \code{X245.GlobalTechShrwt_bld_Subregions_Malaysia}, \code{X245.GlobalTechCost_bld_Subregions_Malaysia}, \code{X245.GlobalTechSCurve_bld_Subregions_Malaysia}, \code{X245.HDDCDD_A2_GFDL_bld_Subregions_Malaysia},
#' \code{X245.HDDCDD_AEO_2015_bld_Subregions_Malaysia}, \code{X245.HDDCDD_constdds_bld_Subregions_Malaysia}, \code{X245.GompFnParam_Malaysia}.
#' The corresponding file in the original data system was \code{X245.building_USA.R} (gcam-usa level2).
#' @details Creates GCAM-SEAsia building output files for writing to xml.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete gather nesting replace_na crossing
#' @author RLH November 2017, MAW April 2021

module_gcamseasia_X245.building_breakout_Subregions_Malaysia <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "energy/A44.gcam_consumer",
             FILE = "energy/A44.sector",
             FILE = "gcam-seasia/IND_bld_techs",
             FILE = "gcam-seasia/IND_A44_shell",
             FILE = "gcam-seasia/IND_A44_demandFn_flsp",
             FILE = "gcam-seasia/IND_A44_demandFn_serv",
             FILE = "gcam-seasia/A44.gcam_consumer",
             FILE = "gcam-seasia/IND_A44_satiation_flsp",
             FILE = "gcam-seasia/IND_A44_sector",
             FILE = "gcam-seasia/IND_A44_subsector_interp",
             FILE = "gcam-seasia/IND_A44_subsector_logit",
             FILE = "gcam-seasia/IND_A44_subsector_shrwt",
             FILE = "gcam-seasia/IND_A44_tech_cost",
             FILE = "gcam-seasia/IND_A44_tech_eff",
             FILE = "gcam-seasia/IND_A44_tech_eff_avg",
             FILE = "gcam-seasia/IND_A44_globaltech_shares",
             FILE = "gcam-seasia/IND_A44_tech_intgains",
             FILE = "gcam-seasia/IND_A44_tech_retirement",
             FILE = "gcam-seasia/IND_A44_tech_shrwt",
             FILE = "gcam-seasia/IND_A44_tech_interp",
             FILE = "gcam-seasia/IND_A44_demand_satiation_mult",
             FILE = "gcam-seasia/IND_A44_subregional_shares",
             FILE = "gcam-seasia/Various_flsp_Mm2",
             FILE = "gcam-seasia/IESS_bld_serv_fuel_Malaysia",
             "X244.Floorspace_Subregions_Malaysia",
             "X244.Satiation_flsp_Subregions_Malaysia",
             "X244.StubTechCalInput_bld_Subregions_Malaysia",
             "L143.HDDCDD_scen_R_Y",
             "X201.Pop_Subregions_Malaysia",
             "X201.GDP_Subregions_Malaysia",
             "L244.GompFnParam"))
  } else if(command == driver.DECLARE_OUTPUTS) {
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
             "X245.SubsectorShrwt_bld_Subregions_Malaysia",
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
             "X245.HDDCDD_A2_GFDL_bld_Subregions_Malaysia",
             "X245.HDDCDD_constdds_bld_Subregions_Malaysia",
             "X245.GompFnParam_Malaysia"
             ))
  } else if(command == driver.MAKE) {

    # Silence package checks
    GCM <- Scen <- base.building.size <- base.service <- calibrated.value <- comm <-
      degree.days <- efficiency <- efficiency_tech1 <- efficiency_tech2 <- fuel <-
      gcam.consumer <- grid_region <- half_life_new <- half_life_stock <- input.cost <-
      input.ratio <- internal.gains.market.name <- internal.gains.output.ratio <-
      internal.gains.scalar <- market.name <- minicam.energy.input <- multiplier <-
      object <- pcFlsp_mm2 <- pcGDP <- pcflsp_mm2cap <- pop <- region <- resid <-
      satiation.adder <- satiation.level <- sector <- sector.name <- service <- share <-
      share.weight <- share_tech1 <- share_tech2 <- share_type <- state <- steepness_new <-
      steepness_stock <- stockavg <- subsector <- subsector.name <- supplysector <-
      tech_type <- technology <- technology1 <- technology2 <-
      thermal.building.service.input <- to.value <- value <- year <- year.fillout <- . <-
      pop_year <- Sector <- pop_share <- growth <- flsp_growth <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID", strip_attributes = TRUE)
    A44.gcam_consumer_en <- get_data(all_data, "energy/A44.gcam_consumer", strip_attributes = TRUE)
    A44.sector_en <- get_data(all_data, "energy/A44.sector", strip_attributes = TRUE)
    IND_bld_techs <- get_data(all_data, "gcam-seasia/IND_bld_techs", strip_attributes = TRUE)
    IND_A44_shell <- get_data(all_data, "gcam-seasia/IND_A44_shell", strip_attributes = TRUE)
    IND_A44_demandFn_flsp <- get_data(all_data, "gcam-seasia/IND_A44_demandFn_flsp", strip_attributes = TRUE)
    IND_A44_demandFn_serv <- get_data(all_data, "gcam-seasia/IND_A44_demandFn_serv", strip_attributes = TRUE)
    A44.gcam_consumer <- get_data(all_data, "gcam-seasia/A44.gcam_consumer", strip_attributes = TRUE)
    IND_A44_satiation_flsp <- get_data(all_data, "gcam-seasia/IND_A44_satiation_flsp", strip_attributes = TRUE)
    IND_A44_sector <- get_data(all_data, "gcam-seasia/IND_A44_sector", strip_attributes = TRUE)
    IND_A44_subsector_interp <- get_data(all_data, "gcam-seasia/IND_A44_subsector_interp", strip_attributes = TRUE)
    IND_A44_subsector_logit <- get_data(all_data, "gcam-seasia/IND_A44_subsector_logit", strip_attributes = TRUE)
    IND_A44_subsector_shrwt <- get_data(all_data, "gcam-seasia/IND_A44_subsector_shrwt", strip_attributes = TRUE)
    IND_A44_tech_cost <- get_data(all_data, "gcam-seasia/IND_A44_tech_cost", strip_attributes = TRUE)
    IND_A44_tech_eff <- get_data(all_data, "gcam-seasia/IND_A44_tech_eff", strip_attributes = TRUE) %>%
      gather_years()
    IND_A44_tech_eff_avg <- get_data(all_data, "gcam-seasia/IND_A44_tech_eff_avg", strip_attributes = TRUE)
    IND_A44_globaltech_shares <- get_data(all_data, "gcam-seasia/IND_A44_globaltech_shares", strip_attributes = TRUE)
    IND_A44_tech_intgains <- get_data(all_data, "gcam-seasia/IND_A44_tech_intgains", strip_attributes = TRUE)
    IND_A44_tech_retirement <- get_data(all_data, "gcam-seasia/IND_A44_tech_retirement", strip_attributes = TRUE)
    IND_A44_tech_shrwt <- get_data(all_data, "gcam-seasia/IND_A44_tech_shrwt", strip_attributes = TRUE)
    IND_A44_tech_interp <- get_data(all_data, "gcam-seasia/IND_A44_tech_interp", strip_attributes = TRUE)
    IND_A44_demand_satiation_mult <- get_data(all_data, "gcam-seasia/IND_A44_demand_satiation_mult", strip_attributes = TRUE)
    IND_A44_subregional_shares <- get_data(all_data, "gcam-seasia/IND_A44_subregional_shares", strip_attributes = TRUE)
    Various_flsp_Mm2 <- get_data(all_data, "gcam-seasia/Various_flsp_Mm2", strip_attributes = TRUE)
    IESS_bld_serv_fuel_Malaysia <- get_data(all_data, "gcam-seasia/IESS_bld_serv_fuel_Malaysia", strip_attributes = TRUE)
    X244.Floorspace_Subregions_Malaysia <- get_data(all_data, "X244.Floorspace_Subregions_Malaysia", strip_attributes = TRUE)
    X244.Satiation_flsp_Subregions_Malaysia <- get_data(all_data, "X244.Satiation_flsp_Subregions_Malaysia", strip_attributes = TRUE)
    X244.StubTechCalInput_bld_Subregions_Malaysia <- get_data(all_data, "X244.StubTechCalInput_bld_Subregions_Malaysia", strip_attributes = TRUE)
    L143.HDDCDD_scen_R_Y <- get_data(all_data, "L143.HDDCDD_scen_R_Y", strip_attributes = TRUE)
    X201.Pop_Subregions_Malaysia <- get_data(all_data, "X201.Pop_Subregions_Malaysia", strip_attributes = TRUE)
    X201.GDP_Subregions_Malaysia <- get_data(all_data, "X201.GDP_Subregions_Malaysia", strip_attributes = TRUE)
    L244.GompFnParam <- get_data(all_data, "L244.GompFnParam", strip_attributes = TRUE)

    # ===================================================
    # Data Processing

    # Note: Building energy demands and floorspace are calculated endogenously - these are undergoing review
    # per-capita demand = (satiation.level - satiation.adder) * (1 - exp( -log2 / satiation.impedance * Demand.Driver)) + satiation.adder)
    #
    # floorspace = (satiation.level - satiation.adder) *
    # [1 - exp{(-ln(2) * per-capita-GDP/satiation.impedance) * (energy_cost/base_energy_cost)^price_effect_exponent}] + satiation.adder
    #
    # satiation.level: maximum per-capita demand that can be achieved
    # satiation.adder: value that allow the starting position of any region to be set along the demand function
    # satiation.impedance: shape parameter

    # ===================================================
    # 0. Pre-Processing
    # ===================================================
    # Need to delete the buildings sector at the national level (gcam.consumers and supplysectors)
    X245.DeleteConsumer_bld_Subregions_Malaysia <- tibble(region = gcam.Malaysia.parentregion, gcam.consumer = A44.gcam_consumer_en$gcam.consumer)
    X245.DeleteSupplysector_bld_Subregions_Malaysia <- tibble(region = gcam.Malaysia.parentregion, supplysector = A44.sector_en$supplysector) %>%
      # removing heating, as these nodes do not exist
      filter( !grepl( "heating", supplysector ) )

    # Assign subregional shares
    # Manipulate subregional shares input to contain relevant info
    subregional_shares_manipulate <- IND_A44_subregional_shares %>%
      filter( region %in% gcam.Malaysia.subregions,
              grepl( "share", item ) ) %>%
      gather_years() %>%
      mutate( value = value / 100,
              rural_share = 1 - value ) %>%
      rename( urban_share = value ) %>%
      select( -c( scenario, unit ) )

    # urban shares
    urban_subregional_pop_shares <- subregional_shares_manipulate %>%
      filter( grepl( "population", item ) ) %>%
      select( -c( item, rural_share ) ) %>%
      mutate( gcam.consumer = "resid urban" ) %>%
      rename( pop.year.fillout = year,
              subregional.population.share = urban_share )

    urban_subregional_income_shares <- subregional_shares_manipulate %>%
      filter( grepl( "GDP", item ) ) %>%
      select( -c( item, rural_share ) ) %>%
      mutate( gcam.consumer = "resid urban" ) %>%
      rename( pop.year.fillout = year,
              subregional.income.share = urban_share )

    urban_subregional_shares <- urban_subregional_pop_shares %>%
      left_join_error_no_match( urban_subregional_income_shares,
                                by = c( "region", "pop.year.fillout", "gcam.consumer" ) ) %>%
      complete( nesting( region, gcam.consumer ), pop.year.fillout = MODEL_YEARS ) %>%
      group_by( region, gcam.consumer ) %>%
      mutate( subregional.population.share = approx_fun( pop.year.fillout, subregional.population.share, rule = 2 ),
              subregional.income.share = approx_fun( pop.year.fillout, subregional.income.share, rule = 2 ),
              inc.year.fillout = pop.year.fillout )

    # rural shares
    rural_subregional_pop_shares <- subregional_shares_manipulate %>%
      filter( grepl( "population", item ) ) %>%
      select( -c( item, urban_share ) ) %>%
      mutate( gcam.consumer = "resid rural" ) %>%
      rename( pop.year.fillout = year,
              subregional.population.share = rural_share )

    rural_subregional_income_shares <- subregional_shares_manipulate %>%
      filter( grepl( "GDP", item ) ) %>%
      select( -c( item, urban_share ) ) %>%
      mutate( gcam.consumer = "resid rural" ) %>%
      rename( pop.year.fillout = year,
              subregional.income.share = rural_share )

    rural_subregional_shares <- rural_subregional_pop_shares %>%
      left_join_error_no_match( rural_subregional_income_shares,
                                by = c( "region", "pop.year.fillout", "gcam.consumer" ) ) %>%
      complete( nesting( region, gcam.consumer ), pop.year.fillout = MODEL_YEARS ) %>%
      group_by( region, gcam.consumer ) %>%
      mutate( subregional.population.share = approx_fun( pop.year.fillout, subregional.population.share, rule = 2 ),
              subregional.income.share = approx_fun( pop.year.fillout, subregional.income.share, rule = 2 ),
              inc.year.fillout = pop.year.fillout )

    X245.SubregionalShares_bld_Subregions_Malaysia <- write_to_all_states(A44.gcam_consumer, c("region", "gcam.consumer"),
                                                                       region_list = gcam.Malaysia.subregions ) %>%
      filter( gcam.consumer == "comm" ) %>%
      repeat_add_columns( tibble::tibble( year = MODEL_YEARS ) ) %>%
      mutate( pop.year.fillout = year,
             subregional.population.share = 1,
             subregional.income.share = 1) %>%
      rename( inc.year.fillout = year ) %>%
      bind_rows( urban_subregional_shares, rural_subregional_shares )


    # ===================================================
    # 0.5. Population
    # ===================================================
    # Bind historical and future population data and filter for the target region(s)
    # Select SSP scenario, default is gSSP2 (business as usual)
    X245.Pop_thous <- X201.Pop_Subregions_Malaysia %>%
      filter( region %in% gcam.Malaysia.subregions ) %>%
      # rename columns
      rename( value = totalPop )

    # X245.PriceExp_IntGains_bld_Subregions_Malaysia: price exponent on floorspace and naming of internal gains trial markets
    X245.PriceExp_IntGains_bld_Subregions_Malaysia <- write_to_all_states(A44.gcam_consumer, LEVEL2_DATA_NAMES[["PriceExp_IntGains"]],
                                                          region_list = gcam.Malaysia.subregions )

    # ===================================================
    # 1. Floorspace
    # ===================================================
    # X245.Floorspace_bld_Subregions_Malaysia: base year floorspace
    # Keep all historical years for now - these are needed in calculating satiation adders later on

    # ===================================================
    # 1.1. Residential Floorspace
    # ===================================================
    X245.Floorspace_resid <- X244.Floorspace_Subregions_Malaysia %>%
      # filter for target region(s)
      filter( region %in% gcam.Malaysia.subregions,
             gcam.consumer == "resid" ) %>%
      # add column that contains gcam.consumer
      mutate( base.building.size = round( base.building.size, energy.DIGITS_FLOORSPACE ) )

    # ===================================================
    # 1.1.1 split residential floorspace into urban and rural
    # ===================================================
    # Apportion to urban and rural on the basis of the historical data
    # TODO: these assumptions should be changed for cities. All residential floorspace urban?
    # using just two endpoints and interpolating in between years because data is missing for some years
    # TODO: use actual data instead of interpolation if we have more recent/complete floorspace data
    flsp_shares <- Various_flsp_Mm2 %>%
      mutate( res_total_bm2 = ( urban + rural ) * CONV_MIL_BIL,
              urban_share = urban * CONV_MIL_BIL / res_total_bm2,
              rural_share = 1 - urban_share ) %>%
      filter( !is.na( rural ) )

    # years to be used to interpolate between
    first_scale_year <- min( flsp_shares$year )
    last_scale_year <- max( flsp_shares$year )

    # values to be used to interpolate between
    rural_share_initial <- ( flsp_shares$rural_share[ flsp_shares$year == first_scale_year ] )
    rural_share_final <- ( flsp_shares$rural_share[ flsp_shares$year == last_scale_year ] )

    # Linearly interpolate residential shares between the initial and final years/shares
     res_rural_shares <- flsp_shares %>%
    #   # remove all columns except for year
       select( year ) %>%
       mutate( share = case_when( year == first_scale_year ~ rural_share_initial,
                                  year == last_scale_year ~ rural_share_final,
                                  TRUE ~  as.numeric( NA ) ),
               share = approx( x = year,
                                y = share,
                                xout = year,
                                rule = 2 )$y)

    # copy the minimum year rural share back to all other historical years, and the max year rural share to all future years
    res_rural_shares_his <- res_rural_shares %>%
      filter( year == min( year ) ) %>%
      select( -year ) %>%
      repeat_add_columns( tibble::tibble( year = MODEL_YEARS ) ) %>%
      filter( year <= first_scale_year )

    res_rural_shares_fut <- res_rural_shares %>%
      filter( year == max( year ) ) %>%
      select( -year ) %>%
      repeat_add_columns( tibble::tibble( year = MODEL_YEARS ) ) %>%
      filter( year >= last_scale_year )

    # bind all years into one dataframe
    res_shares_all <- res_rural_shares_his %>%
      bind_rows( res_rural_shares, res_rural_shares_fut ) %>%
      rename( rural_share = share ) %>%
      mutate( urban_share = 1 - rural_share )

    # Multiply the residential floorspace total by the rural and urban shares
    X245.Floorspace_resid_rural_Yh <- X245.Floorspace_resid %>%
    # can't use a left_join_error_no_match due to differing years
      left_join( res_shares_all, by = c( "year" ) ) %>%
      mutate(  base.building.size = base.building.size * rural_share,
               gcam.consumer = "resid rural",
               nodeInput = "resid rural",
               building.node.input = "resid_rural_building" ) %>%
      select( -c( rural_share, urban_share ) )

    X245.Floorspace_resid_urban_Yh <- X245.Floorspace_resid %>%
      # can't use a left_join_error_no_match due to differing years
      left_join( res_shares_all, by = c( "year" ) ) %>%
      mutate(  base.building.size = base.building.size * urban_share,
               gcam.consumer = "resid urban",
               nodeInput = "resid urban",
               building.node.input = "resid_urban_building" ) %>%
      select( -c( rural_share, urban_share ) )

    # bind rural and urban tables
    X245.Floorspace_resid_Yh <- X245.Floorspace_resid_rural_Yh %>%
      bind_rows( X245.Floorspace_resid_urban_Yh ) %>%
      # filter for GCAM years
      filter( year %in% MODEL_YEARS )

    # ===================================================
    # 1.2. Commercial Floorspace
    # ===================================================
    X245.Floorspace_comm <- X244.Floorspace_Subregions_Malaysia %>%
      # filter for target region(s)
      filter( region %in% gcam.Malaysia.subregions,
              gcam.consumer == "comm" ) %>%
      mutate( base.building.size = round(base.building.size, energy.DIGITS_FLOORSPACE))

    X245.Floorspace_bld_Subregions_Malaysia <- bind_rows(X245.Floorspace_resid_Yh, X245.Floorspace_comm)

    # X245.DemandFunction_serv_bld_Subregions_Malaysia and X245.DemandFunction_flsp_bld_Subregions_Malaysia: demand function types
    X245.DemandFunction_serv_bld_Subregions_Malaysia <- write_to_all_states(IND_A44_demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]],
                                                            region_list = gcam.Malaysia.subregions )
    X245.DemandFunction_flsp_bld_Subregions_Malaysia <- write_to_all_states(IND_A44_demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]],
                                                            region_list = gcam.Malaysia.subregions )

    # ===================================================
    # 1.3. Floorspace Satiation Levels
    # ===================================================
    # X245.Satiation_flsp_bld_Subregions_Malaysia: Satiation levels assumed for floorspace
    # TODO: check on this. Units of original input VS units in data system
    satiation_flsp <- X244.Satiation_flsp_Subregions_Malaysia %>%
      filter( region %in% gcam.Malaysia.subregions ) %>%
      mutate( satiation.level = satiation.level * 1000000 ) %>%
      select( -c( nodeInput, building.node.input ) ) %>%
      spread( gcam.consumer, satiation.level )

    X245.Satiation_flsp_bld_Subregions_Malaysia <- satiation_flsp %>%
      tidyr::gather( gcam.consumer, value, comm ) %>%
      # Need to make sure that the satiation level is greater than the floorspace in the final base year
      left_join_error_no_match(X245.Floorspace_bld_Subregions_Malaysia %>%
                                 filter(year == max(MODEL_BASE_YEARS)), by = c("region", "gcam.consumer")) %>%
      left_join_error_no_match(X245.Pop_thous %>% rename(pop = value), by = c("region", "year")) %>%
      mutate(year = as.integer(year),
             # value.y = population
             pcflsp_mm2cap = base.building.size / pop,
             # Satiation level = must be greater than the observed value in the final calibration year, so if observed value is
             # greater than calculated, multiply observed by 1.001
             satiation.level = round(pmax(value * CONV_THOUS_BIL, pcflsp_mm2cap * 1.001), energy.DIGITS_SATIATION_ADDER)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = c("gcam.consumer", "nodeInput", "building.node.input")) %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], "satiation.level")

    # X245.SatiationAdder_bld_Subregions_Malaysia: Satiation adders in floorspace demand function
    # Required for shaping the future floorspace growth trajectories in each region
    # Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)

    # We will filter GDP to energy.SATIATION_YEAR, but this may be greater than the historical years present
    # under timeshift conditions. So we adjust energy.SATIATION_YEAR
    energy.SATIATION_YEAR <- min(max(MODEL_BASE_YEARS), energy.SATIATION_YEAR)

    X245.SatiationAdder_bld_Subregions_Malaysia <- X201.GDP_Subregions_Malaysia %>%
      # filter for target region(s)
      filter( region %in% gcam.Malaysia.subregions,
              year == energy.SATIATION_YEAR ) %>%
      # convert from million to thousand
      mutate( GDP = GDP * CONV_MIL_THOUS ) %>%
      left_join( X245.Satiation_flsp_bld_Subregions_Malaysia, by = c( "region") ) %>%
      # Add floorspace
      left_join_error_no_match(X245.Floorspace_bld_Subregions_Malaysia, by = c("region", "gcam.consumer", "year", "nodeInput", "building.node.input")) %>%
      # Add population
      left_join_error_no_match(X245.Pop_thous, by = c("region", "year")) %>%
      rename( pop = value ) %>%
      # dividing by 1000 to get per capita instead of per 1,000 people
      mutate( pcGDP = ( GDP / pop ) / 1000 ) %>%
      # Calculate per capita floorspace
      mutate(pcFlsp_mm2 = base.building.size / pop,
             # Calculate the satiation adders
             satiation.adder = round(satiation.level - (
               exp(log(2) * pcGDP / energy.GDP_MID_SATIATION) * (satiation.level - pcFlsp_mm2)),
               energy.DIGITS_SATIATION_ADDER),
             # The satiation adder (million square meters of floorspace per person) needs to be less than the per-capita demand in the final calibration year
             satiation.adder = if_else(satiation.adder > pcFlsp_mm2, pcFlsp_mm2 * 0.999, satiation.adder)) %>%
      select(LEVEL2_DATA_NAMES[["SatiationAdder"]])

    # ===================================================
    # 2. Heating and Cooling Degree Days (thermal services only)
    # ===================================================
    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(IND_A44_tech_intgains$supplysector)
    thermal_services <- dplyr::setdiff(unique(IND_A44_sector$supplysector), generic_services)

    # X245.HDDCDD: Heating and cooling degree days by scenario
    X245.HDDCDD_scen <- L143.HDDCDD_scen_R_Y %>%
      left_join_error_no_match( GCAM_region_names, by = c( "GCAM_region_ID" ) ) %>%
      # filter for target region(s)
      filter( region == gcam.Malaysia.parentregion ) %>%
      # select relevant columns
      select( -c( GCAM_region_ID ) ) %>%
      rename(degree.days = value)

    # Let's make a climate normal (historical average) for each region, using a selected interval of years
    # Don't want to just set one year, because we want average values for all regions
    X245.HDDCDD_normal <- X245.HDDCDD_scen %>%
      filter(year %in% seq(1981, 2000),
             # The AEO_2015 scenario changes this "normal climate" for each region,
             # which is not desirable since it does not include historical data
             # and is not the standard reference assumption.  Thus, we remove it
             # from this calculation.
             SRES != "AEO_2015") %>%
      group_by(region, variable) %>%
      summarise(degree.days = mean(degree.days)) %>%
      ungroup() %>%
      # rename to target region(s)
      write_to_all_states(c( "region", "variable", "degree.days" ),
                        region_list = gcam.Malaysia.subregions )

    # X245.HDDCDD: Heating and cooling degree days for USA, used to calculate internal gains
    X245.HDDCDD_normal_USA <- L143.HDDCDD_scen_R_Y %>%
      left_join_error_no_match( GCAM_region_names, by = c( "GCAM_region_ID" ) ) %>%
      # filter for target region(s)
      filter( region == gcam.USA_REGION ) %>%
      # select relevant columns
      select( -c( GCAM_region_ID ) ) %>%
      rename(degree.days.USA = value) %>%
      filter(year %in% seq(1981, 2000),
             # The AEO_2015 scenario changes this "normal climate" for each region,
             # which is not desirable since it does not include historical data
             # and is not the standard reference assumption.  Thus, we remove it
             # from this calculation.
             SRES != "AEO_2015") %>%
      group_by(region, variable) %>%
      summarise(degree.days.USA = mean(degree.days.USA)) %>%
      ungroup() %>%
      # remove region column for left_joining
      select( -region )

    # Subset the heating and cooling services, separately
    heating_services <- thermal_services[grepl("heating", thermal_services)]
    cooling_services <- thermal_services[grepl("cooling", thermal_services)]

    X245.HDDCDD_temp <- tidyr::crossing(region = gcam.Malaysia.parentregion, thermal.building.service.input = thermal_services) %>%
      # Add in gcam.consumer
      left_join_error_no_match(IND_bld_techs %>%
                                 select(service, gcam.consumer = sector) %>%
                                 distinct(), by = c("thermal.building.service.input" = "service")) %>%
      # Add in nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], thermal.building.service.input) %>%
      # Add in model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Add HDD/CDD so that we can join with X245.HDDCDD_scen_state, remove at end
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add in degree days
      # X245.HDDCDD_scen_state has multiple scenarios, rows in this tbl_df are intended to be duplicated for each scenario
      # left_join_error_no_match throws an error when rows are duplicated (as intended), so left_join is used
      left_join(X245.HDDCDD_scen, by = c("region", "variable", "year")) %>%
      mutate(degree.days = round(degree.days, energy.DIGITS_HDDCDD))

    # We only want to keep degree days for region / consumer combinations that actually exist (i.e. KualaLumpur does not have resid rural)
    # To do this, make a table that contains region / consumer combinations where the share is nonzero
    keep_degree_days <- X245.SubregionalShares_bld_Subregions_Malaysia %>%
      filter( subregional.population.share > 0 ) %>%
      select( c( region, gcam.consumer ) ) %>%
      distinct()

    X245.HDDCDD_constdds_SEA <- X245.HDDCDD_temp %>%
      filter(SRES == "constdd") %>%
      select(-SRES, -GCM, -variable) %>%
      # rename to target region(s)
      write_to_all_states(c( "region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "year", "degree.days" ),
                          region_list = gcam.Malaysia.subregions ) %>%
      semi_join( keep_degree_days, by = c( "region", "gcam.consumer" ) )

    X245.HDDCDD_A2_GFDL_SEA <- X245.HDDCDD_temp %>%
      filter(SRES == "A2") %>%
      select(-SRES, -GCM, -variable) %>%
      # rename to target region(s)
      write_to_all_states(c( "region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "year", "degree.days" ),
                          region_list = gcam.Malaysia.subregions ) %>%
      semi_join( keep_degree_days, by = c( "region", "gcam.consumer" ) )

    # X245.HDDCDD_AEO_2015_SEA <- X245.HDDCDD_temp %>%
    #   filter(SRES == "AEO_2015") %>%
    #   select(-SRES, -GCM, -variable)

    # ===================================================
    # 3. Shell Conductance
    # ===================================================
    # X245.ShellConductance_bld_gcamusa: Shell conductance (inverse of shell efficiency)
    # TODO: Shell conductance from Zarrar's script?
    X245.ShellConductance_bld_Subregions_Malaysia <- IND_A44_shell %>%
      # Convert to long form
      gather_years() %>%
      # Interpolate to model years
      complete(gcam.consumer, year = c(year, MODEL_YEARS)) %>%
      group_by(gcam.consumer) %>%
      mutate(value = round(approx_fun(year, value), energy.DIGITS_EFFICIENCY)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      mutate(floor.to.surface.ratio = energy.FLOOR_TO_SURFACE_RATIO,
             shell.year = year) %>%
      # Rename columns
      rename(shell.conductance = value) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["ShellConductance"]],
                          region_list = gcam.Malaysia.subregions )

    # ===================================================
    # 4. Buildings Sectors, Subsectors, etc.
    # ===================================================

    # The remainder of the building-level parameters require information about the output of each service, which we do not have yet

    # X245.Supplysector_bld: Supplysector info for buildings
    X245.Supplysector_bld_Subregions_Malaysia <- write_to_all_states(IND_A44_sector, c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                                                         region_list = gcam.Malaysia.subregions)

    # X245.FinalEnergyKeyword_bld: Supply sector keywords for detailed building sector
    X245.FinalEnergyKeyword_bld_Subregions_Malaysia <- write_to_all_states(IND_A44_sector, LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],
                                                               region_list = gcam.Malaysia.subregions)

    # X245.SubsectorLogit_bld: Subsector logit exponents of building sector
    X245.SubsectorLogit_bld_Subregions_Malaysia <- write_to_all_states(IND_A44_subsector_logit, c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                                                           region_list = gcam.Malaysia.subregions)

    # X245.SubsectorShrwt_bld and X245.SubsectorShrwtFllt_bld: Subsector shareweights of building sector
    if(any(!is.na(IND_A44_subsector_shrwt$year))) {
      X245.SubsectorShrwt_bld_Subregions_Malaysia <- write_to_all_states(IND_A44_subsector_shrwt %>%
                                                               filter(!is.na(year)), LEVEL2_DATA_NAMES[["SubsectorShrwt"]],
                                                             region_list = gcam.Malaysia.subregions)
    }
    if(any(!is.na(IND_A44_subsector_shrwt$year.fillout))) {
      X245.SubsectorShrwtFllt_bld_Subregions_Malaysia <- write_to_all_states(IND_A44_subsector_shrwt %>%
                                                                   filter(!is.na(year.fillout)), LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]],
                                                                 region_list = gcam.Malaysia.subregions)
    }

    # X245.SubsectorInterp_bld and X245.SubsectorInterpTo_bld: Subsector shareweight interpolation of building sector
    if(any(is.na(IND_A44_subsector_interp$to.value))) {
      X245.SubsectorInterp_bld_Subregions_Malaysia <- write_to_all_states(IND_A44_subsector_interp %>%
                                                                filter(is.na(to.value)), LEVEL2_DATA_NAMES[["SubsectorInterp"]],
                                                              region_list = gcam.Malaysia.subregions)
    }
    if(any(!is.na(IND_A44_subsector_interp$to.value))) {
      X245.SubsectorInterpTo_bld_Subregions_Malaysia <- write_to_all_states(IND_A44_subsector_interp %>%
                                                                  filter(!is.na(to.value)), LEVEL2_DATA_NAMES[["SubsectorInterpTo"]],
                                                                region_list = gcam.Malaysia.subregions)
    }

    # X245.StubTech_bld_Subregions_Malaysia: Identification of stub technologies for buildings
    X245.StubTech_bld_Subregions_Malaysia <- IND_A44_tech_eff %>%
      select(supplysector, subsector, technology) %>%
      distinct() %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["Tech"]], region_list = gcam.Malaysia.subregions) %>%
      rename(stub.technology = technology)

    # X245.GlobalTechEff_bld: Assumed efficiencies (all years) of buildings technologies
    X245.end_use_eff <- IND_A44_tech_eff %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(efficiency = value)

    # TODO: Note from previous author: this code assumes that base-year efficiencies are identical (should fix to copy over to make sure)
    X245.GlobalTechEff_bld <- X245.end_use_eff %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]])

    # X245.StubTechMarket_bld: Specify market names for fuel inputs to all technologies in each region
    X245.StubTechMarket_bld_Subregions_Malaysia <- X245.end_use_eff %>%
      rename(stub.technology = technology) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input"), region_list = gcam.Malaysia.subregions) %>%
      mutate(market.name = gcam.Malaysia.parentregion)

    # X245.StubTechCalInput_bld: Calibrated energy consumption by buildings technologies
    # This needs to be done separately for the city and Rest of Region (RoR)
    # Since we are assuming there is no rural energy consumption in the cities.
    # The input that has energy consumption is at an aggregated level and needs to be broken out
    # the IESS file has fuel shares for "buildings" as a whole
    bld_agg_energy_consumption_city <- X244.StubTechCalInput_bld_Subregions_Malaysia %>%
      filter( !grepl("Rest of", region ) ) %>%
      # aggregate energy consumption by fuel and year for building sector by comm and resid
      separate( supplysector, c( "resid/comm", "specific" ) ) %>%
      group_by( `resid/comm`, subsector, year, region ) %>%
      mutate( value = sum( calibrated.value ) ) %>%
      rename( fuel = subsector ) %>%
      distinct( `resid/comm`, fuel, year, region, value )

    bld_agg_energy_consumption_RoR <- X244.StubTechCalInput_bld_Subregions_Malaysia %>%
      filter( grepl("Rest of", region ) ) %>%
      # aggregate energy consumption by fuel and year for building sector by comm and resid
      separate( supplysector, c( "resid/comm", "specific" ) ) %>%
      group_by( `resid/comm`, subsector, year, region ) %>%
      mutate( value = sum( calibrated.value ) ) %>%
      rename( fuel = subsector ) %>%
      distinct( `resid/comm`, fuel, year, region, value )

    # create a table that has service fuel shares for the model base years
    # TODO: get regional fuel consumption by service by year- using India for now
    # For the IESS, instead of having shares by fuel for buildings as a whole,
    # we want it by resid/comm and fuel
    # For the cities, we are assuming there is no residential rural area,
    # Since the "Rest of Region" has rural, we need to make two tables with different shares.
    IESS_bld_serv_fuel_Malaysia_resid_comm_city <- IESS_bld_serv_fuel_Malaysia %>%
      filter( sector != "residential rural" ) %>%
      separate( sector, c( "resid/comm", "drop" ), remove = F ) %>%
      select( -drop ) %>%
      group_by( `resid/comm`, fuel ) %>%
      mutate( "total" = sum( share ),
              "share" = share / total ) %>%
      ungroup() %>%
      select( -c( `resid/comm`, total ) )

    IESS_bld_serv_fuel_Malaysia_resid_comm_RoR <- IESS_bld_serv_fuel_Malaysia %>%
      separate( sector, c( "resid/comm", "drop" ), remove = F ) %>%
      select( -drop ) %>%
      group_by( `resid/comm`, fuel ) %>%
      mutate( "total" = sum( share ),
              "share" = share / total ) %>%
      ungroup() %>%
      select( -c( `resid/comm`, total ) )


    bld_service_fuel_energy_consumption_city <- IESS_bld_serv_fuel_Malaysia_resid_comm_city %>%
      repeat_add_columns( tibble( year = MODEL_BASE_YEARS ) ) %>%
      mutate( "resid/comm" = sector ) %>%
      separate( `resid/comm`, c( "resid/comm", "drop" ) ) %>%
      mutate( `resid/comm` = gsub( "residential", "resid", `resid/comm` ),
              `resid/comm` = gsub( "commercial", "comm", `resid/comm` ) ) %>%
      select( -drop ) %>%
      # join with the table that has energy consumption by resid/comm, fuel and year
      # TODO: fuel "solar" is not in bld_agg_energy_consumption, which is why solar water heaters are not included
      left_join( bld_agg_energy_consumption_city, by = c( "year", "fuel", "resid/comm" ) ) %>%
      # omit NAs (solar water heater)
      filter(!is.na(value)) %>%
      # multiply the energy consumption value by share to get energy consumption for detailed services
      mutate( value = share * value,
      # change sector names to match format
              sector = gsub( "commercial", "comm", sector ),
              sector = gsub( "residential urban", "resid urban", sector ) ) %>%
      # combine sector and service columns to get supplysector
      unite( supplysector, sector, service, sep = " " ) %>%
      # change NAs to 0
      mutate( value = ifelse(is.na(value), 0, value))

    bld_service_fuel_energy_consumption_RoR <- IESS_bld_serv_fuel_Malaysia_resid_comm_RoR %>%
      repeat_add_columns( tibble( year = MODEL_BASE_YEARS ) ) %>%
      mutate( "resid/comm" = sector ) %>%
      separate( `resid/comm`, c( "resid/comm", "drop" ) ) %>%
      mutate( `resid/comm` = gsub( "residential", "resid", `resid/comm` ),
              `resid/comm` = gsub( "commercial", "comm", `resid/comm` ) ) %>%
      select( -drop ) %>%
      # join with the table that has energy consumption by resid/comm, fuel and year
      # TODO: fuel "solar" is not in bld_agg_energy_consumption, which is why solar water heaters are not included
      left_join( bld_agg_energy_consumption_RoR, by = c( "year", "fuel", "resid/comm" ) ) %>%
      # omit NAs (solar water heater)
      filter(!is.na(value)) %>%
      # multiply the energy consumption value by share to get energy consumption for detailed services
      mutate( value = share * value,
              # change sector names to match format
              sector = gsub( "commercial", "comm", sector ),
              sector = gsub( "residential rural", "resid rural", sector ),
              sector = gsub( "residential urban", "resid urban", sector ) ) %>%
      # combine sector and service columns to get supplysector
      unite( supplysector, sector, service, sep = " " ) %>%
      # change NAs to 0
      mutate( value = ifelse(is.na(value), 0, value))


    X245.in_EJ_R_bld_serv_F_Yh <- bld_service_fuel_energy_consumption_city %>%
      bind_rows( bld_service_fuel_energy_consumption_RoR ) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      # Add subsector and energy.input
      # IND_bld_techs has hi and lo efficiency, so left_join_error_no_match does not work
      left_join( IND_bld_techs, by = c( "supplysector", "fuel", "technology" ) ) %>%
      select(region, supplysector, subsector, minicam.energy.input, year, technology, calibrated.value) %>%
      distinct(region, supplysector, subsector, minicam.energy.input, year, technology, calibrated.value)

    # Shares allocated to partitioned technologies need to be computed first using efficiencies
    X245.globaltech_eff_prt <- IND_A44_tech_eff %>%
      semi_join(IND_A44_tech_eff_avg, by = c("supplysector", "subsector")) %>%
      filter(year == gcamusa.EFFICIENCY_PARTITION_YEAR) %>%
      select(supplysector, subsector, technology, efficiency = value)

    # Calculate technology shares using efficiency values
    X245.globaltech_shares <- IND_A44_tech_eff_avg %>%
      # Adding specific technology efficiency to stock average efficiency
      left_join_error_no_match(X245.globaltech_eff_prt, by = c("supplysector", "subsector", "technology2" = "technology")) %>%
      rename(efficiency_tech2 = efficiency) %>%
      left_join_error_no_match(X245.globaltech_eff_prt, by = c("supplysector", "subsector", "technology3" = "technology")) %>%
      rename(efficiency_tech3 = efficiency) %>%
      # Calculate technology shares using stock average efficiency and individual technology efficiencies
      # Equation can be derived by solving following system of equations:
      # stockavg = efficiency_tech2 * share_tech2 + efficiency_tech3 * share_tech3
      # share_tech2 + share_tech3 = 1
      mutate(share_tech2 = (stockavg - efficiency_tech3) / (efficiency_tech2 - efficiency_tech3),
             share_tech3 = 1 - share_tech2) %>%
      # Keep only same names as A44.globaltech_shares and bind with A44.globaltech_shares
      select( -c( technology1, `minicam-energy-input`, stockavg, efficiency_tech2, efficiency_tech3 ) ) %>%
      rename( technology1 = technology2,
              technology2 = technology3,
              share_tech1 = share_tech2,
              share_tech2 = share_tech3 ) %>%
      bind_rows(IND_A44_globaltech_shares) %>%
      # Clunky, but we want only one technology and share value, currently have technology1, technology2, share1, share2
      tidyr::gather(share_type, share, share_tech1, share_tech2)%>%
      tidyr::gather(tech_type, technology, technology1, technology2) %>%
      # Filter for same technology and share number, then remove tech_type and share_type columns
      filter(substr(tech_type, nchar(tech_type), nchar(tech_type)) == substr(share_type, nchar(share_type), nchar(share_type))) %>%
      select(-tech_type, -share_type) %>%
      # there are NAs due to some technologies not having high efficiency equivalents
      na.omit()

    # For calibration table, start with global tech efficiency table, and match in tech shares.
    X245.StubTechCalInput_bld_Subregions_Malaysia <- X245.GlobalTechEff_bld %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["GlobalTechEff"]], "region"),
                          region_list = gcam.Malaysia.subregions) %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      # Using left_join because we don't have shares for all technologies, NAs will be set to 1
      left_join(X245.globaltech_shares, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      replace_na(list(share = 1)) %>%
      # join back with technology table to get a column that has general technology names
      left_join_error_no_match( IND_bld_techs, by = c( "supplysector", "subsector", "minicam.energy.input", "stub.technology" = "technology_specific")) %>%
      # remove unnecessary columns from the join
      select( -c( "sector", "fuel", "service" ) ) %>%
      # Add energy by state/service/fuel
      # can't use left_join_error_no_match because solar is missing
      left_join(X245.in_EJ_R_bld_serv_F_Yh, by = c("region", "supplysector", "subsector", "technology", "minicam.energy.input", "year")) %>%
      # remove NAs (solar)
      filter( !is.na( calibrated.value ) ) %>%
      # calibrated.value = energy * share
      mutate(calibrated.value = round(share * calibrated.value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             calOutputValue = calibrated.value) %>%
      # Set subsector and technology shareweights
      set_subsector_shrwt() %>%
      mutate(tech.share.weight =  if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])

    # Some technologies may be missing from the calibrated inputs table due to being in IESS
    # but not in X244.StubTechCalInput_bld_Subregions_Malaysia. We want to know what these technologies are, and
    # assign them 0 inputs in the base years
    # First, reformat IESS data for joining purposes
    IESS_reformat <- IESS_bld_serv_fuel_Malaysia %>%
      mutate( sector = gsub( "residential urban", "resid urban", sector ),
              sector = gsub( "residential rural", "resid rural", sector ),
              sector = gsub( "commercial", "comm", sector )) %>%
      unite( supplysector, c( sector, service ), sep = " " )
    # Then, anti-join the two dataframes of interest to see what is missing
    missing_entries <- anti_join( IESS_reformat, X245.StubTechCalInput_bld_Subregions_Malaysia,
               by = c( "supplysector", "fuel" = "subsector" ) )

    # If there are missing entries, we need to add them to the calibrated input dataframe
    if ( dim(missing_entries)[1] != 0 ) {
      X245.StubTechCalInput_bld_Subregions_Malaysia <- missing_entries %>%
        select( -share ) %>%
        rename( subsector = fuel,
                stub.technology = technology ) %>%
        mutate( year = 0 ) %>%
        complete( nesting( supplysector, subsector, stub.technology ), year = MODEL_BASE_YEARS ) %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        write_to_all_states(c("supplysector", "subsector", "stub.technology", "year", "region"),
                            region_list = gcam.Malaysia.subregions) %>%
        mutate( calibrated.value = 0,
                share.weight.year = year,
                calOutputValue = calibrated.value ) %>%
        # join back with technology table to get a column that has general technology names
        left_join_error_no_match( IND_bld_techs, by = c( "supplysector", "subsector", "stub.technology" = "technology_specific")) %>%
        # remove unnecessary columns from the join
        select( -c( "sector", "fuel", "service" ) ) %>%
        # Set subsector and technology shareweights
        set_subsector_shrwt() %>%
        mutate( tech.share.weight =  if_else( calibrated.value > 0, 1, 0 ) ) %>%
        select( LEVEL2_DATA_NAMES[["StubTechCalInput"]] ) %>%
        bind_rows( X245.StubTechCalInput_bld_Subregions_Malaysia )
    }
    else{ print("No missing technologies") }

    # X245.GlobalTechShrwt_bld_Subregions_Malaysia: Default shareweights for global building technologies
    X245.GlobalTechShrwt_bld_Subregions_Malaysia <- IND_A44_tech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight)

    # X245.GlobalTechInterpTo_bld: Technology shareweight interpolation (selected techs only)
    X245.GlobalTechInterpTo_bld <- IND_A44_tech_interp %>%
      set_years() %>%
      mutate(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]])

    # X245.GlobalTechCost_bld: Non-fuel costs of global building technologies
    X245.GlobalTechCost_bld_Subregions_Malaysia <- IND_A44_tech_cost %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(input.cost = approx_fun(year, input.cost)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    # X245.GlobalTechSCurve_bld: Retirement rates for building technologies
    X245.GlobalTechSCurve_bld <- X245.GlobalTechCost_bld_Subregions_Malaysia %>%
      filter(year %in% c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS),
             sector.name %in% IND_A44_tech_retirement$supplysector) %>%
      # Add lifetimes and steepness
      left_join_error_no_match(IND_A44_tech_retirement, by = c("sector.name" = "supplysector")) %>%
      # Set steepness/halflife values to stock for base years, new for future years
      mutate(steepness = if_else(year == max(MODEL_BASE_YEARS), steepness_stock, steepness_new),
             half.life = if_else(year == max(MODEL_BASE_YEARS), half_life_stock, half_life_new)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechSCurve"]])

    # X245.GlobalTechIntGainOutputRatio: Output ratios of internal gain energy from non-thermal building services
    calibrated_techs_bld_SEA_consumer <- IND_bld_techs %>%
      select(gcam.consumer = sector, supplysector) %>%
      distinct()

    X245.GlobalTechIntGainOutputRatio <- IND_A44_tech_intgains %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))%>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(calibrated_techs_bld_SEA_consumer, by = "supplysector") %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      # Add internal.gains.market.name
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      # Add efficiency
      left_join_error_no_match(X245.GlobalTechEff_bld,
                               by = c("sector.name", "subsector.name", "technology", "year")) %>%
      mutate(internal.gains.output.ratio = round(input.ratio / efficiency, energy.DIGITS_EFFICIENCY)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], internal.gains.output.ratio, internal.gains.market.name)

    # X245.GenericBaseService and X245.ThermalBaseService: Base year output of buildings services (per unit floorspace)
    # Base-service: Multiply energy consumption by efficiency for each technology, and aggregate by service
    X245.base_service <- X245.StubTechCalInput_bld_Subregions_Malaysia %>%
      # Add in efficiency by technology
      left_join_error_no_match(X245.GlobalTechEff_bld,
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year", "minicam.energy.input")) %>%
      # Calculate base.service = calibrated.value(energy) * efficiency
      mutate(base.service = round(calibrated.value * efficiency, energy.DIGITS_CALOUTPUT)) %>%
      # Aggregate base service by service (supplysector)
      group_by(region, supplysector, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(calibrated_techs_bld_SEA_consumer, by = "supplysector") %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer")

    # Separate thermal and generic services into separate tables with different ID strings
    X245.GenericBaseService_bld_Subregions_Malaysia <- X245.base_service %>%
      filter(supplysector %in% generic_services) %>%
      rename(building.service.input = supplysector) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]])

    X245.ThermalBaseService_bld_Subregions_Malaysia <- X245.base_service %>%
      filter(supplysector %in% thermal_services) %>%
      rename(thermal.building.service.input = supplysector) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]])

    # X245.GenericServiceSatiation_bld_Subregions_Malaysia: Satiation levels assumed for non-thermal building services
    # Just multiply the base-service by an exogenous multiplier
    X245.GenericServiceSatiation_bld_Subregions_Malaysia <- X245.GenericBaseService_bld_Subregions_Malaysia %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(X245.Floorspace_bld_Subregions_Malaysia, by = unique(c(LEVEL2_DATA_NAMES[["BldNodes"]], "year", "region"))) %>%
      # Add multiplier
      # TODO: Need multiplier for SEA, currently just changed to SEAsia sectors, but the values are not correct
      # TODO: ask Sha or Page about demand satiation multiplier
      left_join_error_no_match(IND_A44_demand_satiation_mult, by = c("building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]])

    # X245.ThermalServiceSatiation: Satiation levels assumed for thermal building services
    X245.ThermalServiceSatiation_bld_Subregions_Malaysia <- X245.ThermalBaseService_bld_Subregions_Malaysia %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(X245.Floorspace_bld_Subregions_Malaysia, by = unique(c(LEVEL2_DATA_NAMES[["BldNodes"]], "year", "region"))) %>%
      # Add multiplier
      left_join_error_no_match(IND_A44_demand_satiation_mult, by = c("thermal.building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])

    # X245.Intgains_scalar: Scalers relating internal gain energy to increased/reduced cooling/heating demands
    variable <- c("HDD", "CDD")
    scalar <- c(energy.INTERNAL_GAINS_SCALAR_USA_H, energy.INTERNAL_GAINS_SCALAR_USA_C)
    DDnorm <- c(gcamSEA.BASE_HDD_SEA, gcamSEA.BASE_CDD_SEA)
    SEA.base.scalar <- tibble(variable, scalar, DDnorm)
    threshold_HDD <- 500

    X245.Intgains_scalar_bld_Subregions_Malaysia <- X245.ThermalServiceSatiation_bld_Subregions_Malaysia %>%
      # Assign HDD or CDD
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add scalar
      left_join_error_no_match(SEA.base.scalar, by = "variable") %>%
      # Add degree days
      left_join_error_no_match(X245.HDDCDD_normal, by = c("region", "variable")) %>%
      left_join_error_no_match(X245.HDDCDD_normal_USA, by = c("variable")) %>%
      group_by(thermal.building.service.input) %>%
      mutate( scalar_mult = degree.days / degree.days.USA ) %>%
      ungroup() %>%
      mutate(internal.gains.scalar = round(scalar * scalar_mult, energy.DIGITS_HDDCDD),
             # Prevent very warm places from having negative heating demands, using exogenous threshold
             internal.gains.scalar = if_else(variable == "HDD" & degree.days < threshold_HDD, 0, internal.gains.scalar)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]])

    # ===================================================
    # 6. Building Gompertz Function
    # ===================================================
    # GCAM 6.0 now uses a different function to model floorpsace in residential buildings.
    # We will apply the values from the parent region to the subregions.
    # This table has the region / gcam.consumer / nodeInput / building.node.input information we need
    resid_subregions <- X245.DemandFunction_serv_bld_Subregions_Malaysia %>%
      # filter for residential only
      filter(gcam.consumer != "comm") %>%
      # remove unneeded column
      select(-prodDmdFnType)

    # Now, modify the National level gompertz table
    gompertz_subregions <- L244.GompFnParam %>%
      # filter for parent region
      filter(region == gcam.Malaysia.parentregion) %>%
      # apply these values to the subregions
      write_to_all_states(LEVEL2_DATA_NAMES[["GompFnParam"]],
                          region_list = gcam.Malaysia.subregions) %>%
      # remove the columns we will replace with entries from the resid_subregions table above
      select(-c(gcam.consumer, nodeInput, building.node.input))

    # Join the two tables together
    X245.GompFnParam_Malaysia <- resid_subregions %>%
      left_join_error_no_match(gompertz_subregions, by = c("region"))

    # ===================================================
    # 6. Checking Subregional Shares
    # ===================================================
    # If the subregional share for any region is 0, it must be removed from all data tables
    # First, figure out if this case exists, and for what regions / years
    zero_subregional_shares <- X245.SubregionalShares_bld_Subregions_Malaysia %>%
      filter( subregional.population.share == 0 ) %>%
      distinct( region, gcam.consumer, inc.year.fillout )

    # If this case exists, we need to remove those entries from all data tables using an anti-join
    # and reassign each dataframe to the same name
    if( length( zero_subregional_shares != 0 ) ) {

      X245.SubregionalShares_bld_Subregions_Malaysia <- X245.SubregionalShares_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer", "inc.year.fillout" ) )

      X245.PriceExp_IntGains_bld_Subregions_Malaysia <- X245.PriceExp_IntGains_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer", "price.exp.year.fillout" = "inc.year.fillout" ) )

      X245.Floorspace_bld_Subregions_Malaysia <- X245.Floorspace_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer", "year" = "inc.year.fillout" ) )

      X245.DemandFunction_serv_bld_Subregions_Malaysia <- X245.DemandFunction_serv_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) )

      X245.DemandFunction_flsp_bld_Subregions_Malaysia <- X245.DemandFunction_flsp_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) )

      X245.Satiation_flsp_bld_Subregions_Malaysia <- X245.Satiation_flsp_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) )

      X245.SatiationAdder_bld_Subregions_Malaysia <- X245.SatiationAdder_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) )

      X245.ThermalBaseService_bld_Subregions_Malaysia <- X245.ThermalBaseService_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer", "year" = "inc.year.fillout" ) )

      X245.GenericBaseService_bld_Subregions_Malaysia <- X245.GenericBaseService_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer", "year" = "inc.year.fillout" ) )

      X245.ThermalServiceSatiation_bld_Subregions_Malaysia <- X245.ThermalServiceSatiation_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) )

      X245.GenericServiceSatiation_bld_Subregions_Malaysia <- X245.GenericServiceSatiation_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) )

      X245.Intgains_scalar_bld_Subregions_Malaysia <- X245.Intgains_scalar_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) )

      X245.ShellConductance_bld_Subregions_Malaysia <- X245.ShellConductance_bld_Subregions_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer", "year" = "inc.year.fillout" ) )

      X245.Supplysector_bld_Subregions_Malaysia <- X245.Supplysector_bld_Subregions_Malaysia %>%
        separate( supplysector, c( "resid/comm", "specific" ), remove = F ) %>%
        unite( "gcam.consumer", c( "resid/comm", "specific" ), sep = " " ) %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer", "logit.year.fillout" = "inc.year.fillout" ) ) %>%
        select( -gcam.consumer )

      X245.FinalEnergyKeyword_bld_Subregions_Malaysia <- X245.FinalEnergyKeyword_bld_Subregions_Malaysia %>%
        separate( supplysector, c( "resid/comm", "specific" ), remove = F ) %>%
        unite( "gcam.consumer", c( "resid/comm", "specific" ), sep = " " ) %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) ) %>%
        select( -gcam.consumer )

      X245.SubsectorShrwtFllt_bld_Subregions_Malaysia <- X245.SubsectorShrwtFllt_bld_Subregions_Malaysia %>%
        separate( supplysector, c( "resid/comm", "specific" ), remove = F ) %>%
        unite( "gcam.consumer", c( "resid/comm", "specific" ), sep = " " ) %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) ) %>%
        select( -gcam.consumer )

      X245.SubsectorInterp_bld_Subregions_Malaysia <- X245.SubsectorInterp_bld_Subregions_Malaysia %>%
        separate( supplysector, c( "resid/comm", "specific" ), remove = F ) %>%
        unite( "gcam.consumer", c( "resid/comm", "specific" ), sep = " " ) %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) ) %>%
        select( -gcam.consumer )

      X245.SubsectorLogit_bld_Subregions_Malaysia <- X245.SubsectorLogit_bld_Subregions_Malaysia %>%
        separate( supplysector, c( "resid/comm", "specific" ), remove = F ) %>%
        unite( "gcam.consumer", c( "resid/comm", "specific" ), sep = " " ) %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) ) %>%
        select( -gcam.consumer )

      X245.StubTech_bld_Subregions_Malaysia <- X245.StubTech_bld_Subregions_Malaysia %>%
        separate( supplysector, c( "resid/comm", "specific" ), remove = F ) %>%
        unite( "gcam.consumer", c( "resid/comm", "specific" ), sep = " " ) %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) ) %>%
        select( -gcam.consumer )

      X245.StubTechCalInput_bld_Subregions_Malaysia <- X245.StubTechCalInput_bld_Subregions_Malaysia %>%
        separate( supplysector, c( "resid/comm", "specific" ), remove = F ) %>%
        unite( "gcam.consumer", c( "resid/comm", "specific" ), sep = " " ) %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer", "year" = "inc.year.fillout" ) ) %>%
        select( -gcam.consumer )

      X245.StubTechMarket_bld_Subregions_Malaysia <- X245.StubTechMarket_bld_Subregions_Malaysia %>%
        separate( supplysector, c( "resid/comm", "specific" ), remove = F ) %>%
        unite( "gcam.consumer", c( "resid/comm", "specific" ), sep = " " ) %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer", "year" = "inc.year.fillout" ) ) %>%
        select( -gcam.consumer )

      X245.GompFnParam_Malaysia <- X245.GompFnParam_Malaysia %>%
        anti_join( zero_subregional_shares, by = c( "region", "gcam.consumer" ) )

      }


    # ===================================================
    # Produce outputs
    X245.DeleteConsumer_bld_Subregions_Malaysia %>%
      add_title("Deletes building sector in SEA region to rewrite with gcam-seasia data") %>%
      add_units("NA") %>%
      add_comments("gcam.consumer column from A44.gcam_consumer") %>%
      add_legacy_name("X245.DeleteConsumer_SEAbld") %>%
      add_precursors("energy/A44.gcam_consumer") ->
      X245.DeleteConsumer_bld_Subregions_Malaysia

    X245.DeleteSupplysector_bld_Subregions_Malaysia %>%
      add_title("Deletes building sector in SEA region to rewrite with gcam-seasia data") %>%
      add_units("NA") %>%
      add_comments("supplysector column from A44.sector") %>%
      add_legacy_name("X245.DeleteSupplysector_SEAbld") %>%
      add_precursors("energy/A44.sector") ->
      X245.DeleteSupplysector_bld_Subregions_Malaysia

    X245.SubregionalShares_bld_Subregions_Malaysia %>%
      add_title("Shares out population and GDP to residential rural and urban") %>%
      add_units("NA") %>%
      add_precursors("gcam-seasia/IND_A44_subregional_shares", "gcam-seasia/A44.gcam_consumer") ->
      X245.SubregionalShares_bld_Subregions_Malaysia

    X245.PriceExp_IntGains_bld_Subregions_Malaysia %>%
      add_title("Price exponent on floorspace and naming of internal gains trial markets") %>%
      add_units("Unitless") %>%
      add_comments("A44.gcam_consumer written to all states") %>%
      add_legacy_name("X245.PriceExp_IntGains") %>%
      add_precursors("gcam-seasia/A44.gcam_consumer") ->
      X245.PriceExp_IntGains_bld_Subregions_Malaysia

    X245.Floorspace_bld_Subregions_Malaysia %>%
      add_title("base year floorspace") %>%
      add_units("billion m2") %>%
      add_comments("Data from L144.flsp_bm2_R_res_Yh and L144.flsp_bm2_R_comm_Yh") %>%
      add_legacy_name("X245.Floorspace") %>%
      add_precursors("X244.Floorspace_Subregions_Malaysia", "gcam-seasia/A44.gcam_consumer",
                     "gcam-seasia/Various_flsp_Mm2") ->
      X245.Floorspace_bld_Subregions_Malaysia

    X245.DemandFunction_serv_bld_Subregions_Malaysia %>%
      add_title("Service demand function types") %>%
      add_units("NA") %>%
      add_comments("IND_A44_demandFn_serv written to all states") %>%
      add_legacy_name("X245.DemandFunction_serv") %>%
      add_precursors("gcam-seasia/IND_A44_demandFn_serv") ->
      X245.DemandFunction_serv_bld_Subregions_Malaysia

    X245.DemandFunction_flsp_bld_Subregions_Malaysia %>%
      add_title("Floorspace demand function types") %>%
      add_units("NA") %>%
      add_comments("IND_A44_demandFn_flsp written to all states") %>%
      add_legacy_name("X245.DemandFunction_flsp") %>%
      add_precursors("gcam-seasia/IND_A44_demandFn_flsp") ->
      X245.DemandFunction_flsp_bld_Subregions_Malaysia

    X245.Satiation_flsp_bld_Subregions_Malaysia %>%
      add_title("Satiation levels assumed for floorspace") %>%
      add_units("million m2 / person") %>%
      add_comments("Values from X244.Satiation_flsp_Subregions_Malaysia or X245.Floorspace_bld_Subregions_Malaysia/X245.Pop_thous") %>%
      add_comments("Whichever is larger") %>%
      add_legacy_name("X245.Satiation_flsp") %>%
      add_precursors("X244.Satiation_flsp_Subregions_Malaysia", "gcam-seasia/A44.gcam_consumer", "X201.Pop_Subregions_Malaysia",
                     "X244.Floorspace_Subregions_Malaysia", "gcam-seasia/Various_flsp_Mm2") ->
      X245.Satiation_flsp_bld_Subregions_Malaysia

    X245.SatiationAdder_bld_Subregions_Malaysia %>%
      add_title("Satiation adders in floorspace demand function") %>%
      add_units("million m2 / person") %>%
      add_comments("Calculated with function dependent on satiation level; per capita floorspace; and per capita GDP") %>%
      add_legacy_name("X245.SatiationAdder") %>%
      add_precursors("X244.Satiation_flsp_Subregions_Malaysia", "gcam-seasia/A44.gcam_consumer", "X201.Pop_Subregions_Malaysia",
                     "X244.Floorspace_Subregions_Malaysia", "X201.GDP_Subregions_Malaysia") ->
      X245.SatiationAdder_bld_Subregions_Malaysia

    X245.ThermalBaseService_bld_Subregions_Malaysia %>%
      add_title("Base year output of thermal buildings services") %>%
      add_units("EJ per unit floorspace") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("X245.ThermalBaseService") %>%
      add_precursors("X244.StubTechCalInput_bld_Subregions_Malaysia", "gcam-seasia/IND_bld_techs",
                     "gcam-seasia/IND_A44_tech_eff", "gcam-seasia/IND_A44_tech_eff_avg", "gcam-seasia/IND_A44_globaltech_shares",
                     "gcam-seasia/A44.gcam_consumer", "gcam-seasia/IESS_bld_serv_fuel_Malaysia") ->
      X245.ThermalBaseService_bld_Subregions_Malaysia

    X245.GenericBaseService_bld_Subregions_Malaysia %>%
      add_title("Base year output of generic buildings services") %>%
      add_units("EJ per unit floorspace") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("X245.GenericBaseService") %>%
      add_precursors("X244.StubTechCalInput_bld_Subregions_Malaysia", "gcam-seasia/IND_bld_techs",
                     "gcam-seasia/IND_A44_tech_eff", "gcam-seasia/IND_A44_tech_eff_avg", "gcam-seasia/IND_A44_globaltech_shares",
                     "gcam-seasia/A44.gcam_consumer", "gcam-seasia/IESS_bld_serv_fuel_Malaysia") ->
      X245.GenericBaseService_bld_Subregions_Malaysia

    X245.GenericServiceSatiation_bld_Subregions_Malaysia %>%
      add_title("Satiation levels assumed for non-thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("X245.GenericServiceSatiation") %>%
      add_precursors("X244.StubTechCalInput_bld_Subregions_Malaysia", "gcam-seasia/IND_bld_techs",
                     "gcam-seasia/IND_A44_tech_eff", "gcam-seasia/IND_A44_tech_eff_avg", "gcam-seasia/IND_A44_globaltech_shares",
                     "gcam-seasia/A44.gcam_consumer", "X244.Floorspace_Subregions_Malaysia",
                     "gcam-seasia/IND_A44_demand_satiation_mult", "gcam-seasia/IESS_bld_serv_fuel_Malaysia") ->
      X245.GenericServiceSatiation_bld_Subregions_Malaysia

    X245.ThermalServiceSatiation_bld_Subregions_Malaysia %>%
      add_title("Satiation levels assumed for thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("X245.ThermalServiceSatiation") %>%
      add_precursors("X244.StubTechCalInput_bld_Subregions_Malaysia", "gcam-seasia/IND_bld_techs",
                     "gcam-seasia/IND_A44_tech_eff", "gcam-seasia/IND_A44_tech_eff_avg", "gcam-seasia/IND_A44_globaltech_shares",
                     "gcam-seasia/A44.gcam_consumer", "X244.Floorspace_Subregions_Malaysia",
                     "gcam-seasia/IND_A44_demand_satiation_mult", "gcam-seasia/IESS_bld_serv_fuel_Malaysia") ->
      X245.ThermalServiceSatiation_bld_Subregions_Malaysia

    X245.Intgains_scalar_bld_Subregions_Malaysia %>%
      add_title("Scalers relating internal gain energy to increased/reduced cooling/heating demands") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.scalar = exogenous scalar * degree.days / exogenous degree day norm") %>%
      add_legacy_name("X245.Intgains_scalar") %>%
      add_precursors("X244.StubTechCalInput_bld_Subregions_Malaysia", "gcam-seasia/IND_bld_techs",
                     "gcam-seasia/IND_A44_tech_eff", "gcam-seasia/IND_A44_tech_eff_avg", "gcam-seasia/IND_A44_globaltech_shares",
                     "gcam-seasia/A44.gcam_consumer", "X244.Floorspace_Subregions_Malaysia",
                     "gcam-seasia/IND_A44_demand_satiation_mult", "L143.HDDCDD_scen_R_Y") ->
      X245.Intgains_scalar_bld_Subregions_Malaysia

    X245.ShellConductance_bld_Subregions_Malaysia %>%
      add_title("Shell conductance (inverse of shell efficiency) by state") %>%
      add_units("Unitless") %>%
      add_comments("values from IND_A44_shell") %>%
      add_legacy_name("X245.ShellConductance_bld") %>%
      add_precursors("gcam-seasia/IND_A44_shell", "gcam-seasia/A44.gcam_consumer") ->
      X245.ShellConductance_bld_Subregions_Malaysia

    X245.Supplysector_bld_Subregions_Malaysia %>%
      add_title("Supplysector info for buildings") %>%
      add_units("Unitless") %>%
      add_comments("IND_A44_sector written to all states") %>%
      add_legacy_name("X245.Supplysector_bld") %>%
      add_precursors("gcam-seasia/IND_A44_sector") ->
      X245.Supplysector_bld_Subregions_Malaysia

    X245.FinalEnergyKeyword_bld_Subregions_Malaysia %>%
      add_title("Supply sector keywords for detailed building sector") %>%
      add_units("NA") %>%
      add_comments("IND_A44_sector written to all states") %>%
      add_legacy_name("X245.FinalEnergyKeyword_bld") %>%
      add_precursors("gcam-seasia/IND_A44_sector") ->
      X245.FinalEnergyKeyword_bld_Subregions_Malaysia

    if(exists("X245.SubsectorShrwt_bld")) {
      X245.SubsectorShrwt_bld_Subregions_Malaysia %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("IND_A44_subsector_shrwt written to all regions") %>%
        add_legacy_name("X245.SubsectorShrwt_bld") %>%
        add_precursors("gcam-seasia/IND_A44_subsector_shrwt") ->
        X245.SubsectorShrwt_bld_Subregions_Malaysia
    } else {
      missing_data() %>%
        add_legacy_name("X245.SubsectorShrwt_bld") ->
        X245.SubsectorShrwt_bld_Subregions_Malaysia
    }

    if(exists("X245.SubsectorShrwtFllt_bld_Subregions_Malaysia")) {
      X245.SubsectorShrwtFllt_bld_Subregions_Malaysia %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("IND_A44_subsector_shrwt written to all regions") %>%
        add_legacy_name("X245.SubsectorShrwtFllt_bld") %>%
        add_precursors("gcam-seasia/IND_A44_subsector_shrwt") ->
        X245.SubsectorShrwtFllt_bld_Subregions_Malaysia
    } else {
      missing_data() %>%
        add_legacy_name("X245.SubsectorShrwtFllt_bld") ->
        X245.SubsectorShrwtFllt_bld_Subregions_Malaysia
    }


    if(exists("X245.SubsectorInterp_bld_Subregions_Malaysia")) {
      X245.SubsectorInterp_bld_Subregions_Malaysia %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("IND_A44_subsector_interp written to all regions") %>%
        add_legacy_name("X245.SubsectorInterp_bld") %>%
        add_precursors("gcam-seasia/IND_A44_subsector_interp") ->
        X245.SubsectorInterp_bld_Subregions_Malaysia
    } else {
      missing_data() %>%
        add_legacy_name("X245.SubsectorInterp_bld") ->
        X245.SubsectorInterp_bld_Subregions_Malaysia
    }

    if(exists("X245.SubsectorInterpTo_bld_Subregions_Malaysia")) {
      X245.SubsectorInterpTo_bld_Subregions_Malaysia %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("IND_A44_subsector_interp written to all regions") %>%
        add_legacy_name("X245.SubsectorInterpTo_bld") %>%
        add_precursors("gcam-seasia/IND_A44_subsector_interp") ->
        X245.SubsectorInterpTo_bld_Subregions_Malaysia
    } else {
      missing_data() %>%
        add_legacy_name("X245.SubsectorInterpTo_bld") ->
        X245.SubsectorInterpTo_bld_Subregions_Malaysia
    }

    X245.SubsectorLogit_bld_Subregions_Malaysia %>%
      add_title("Subsector logit exponents of building sector") %>%
      add_units("Unitless") %>%
      add_comments("IND_A44_subsector_logit written to all states") %>%
      add_legacy_name("X245.SubsectorLogit_bld") %>%
      add_precursors("gcam-seasia/IND_A44_subsector_logit") ->
      X245.SubsectorLogit_bld_Subregions_Malaysia

    X245.StubTech_bld_Subregions_Malaysia %>%
      add_title("Identification of stub technologies for buildings") %>%
      add_units("NA") %>%
      add_comments("IND_A44_tech_eff written to all states") %>%
      add_legacy_name("X245.StubTech_bld") %>%
      add_precursors("gcam-seasia/IND_A44_tech_eff") ->
      X245.StubTech_bld_Subregions_Malaysia

    X245.StubTechCalInput_bld_Subregions_Malaysia %>%
      add_title("Calibrated energy consumption and share weights by buildings technologies") %>%
      add_units("calibrated.value: EJ/yr; shareweights: Unitless") %>%
      add_comments("Energy consumption multiplied by shares to get calibrated energy") %>%
      add_comments("Shares calculated using efficiency averages") %>%
      add_legacy_name("X245.StubTechCalInput_bld") %>%
      add_precursors("X244.StubTechCalInput_bld_Subregions_Malaysia", "gcam-seasia/IND_bld_techs", "gcam-seasia/IND_A44_tech_eff",
                     "gcam-seasia/IND_A44_tech_eff_avg", "gcam-seasia/IND_A44_globaltech_shares", "gcam-seasia/IESS_bld_serv_fuel_Malaysia") ->
      X245.StubTechCalInput_bld_Subregions_Malaysia

    X245.StubTechMarket_bld_Subregions_Malaysia %>%
      add_title("market names for fuel inputs to all technologies in each state") %>%
      add_units("NA") %>%
      add_comments("Categories from IND_A44_tech_eff written to all states") %>%
      add_comments("Market set to states for electricity") %>%
      add_legacy_name("X245.StubTechMarket_bld") %>%
      add_precursors("gcam-seasia/IND_A44_tech_eff") ->
      X245.StubTechMarket_bld_Subregions_Malaysia

    X245.GlobalTechIntGainOutputRatio %>%
      add_title("Output ratios of internal gain energy from non-thermal building services") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.output.ratio = input.ratio from IND_A44_tech_intgains divided by efficiency from X245.GlobalTechEff_bld") %>%
      add_legacy_name("X245.GlobalTechIntGainOutputRatio") %>%
      add_precursors("gcam-seasia/IND_A44_tech_intgains", "gcam-seasia/IND_bld_techs",
                     "gcam-seasia/A44.gcam_consumer", "gcam-seasia/IND_A44_tech_eff") ->
      X245.GlobalTechIntGainOutputRatio_bld_Subregions_Malaysia

    X245.GlobalTechInterpTo_bld %>%
      add_title("Technology shareweight interpolation") %>%
      add_units("NA") %>%
      add_comments("Directly from IND_A44_tech_interp") %>%
      add_legacy_name("X245.GlobalTechInterpTo_bld") %>%
      add_precursors("gcam-seasia/IND_A44_tech_interp") ->
      X245.GlobalTechInterpTo_bld_Subregions_Malaysia

    X245.GlobalTechEff_bld %>%
      add_title("Assumed efficiencies (all years) of buildings technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values from IND_A44_tech_eff") %>%
      add_legacy_name("X245.GlobalTechEff_bld") %>%
      add_precursors("gcam-seasia/IND_A44_tech_eff") ->
      X245.GlobalTechEff_bld_Subregions_Malaysia

    X245.GlobalTechShrwt_bld_Subregions_Malaysia %>%
      add_title("Default shareweights for global building technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values interpolated to model years from IND_A44_tech_shrwt") %>%
      add_legacy_name("X245.GlobalTechShrwt_bld") %>%
      add_precursors("gcam-seasia/IND_A44_tech_shrwt") ->
      X245.GlobalTechShrwt_bld_Subregions_Malaysia

    X245.GlobalTechCost_bld_Subregions_Malaysia %>%
      add_title("Non-fuel costs of global building technologies") %>%
      add_units("1975$/GJ") %>%
      add_comments("Values from IND_A44_tech_cost") %>%
      add_legacy_name("X245.GlobalTechCost_bld") %>%
      add_precursors("gcam-seasia/IND_A44_tech_cost") ->
      X245.GlobalTechCost_bld_Subregions_Malaysia

    X245.GlobalTechSCurve_bld %>%
      add_title("Retirement rates for building technologies") %>%
      add_units("lifetime/half.life = years") %>%
      add_comments("Lifetime, steepness, and half.life from IND_A44_tech_retirement") %>%
      add_legacy_name("X245.GlobalTechSCurve_bld") %>%
      add_precursors("gcam-seasia/IND_A44_tech_cost", "gcam-seasia/IND_A44_tech_retirement") ->
      X245.GlobalTechSCurve_bld_Subregions_Malaysia

    # X245.HDDCDD_AEO_2015_SEA %>%
    #   add_title("Heating and Cooling Degree Days by State consistent with AEO 2015") %>%
    #   add_units("Fahrenheit Degree Days") %>%
    #   add_comments("L143.HDDCDD_scen_R_Y assigned to GCAM residential / commercial building consumers") %>%
    #   add_legacy_name("X245.HDDCDD_QER_QER") %>%
    #   same_precursors_as("X245.HDDCDD_A2_GFDL_SEA") ->
    #   X245.HDDCDD_AEO_2015_bld_Subregions_Malaysia

    X245.HDDCDD_A2_GFDL_SEA %>%
      add_title("Heating and Cooling Degree Days by State for GFDL A2") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("L143.HDDCDD_scen_R_Y assigned to GCAM subsectors") %>%
      add_legacy_name("X245.HDDCDD_A2_GFDL") %>%
      add_precursors("L143.HDDCDD_scen_R_Y", "gcam-seasia/IND_A44_sector",
                     "gcam-seasia/IND_bld_techs", "gcam-seasia/A44.gcam_consumer") ->
      X245.HDDCDD_A2_GFDL_bld_Subregions_Malaysia

    X245.HDDCDD_constdds_SEA %>%
      add_title("Heating and Cooling Degree Days by State - constant at historical levels") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("L143.HDDCDD_scen_R_Y assigned to GCAM subsectors") %>%
      same_precursors_as("X245.HDDCDD_A2_GFDL_SEA") ->
      X245.HDDCDD_constdds_bld_Subregions_Malaysia

    X245.GompFnParam_Malaysia %>%
      add_title("Parameters for the floorspace Gompertz function") %>%
      add_units("Unitless") %>%
      add_comments("Computed offline based on data from RECS and IEA") %>%
      add_precursors("L244.GompFnParam",
                     "gcam-seasia/IND_A44_demandFn_serv") ->
      X245.GompFnParam_Malaysia

    return_data(X245.DeleteConsumer_bld_Subregions_Malaysia,
                X245.DeleteSupplysector_bld_Subregions_Malaysia,
                X245.SubregionalShares_bld_Subregions_Malaysia,
                X245.PriceExp_IntGains_bld_Subregions_Malaysia,
                X245.Floorspace_bld_Subregions_Malaysia,
                X245.DemandFunction_serv_bld_Subregions_Malaysia,
                X245.DemandFunction_flsp_bld_Subregions_Malaysia,
                X245.Satiation_flsp_bld_Subregions_Malaysia,
                X245.SatiationAdder_bld_Subregions_Malaysia,
                X245.ThermalBaseService_bld_Subregions_Malaysia,
                X245.GenericBaseService_bld_Subregions_Malaysia,
                X245.ThermalServiceSatiation_bld_Subregions_Malaysia,
                X245.GenericServiceSatiation_bld_Subregions_Malaysia,
                X245.Intgains_scalar_bld_Subregions_Malaysia,
                X245.ShellConductance_bld_Subregions_Malaysia,
                X245.Supplysector_bld_Subregions_Malaysia,
                X245.FinalEnergyKeyword_bld_Subregions_Malaysia,
                X245.SubsectorShrwt_bld_Subregions_Malaysia,
                X245.SubsectorShrwtFllt_bld_Subregions_Malaysia,
                X245.SubsectorInterp_bld_Subregions_Malaysia,
                X245.SubsectorInterpTo_bld_Subregions_Malaysia,
                X245.SubsectorLogit_bld_Subregions_Malaysia,
                X245.StubTech_bld_Subregions_Malaysia,
                X245.StubTechCalInput_bld_Subregions_Malaysia,
                X245.StubTechMarket_bld_Subregions_Malaysia,
                X245.GlobalTechIntGainOutputRatio_bld_Subregions_Malaysia,
                X245.GlobalTechInterpTo_bld_Subregions_Malaysia,
                X245.GlobalTechEff_bld_Subregions_Malaysia,
                X245.GlobalTechShrwt_bld_Subregions_Malaysia,
                X245.GlobalTechCost_bld_Subregions_Malaysia,
                X245.GlobalTechSCurve_bld_Subregions_Malaysia,
                X245.HDDCDD_A2_GFDL_bld_Subregions_Malaysia,
                X245.HDDCDD_constdds_bld_Subregions_Malaysia,
                X245.GompFnParam_Malaysia)
  } else {
    stop("Unknown command")
  }
}
