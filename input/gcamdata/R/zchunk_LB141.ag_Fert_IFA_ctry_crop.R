# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB141.ag_Fert_IFA_ctry_crop
#'
#' Reconcile disparate IFA fertilizer consumption data to calculate fertilizer consumption (demand) for each GTAP country/crop.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L141.ag_Fert_Cons_MtN_ctry_crop}. The corresponding file in the
#' original data system was \code{LB141.ag_Fert_IFA_ctry_crop.R} (aglu level1).
#' @details Multiple harvested area data sources (LDS, FAO) are reconciled and used with bottom-up fertilizer consumption data from
#' IFA2002 to calculate fertilizer demand for each country and crop.
#' Top down estimates are calculated using IFA fertilizer data, and the top down estimates are used to fill in missing data from
#' the bottom up estimates and scale the bottom-up estimates, making the final output.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr gather drop_na replace_na
#' @author ACS May 2017
module_aglu_LB141.ag_Fert_IFA_ctry_crop <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             "L100.LDS_ag_HA_ha",
             "L100.FAO_ag_HA_ha",
             FILE = "aglu/IFA2002_Fert_ktN",
             FILE = "aglu/IFA_Fert_ktN"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L141.ag_Fert_Cons_MtN_ctry_crop"))
  } else if(command == driver.MAKE) {

    AREA_thousHa <- COUNTRY <- CROP <- Country <- FAO <- Fert_Cons_MtN <- Fert_MtN <- Fert_ktN <-
      GCAM_commodity <- GCAM_region_ID <- GTAP_crop <- HA_ha <- IFA2002_N_tha <-
      IFA2002_country <- IFA2002_crop <- IFA_N_Mt <- IFA_N_Mt_unscaled <- item <-
      IFA_N_tha_default <- IFA_commodity <- IFA_region <- LDS <- N_kt <- N_tha <- iso <- scaler <-
      value <- year <- NULL   # silence package checks

    all_data <- list(...)[[1]]

    # Load required inputs
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L100.FAO_ag_HA_ha <- get_data(all_data, "L100.FAO_ag_HA_ha")
    IFA2002_Fert_ktN <- get_data(all_data, "aglu/IFA2002_Fert_ktN")
    IFA_Fert_ktN <- get_data(all_data, "aglu/IFA_Fert_ktN")


    # 11/17/2022 GPK - because of comparative low yields for its surrounding commodity classes, Nutmeg production in
    # India is getting assigned very high fertilizer input-output coefficients, almost 0.5 kgN per harvested kg of nutmeg.
    # Bottom-up data on nutmeg production (e.g. https://indiaagronet.com/indiaagronet/crop%20info/nutmeg.htm) suggests
    # that the IO-coef should be more like 0.1, and for plantations whose avg yield is about 100 kg/ha, the N application
    # rate is about 10 kgN/ha
    IFA2002_Fert_ktN_supplement <- tibble(
      COUNTRY = "INDIA",
      YEAR = 2000,
      CROP = "Nutmeg",
      AREA_thousHa = 100,
      N_kt = 1,
      P2O5_kt = 2,
      K2O_kt = 0)
    IFA2002_Fert_ktN <- bind_rows(IFA2002_Fert_ktN, IFA2002_Fert_ktN_supplement)
    FAO_ag_items_PRODSTAT$IFA2002_crop[FAO_ag_items_PRODSTAT$GTAP_crop == "Ntmg_Mc_Crdm" & !is.na(FAO_ag_items_PRODSTAT$GTAP_crop)] <- "Nutmeg"

    # Perform Calculations

    # 02/2018 revision (GPK): several of the "Other" commodities in the IFA 2009 inventory have very high fertilizer
    # application quantities for the given land areas, leading to crops whose fertilizer costs alone approach or exceed
    # the commodity price. These are reduced by a factor of 5; this value is selected to get the implied fertilizer
    # application rates (kg N / m2) of IFA's "Other" crops in Pakistan and Morocco within the range of the "Other" crops'
    # average application rates observed in all other regions.
    IFA_Fert_ktN$Other[IFA_Fert_ktN$Country %in% c("Morocco", "Pakistan")] <-
      IFA_Fert_ktN$Other[IFA_Fert_ktN$Country %in% c("Morocco", "Pakistan")] / 5

    # Convert IFA (Heffer 2009) fertilizer consumption to long form and convert units to Mt of N consumed.
    # This is a top-down fertilizer consumption inventory by 24 large countries + regions and commodity
    # classes for a 2006-2007 base year, to be supplemented with bottom-up application rates estimated by
    # IFA2002, multiplied by LDS/Monfreda harvested area to get consumption quantities.

    IFA_Fert_ktN %>%
      filter(Country != "World Total") %>%
      gather(IFA_commodity, value, -Country) %>%
      mutate(Fert_MtN = value * CONV_KT_MT) %>%
      rename(IFA_region = Country, Fert_ktN = value) ->
      L141.IFA_Fert_ktN


    # Bring together LDS/Monfreda and FAO harvested area by country and crop, in order to calculate an FAO_LDS
    # scaler for each country-crop combination.
    # This scaler is used to scale the LDS harvested area data given at the
    # isoCountry-GLU-GTAPcrop level. This scaled harvested area is then multiplied by Fert application rates
    # and summed to get a bottom-up estimate of fertilizer demands at the iso-GTAPcrop level.
    #
    # The reason for computing and applying this scaler is that Monfreda/LDS harvested area are used to downscale
    # fertilizer consumption quantities from the inventories, but GCAM's area and production are (downscaled) from FAO's
    # land area and production quantities. Since the Monfreda/LDS data are constructed to be consistent with FAO national
    # data, this shouldn't be an issue, however for some crops, particularly fodder crops, there are significant
    # discrepancies (up to 50x), which if unaddressed, would return high fertilizer IO coefs and therefore negative profit rates.

    # GPK note August 2018 - this scaler used to be computed at the level of GCAM region and commodity. The method
    # is revised to instead apply by country and specific crop. This addresses (further) data discrepancies on
    # fodder crop production and harvested area that would otherwise lever off of one another to make unrealistic
    # fertilizer IO coefficients in selected region/crop/GLUs.

    # First, calculate the FAO_LDS scaler for each nation-crop combination
    # Take the FAO Harvested area data in table L100.FAO_ag_HA_ha and get the average value for each
    # region-commodity combination over the Monfreda/LDS years = 1998-2002 default.
    # Then Take the LDS harvested area data, L100.LDS_ag_HA_ha, sum to the nation-crop level,
    # join it to the averaged FAO data and compute an FAO_LDS scaler as FAO/LDS:
    #
    # Take FAO HA data and calulate the average value over aglu.FAO_LDS_YEARS for each nation-crop combo:
    # Note - using FAO's "item" here rather than LDS's "GTAP_crop" because the latter has two crops that correspond
    # to the FAO's "Grasses Nes for forage;Sil"
    L100.FAO_ag_HA_ha %>%
      filter(year %in% aglu.FAO_LDS_YEARS) %>%
      group_by(iso, item) %>%
      summarise(FAO = mean(value)) %>%
      ungroup() ->
      L141.FAO
    # Take LDS HA data and aggregate to the GCAM region-commodity level:
    L100.LDS_ag_HA_ha %>%
      left_join(FAO_ag_items_PRODSTAT[c("item", "GTAP_crop")], by = "GTAP_crop") %>%
      drop_na() %>%                 # Some of the GTAP crops don't have a corresponding crop in the FAO databases
      group_by(iso, item) %>%
      summarise(LDS = sum(value)) %>%
      ungroup() %>%
      # Join in the FAO data from the previous pipeline
      left_join(L141.FAO, by = c("iso", "item")) %>%
      replace_na(list(FAO = 0)) %>%
      # Calculate the FAO_LDS scaler value = FAO/LDS
      # Set an upper bound to prevent extremely high fertilizer allocations to potentially low production volume GLUs
      mutate(scaler = pmin(aglu.MAX_FAO_LDS_SCALER, FAO / LDS)) %>%
      # join back in the GTAP crop, which will be used for the remainder of the processing
      # Because the FAO item "Grasses nes for forage;Sil" is assigned to two GTAP crops ("GrsNESFrgSlg" and "MxGrss_Lgm"),
      # the "item" column will get longer. This is OK.
      left_join(FAO_ag_items_PRODSTAT[c("item", "GTAP_crop")], by = "item", ignore_columns = "item") %>%
      select(iso, GTAP_crop, scaler) ->
      # store in an FAO_LDS table:
      L141.FAO_LDS

    # Second, use the nation-crop scalers to scale LDS harvested area data at the iso-GLU-GTAPcrop level.
    #
    # Take LDS harvested area data at the iso-GLU-GTAPcrop level, L100.LDS_ag_HA_ha, join in the GCAM_region_ID info for
    # each iso, join in the GCAM_commodity info for each GTAPcrop, join the FAO_LDS scaler calculated in the previous
    # pipeline, and use the FAO_LDS scaler to scale the iso-GLU-GTAPcrop harvested area data
    L100.LDS_ag_HA_ha %>%
      left_join(L141.FAO_LDS, by = c("iso", "GTAP_crop")) %>%
      drop_na() %>%     # get rid of the green broad beans and kapok fibres that are in LDS but not FAO
      # use the nation-crop scaler to scale down each iso-GLU-GTAP harvested area
      mutate(value = value * scaler) %>%
      select(-scaler) ->
      # store in an LDS harvested area table
      L141.LDS_ag_HA_ha

    # Step 1: Supplement IFA (Heffer 2009) global fertilizer consumption by commodity and region to more detailed inventory
    # with partial coverage. The purpose of this step is to improve the specificity of both crops and regions, where possible.
    # This ultimately leads to the bottom-up estimate of fertilizer consumption at the iso-GTAPcrop level, using scaled
    # LDS data from L141.LDS_ag_HA_ha, IFA2002 fertlizer consumption data, and the top down consumption data in L141.IFA_Fert_ktN.
    #
    # Calculate a bottom up consumption RATE of N in units of tons/hectare for each IFA2002 country and crop.
    # There is a time component of IFA2002 that is handled by taking the max values over time for each country-crop.
    # specifically, the rate is calculated first and THEN the max is taken, rather than taking the rate as
    # max N_kt / max Area_ha (each of which could be from different years).
    IFA2002_Fert_ktN %>%
      # calculate nitrogen consumption in tons per hectare N_tha = N_kt / AREA_thousHa
      mutate(N_tha = N_kt / AREA_thousHa) %>%
      na.omit() %>%
      # get the max fertilizer consumption across years for each country and crop
      group_by(COUNTRY, CROP) %>%
      summarise(Area_thousHa = max(AREA_thousHa), N_kt = max(N_kt), N_tha = max(N_tha)) %>%
      ungroup() %>%
      rename(IFA2002_country= COUNTRY, IFA2002_crop = CROP) ->
      # store in a table of ferlizer application rates for each iso-GTAPcrop combo
      L141.IFA2002_Fert_ktN

    # Calculate fertilizer demand coefficients for 87 countries / 106 crops where available
    # Fertilizer demand = harvested area * fertilizer application rate for each country-crop.
    # Use FAO-scaled Monfreda/LDS harvested area by country-crop level, L141.LDS_ag_HA_ha,
    # sum to iso and GTAPcrop level, join IFA2002_crop and IFA_commodity (i.e., Heffer 2009) information.
    # Assign "unspecified cereals" in Ethiopia to teff,
    # join IFA2002_country information from AGLU_ctry (processed first),
    # join in the N application rate (N_tha = tN/ha) from L141.IFA2002_Fert_ktN,
    # join in IFA_commodity information from FAO_PRODSTAT table

    # process ALGU_ctry
    # we only need iso, IFA2002_country, and IFA_region to join to L141.LDS_ag_HA_ha when calculating bottom-up
    # consumption estimates below.
    # Joining as-is to L141.LDS_ag_HA_ha leads to several duplications due to multiple countries sharing iso codes.
    # This is incorrect, and inflates the number of rows.
    # The below pipeline fixes, now that IFA2002_country information has been added for Yugoslav SFR w/ iso = "hrv".
    # Some IFA_region information was added to other Yugoslav/iso's when present for corresponding iso's (to allow
    # the call to dplyr::distinct).
    # This avoids the use of left_join_keep_first_only in the subsequent pipeline.
    AGLU_ctry %>%
      # take only the isos we need
      filter(iso %in% L141.LDS_ag_HA_ha$iso) %>%
      select(iso, IFA2002_country, IFA_region) %>%
      distinct() ->
      # store in a table to join to L141.LDS_ag_HA_ha
      AGLU_ctry_iso_IFA_LDS

    # process L141.LDS_ag_HA_ha
    L141.LDS_ag_HA_ha %>%
      # Aggregate to the iso-GTAPcrop level
      group_by(iso, GTAP_crop) %>%
      summarise(HA_ha = sum(value)) %>%
      ungroup() %>%
      # join in IFA2002_crop and IFA_commodity info from the FAO PRODSTAT table, preserving NAs as in original code for later processing
      left_join(select(FAO_ag_items_PRODSTAT, GTAP_crop, IFA2002_crop, IFA_commodity), by = "GTAP_crop") %>%
      # In Ethiopia, replace unspecified cereals with teff
      mutate(IFA2002_crop = if_else(iso == "eth" & GTAP_crop == "cerealsnes", "Teff", IFA2002_crop)) %>%
      # add IFA2002_country and IFA_region information, keeping NA information for later processing
      left_join(AGLU_ctry_iso_IFA_LDS, by = "iso") %>%
      # join in the fertilizer application rate in N_tha (tN/ha) for each iso-GTAPcrop combo from L141.IFA2002_Fert_ktN,
      # keeping NA for later processing
      left_join(select(L141.IFA2002_Fert_ktN, IFA2002_crop, IFA2002_country, N_tha), by = c("IFA2002_crop", "IFA2002_country")) %>%
      rename(IFA2002_N_tha = N_tha) %>%
      # drop rows with NA for IFA_commodity
      drop_na(IFA_commodity) ->
      # store in ctry-crop specific table of fertilizer consumption rates in t/ha for use in multiple subsequent pipelines
      L141.IFA_Fert_Cons_MtN_ctry_crop

    # Take the above-processed country-crop specific Fertilizer information, aggregate to IFAregion-IFAcommodity
    # and use the top down estimates in table L141.IFA_Fert_ktN to calculate a default fertilizer consumption rate for
    # later use to fill in missing fertilizer demands from the bottom up estimate.
    L141.IFA_Fert_Cons_MtN_ctry_crop %>%
      # aggregate harvested area to IFA_region-IFA_commodity level
      group_by(IFA_region, IFA_commodity) %>%
      summarise(HA_ha = sum(HA_ha, na.rm = TRUE)) %>%
      ungroup() %>%
      # join in top-down fertilizer demand in MtN by IFA_region, IFA_commodity from L141.IFA_Fert_ktN
      left_join_error_no_match(select(L141.IFA_Fert_ktN, IFA_region, IFA_commodity, Fert_MtN),
                               by = c("IFA_region", "IFA_commodity")) %>%
      rename(IFA_N_Mt = Fert_MtN) %>%
      # calculate a default fertalizer demand in tons/hectare for each IFAregion-IFAcommodity
      # IFA_N_tha_default = IFA_N_Mt / HA_ha / conv_t_Mt
      mutate(IFA_N_tha_default = IFA_N_Mt / HA_ha / CONV_T_MT) ->
      # store in a table by IFAregion and IFAcommodity:
      L141.HA_ha_Rifa_Cifa

    # Use the bottom-up country crop table of harvested area and N application rate, L141.IFA_Fert_Cons_MtN_ctry_crop,
    # to calculate specific bottom up fertilizer demand = harvested area * application rate.
    # When such information is missing from the bottom up tables, use the default estimate from the top-down estimate
    # in table L141.HA_ha_Rifa_Cifa.
    L141.IFA_Fert_Cons_MtN_ctry_crop %>%
      # join in relevent IFA_region, IFA_commodity information
      left_join_error_no_match(select(L141.HA_ha_Rifa_Cifa, IFA_region, IFA_commodity, IFA_N_tha_default),
                               by = c("IFA_region", "IFA_commodity")) %>%
      # calculate bottom up fertilizer demand = harvested area * consumption rate, using defaults where missing
      mutate(IFA_N_Mt_unscaled = HA_ha * IFA2002_N_tha * CONV_T_MT,
             IFA_N_Mt_unscaled = replace(IFA_N_Mt_unscaled,
                                         is.na(IFA_N_Mt_unscaled),
                                         (HA_ha * IFA_N_tha_default)[is.na(IFA_N_Mt_unscaled)] * CONV_T_MT)) ->
      # store in table of ctry-crop level bottom up estimates of N consumption
      L141.IFA_Fert_Cons_MtN_ctry_crop


    # Step 2: Re-scale fertilizer consumption estimates so that totals match the IFA top-down inventory
    # Reconcile top down consumption of N (table L141.IFA_Fert_ktN, based on input IFA_Fert_ktN) with the bottom up
    # consumption of N (table L141.IFA_Fert_Cons_MtN_ctry_crop, based on input IFA2002_Fert_ktN, harvested area data
    # from multiple sources, and top-down estimates where necessary).
    #
    # Pipeline 1: Take unscaled, bottom up ctry-crop level fertilizer consumption rates in table L141.IFA_Fert_Cons_MtN_ctry_crop,
    # aggregate to the IFAregion-IFAcrop level,
    # join IFAregion-IFAcommodity Fertilizer top-down estimates from table L141.IFA_Fert_ktN,
    # use these two different estimates to calulate a saceler = top_down / bottom_up = IFA_N_Mt / IFA_N_Mt_unscaled,
    # replace any NA scalers with 1.
    #
    # Pipeline 2: Use this scaler to scale the bottom-up estimates of fertilizer consumption at the iso-GTAPcrop level in
    # table L141.IFA_Fert_Cons_MtN_ctry_crop

    # Pipeline 1:
    # take ctry crop bottom up estimate of N consumption
    L141.IFA_Fert_Cons_MtN_ctry_crop %>%
      select(IFA_region, IFA_commodity, IFA_N_Mt_unscaled) %>%
      # aggregate to IFA_region, IFA_commodity via summing
      group_by(IFA_region, IFA_commodity) %>%
      summarise(IFA_N_Mt_unscaled = sum(IFA_N_Mt_unscaled)) %>%
      ungroup() %>%
      # join top down IFA fertilizer estimates
      left_join_error_no_match(select(L141.IFA_Fert_ktN, -Fert_ktN), by = c("IFA_region", "IFA_commodity")) %>%
      # calculate scaler = top down / bottom up N consumption = Fert_MtN / IFA_N_Mt_unscaled
      mutate(scaler = Fert_MtN / IFA_N_Mt_unscaled) %>%
      replace_na(list(scaler = 1)) ->
      L141.IFA_Fert_Cons_Rifa_Cifa

    # Pipeline 2:
    # join this scaler to the bottom up country-crop Fertilizer consumption, L141.IFA_Fert_Cons_MtN_ctry_crop,
    # multiply by unscaled consumption to get final N consumption.
    # ouput only iso, GTAP_crop, and N consumption data.
    L141.IFA_Fert_Cons_MtN_ctry_crop %>%
      left_join_error_no_match(select(L141.IFA_Fert_Cons_Rifa_Cifa, IFA_region, IFA_commodity, scaler),
                               by = c("IFA_region", "IFA_commodity")) %>%
      mutate(Fert_Cons_MtN = IFA_N_Mt_unscaled * scaler) %>%
      select(iso, GTAP_crop, Fert_Cons_MtN) ->
      L141.ag_Fert_Cons_MtN_ctry_crop


    # Produce outputs
    L141.ag_Fert_Cons_MtN_ctry_crop %>%
      add_title("Fertilizer consumption by GTAP country / crop") %>%
      add_units("Megatons of Nitrogen (MtN)") %>%
      add_comments("Bottom-up estimates of fertilizer consumption are calulated using IFA2002 fertilizer data and ") %>%
      add_comments("multiple harvested area data sources. Top down estimates are calculated using IFA fertilizer data, ") %>%
      add_comments("and the top down estimates are used to scale the bottom-up estimates, making the final output.") %>%
      add_legacy_name("L141.ag_Fert_Cons_MtN_ctry_crop") %>%
      add_precursors("aglu/FAO/FAO_ag_items_PRODSTAT",
                     "common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "L100.LDS_ag_HA_ha",
                     "L100.FAO_ag_HA_ha",
                     "aglu/IFA2002_Fert_ktN",
                     "aglu/IFA_Fert_ktN") ->
      L141.ag_Fert_Cons_MtN_ctry_crop

    return_data(L141.ag_Fert_Cons_MtN_ctry_crop)
  } else {
    stop("Unknown command")
  }
}
