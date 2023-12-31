# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_land_input_5_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{land_input_5_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_5_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_5_IRR_MGMT_xml.R} (aglu XML).
module_aglu_batch_land_input_5_IRR_MGMT_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             "L2252.LN5_Logit",
             "L2252.LN5_HistMgdAllocation_crop",
             "L2252.LN5_MgdAllocation_crop",
             "L2252.LN5_HistMgdAllocation_bio",
             "L2252.LN5_MgdAllocation_bio",
             "L2252.LN5_MgdCarbon_crop",
             "L2252.LN5_MgdCarbon_bio",
             "L2252.LN5_LeafGhostShare",
             "L2252.LN5_NodeGhostShare"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "land_input_5_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2252.LN5_Logit <- get_data(all_data, "L2252.LN5_Logit")
    L2252.LN5_HistMgdAllocation_crop <- get_data(all_data, "L2252.LN5_HistMgdAllocation_crop")
    L2252.LN5_MgdAllocation_crop <- get_data(all_data, "L2252.LN5_MgdAllocation_crop")
    L2252.LN5_HistMgdAllocation_bio <- get_data(all_data, "L2252.LN5_HistMgdAllocation_bio")
    L2252.LN5_MgdAllocation_bio <- get_data(all_data, "L2252.LN5_MgdAllocation_bio")
    L2252.LN5_MgdCarbon_crop <- get_data(all_data, "L2252.LN5_MgdCarbon_crop")
    L2252.LN5_MgdCarbon_bio <- get_data(all_data, "L2252.LN5_MgdCarbon_bio")
    L2252.LN5_LeafGhostShare <- get_data(all_data, "L2252.LN5_LeafGhostShare")
    L2252.LN5_NodeGhostShare <- get_data(all_data, "L2252.LN5_NodeGhostShare")

    #........................
    # gcambreakout 17 July 2022
    # Temporary fix until circular dependency on
    # L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust is fixed.
    #........................

    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")

    # Valid GLU Region combos
    glu_gcam <- basin_to_country_mapping %>%
      dplyr::select(iso=ISO, GLU=GLU_code, GLU_name) %>%
      dplyr::mutate(iso = tolower(iso)) %>%
      dplyr::left_join(iso_GCAM_regID, by=c("iso")) %>%
      dplyr::select(GLU, GCAM_region_ID, GLU_name) %>%
      dplyr::filter(GCAM_region_ID > 32) %>%
      unique() %>%
      dplyr::left_join(GCAM_region_names); glu_gcam

    remove_glu <- glu_gcam$GLU_name %>% unique(); remove_glu
    regions_check <- GCAM_region_names$region[!GCAM_region_names$region %in% glu_gcam$region];  regions_check

    L2252.LN5_Logit %>%
      dplyr::mutate(remove = dplyr::if_else(((gsub(".*_","",LandNode1) %in% remove_glu) & (region %in% regions_check)),1,0)) %>%
      dplyr::filter(remove==0) %>%
      dplyr::select(-remove) ->
      L2252.LN5_Logit

    L2252.LN5_HistMgdAllocation_crop %>%
      dplyr::mutate(remove = dplyr::if_else(((gsub(".*_","",LandNode1) %in% remove_glu) & (region %in% regions_check)),1,0)) %>%
      dplyr::filter(remove==0) %>%
      dplyr::select(-remove) ->
      L2252.LN5_HistMgdAllocation_crop

    L2252.LN5_MgdAllocation_crop %>%
      dplyr::mutate(remove = dplyr::if_else(((gsub(".*_","",LandNode1) %in% remove_glu) & (region %in% regions_check)),1,0)) %>%
      dplyr::filter(remove==0) %>%
      dplyr::select(-remove) ->
      L2252.LN5_MgdAllocation_crop

    L2252.LN5_HistMgdAllocation_bio %>%
      dplyr::mutate(remove = dplyr::if_else(((gsub(".*_","",LandNode1) %in% remove_glu) & (region %in% regions_check)),1,0)) %>%
      dplyr::filter(remove==0) %>%
      dplyr::select(-remove) ->
      L2252.LN5_HistMgdAllocation_bio

    L2252.LN5_MgdAllocation_bio %>%
      dplyr::mutate(remove = dplyr::if_else(((gsub(".*_","",LandNode1) %in% remove_glu) & (region %in% regions_check)),1,0)) %>%
      dplyr::filter(remove==0) %>%
      dplyr::select(-remove) ->
      L2252.LN5_MgdAllocation_bio

    L2252.LN5_MgdCarbon_crop %>%
      dplyr::mutate(remove = dplyr::if_else(((gsub(".*_","",LandNode1) %in% remove_glu) & (region %in% regions_check)),1,0)) %>%
      dplyr::filter(remove==0) %>%
      dplyr::select(-remove) ->
      L2252.LN5_MgdCarbon_crop

    L2252.LN5_MgdCarbon_bio %>%
      dplyr::mutate(remove = dplyr::if_else(((gsub(".*_","",LandNode1) %in% remove_glu) & (region %in% regions_check)),1,0)) %>%
      dplyr::filter(remove==0) %>%
      dplyr::select(-remove) ->
      L2252.LN5_MgdCarbon_bio

    L2252.LN5_LeafGhostShare %>%
      dplyr::mutate(remove = dplyr::if_else(((gsub(".*_","",LandNode1) %in% remove_glu) & (region %in% regions_check)),1,0)) %>%
      dplyr::filter(remove==0) %>%
      dplyr::select(-remove) ->
      L2252.LN5_LeafGhostShare

    L2252.LN5_NodeGhostShare %>%
      dplyr::mutate(remove = dplyr::if_else(((gsub(".*_","",LandNode1) %in% remove_glu) & (region %in% regions_check)),1,0)) %>%
      dplyr::filter(remove==0) %>%
      dplyr::select(-remove) ->
      L2252.LN5_NodeGhostShare

    #............breakout close edits

    # ===================================================

    # Produce outputs
    create_xml("land_input_5_IRR_MGMT.xml") %>%
      add_logit_tables_xml(L2252.LN5_Logit, "LN5_Logit") %>%
      add_xml_data(L2252.LN5_HistMgdAllocation_crop, "LN5_HistMgdAllocation") %>%
      add_xml_data(L2252.LN5_MgdAllocation_crop, "LN5_MgdAllocation") %>%
      add_xml_data(L2252.LN5_HistMgdAllocation_bio, "LN5_HistMgdAllocation") %>%
      add_xml_data(L2252.LN5_MgdAllocation_bio, "LN5_MgdAllocation") %>%
      add_xml_data(L2252.LN5_MgdCarbon_crop, "LN5_MgdCarbon") %>%
      add_xml_data(L2252.LN5_MgdCarbon_bio, "LN5_MgdCarbon") %>%
      add_xml_data(L2252.LN5_LeafGhostShare, "LN5_LeafGhostShare") %>%
      add_xml_data(L2252.LN5_NodeGhostShare, "LN5_NodeGhostShare") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L2252.LN5_Logit",
                     "L2252.LN5_HistMgdAllocation_crop",
                     "L2252.LN5_MgdAllocation_crop",
                     "L2252.LN5_HistMgdAllocation_bio",
                     "L2252.LN5_MgdAllocation_bio",
                     "L2252.LN5_MgdCarbon_crop",
                     "L2252.LN5_MgdCarbon_bio",
                     "L2252.LN5_LeafGhostShare",
                     "L2252.LN5_NodeGhostShare",
                     "common/iso_GCAM_regID",
                     "water/basin_to_country_mapping",
                     "common/GCAM_region_names") ->
      land_input_5_IRR_MGMT.xml

    return_data(land_input_5_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
