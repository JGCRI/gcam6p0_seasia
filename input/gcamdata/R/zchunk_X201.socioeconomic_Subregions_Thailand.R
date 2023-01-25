# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_breakout_X201.socioeconomics_Subregions_Thailand
#'
#' Population and GDP of City Chosen and Rest of Region
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate}, \code{L201.LaborForceFillout}, \code{L201.PPPConvert}, \code{L201.BaseGDP_Scen}, \code{L201.Pop_gSSP1}, \code{L201.Pop_gSSP2}, \code{L201.Pop_gSSP3}, \code{L201.Pop_gSSP4}, \code{L201.Pop_gSSP5}, \code{L201.Pop_SSP1}, \code{L201.Pop_SSP2}, \code{L201.Pop_SSP3}, \code{L201.Pop_SSP4}, \code{L201.Pop_SSP5}, \code{L201.LaborProductivity_gSSP1}, \code{L201.LaborProductivity_gSSP2}, \code{L201.LaborProductivity_gSSP3}, \code{L201.LaborProductivity_gSSP4}, \code{L201.LaborProductivity_gSSP5}, \code{L201.LaborProductivity_SSP1}, \code{L201.LaborProductivity_SSP2}, \code{L201.LaborProductivity_SSP3}, \code{L201.LaborProductivity_SSP4}, and \code{L201.LaborProductivity_SSP5}. The corresponding file in the
#' original data system was \code{L201.Pop_GDP_scenarios.R} (socioeconomics level2).
#' @details Produces default interest rate by region, historical and future population by region and SSP scenario,
#' and uses per-capita GDP to calculate labor productivity by region and scenario.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by lag mutate order_by select transmute
#' @author PK, ZK & NB April 2021
module_breakout_X201.socioeconomics_Subregions_Thailand <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "breakout/Subregions_Thailand_pop",
             FILE = "breakout/Subregions_Thailand_pcgdp"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("X201.Pop_Subregions_Thailand",
             "X201.BaseGDP_Subregions_Thailand",
             "X201.LaborForceFillout_Subregions_Thailand",
             "X201.LaborProductivity_Subregions_Thailand",
             "X201.GDP_Subregions_Thailand"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    totalPop <- population <- year <- NULL  # silence package check notes

    # Load required inputs
    city_pop <- get_data(all_data,"breakout/Subregions_Thailand_pop", strip_attributes = TRUE)
    city_pcgdp <- get_data(all_data,"breakout/Subregions_Thailand_pcgdp", strip_attributes = TRUE)

    X201.Pop_Subregions_Thailand <- city_pop %>%
      complete(nesting(region), year = c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      group_by(region) %>%
      mutate(totalPop = round(approx_fun(year, population) * CONV_MIL_THOUS), socioeconomics.POP_DIGITS) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["Pop"]])

    L201.PCGDP_Subregions_Thailand <- city_pcgdp %>%
      complete(nesting(region), year = c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      group_by(region) %>%
      mutate(pcgdp = approx_fun(year, pcgdp) * gdp_deflator(1990, 2005)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS)

    # X201.BaseGDP_Subregions_Thailand:
    X201.BaseGDP_Subregions_Thailand <- filter(L201.PCGDP_Subregions_Thailand, year == min(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(X201.Pop_Subregions_Thailand, by = c("region", "year")) %>%
      mutate(baseGDP = round(pcgdp * totalPop, socioeconomics.GDP_DIGITS)) %>%
      select(LEVEL2_DATA_NAMES[["BaseGDP"]])

    X201.LaborForceFillout_Subregions_Thailand <- X201.BaseGDP_Subregions_Thailand %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             laborforce = socioeconomics.DEFAULT_LABORFORCE) %>%
      select(LEVEL2_DATA_NAMES[["LaborForceFillout"]])

    X201.LaborProductivity_Subregions_Thailand <- L201.PCGDP_Subregions_Thailand %>%
      group_by(region) %>%
      mutate(timestep = year - lag(year),
             pcgdp_ratio = pcgdp / lag(pcgdp),
             laborproductivity = round(pcgdp_ratio ^ (1 / timestep) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      ungroup() %>%
      drop_na() %>%
      select(LEVEL2_DATA_NAMES[["LaborProductivity"]])

    X201.GDP_Subregions_Thailand <- left_join_error_no_match(X201.Pop_Subregions_Thailand, L201.PCGDP_Subregions_Thailand, by = c("region", "year")) %>%
      mutate(GDP = totalPop * pcgdp) %>%
      select(region, year, GDP)

    # ===================================================

    # Produce outputs
    X201.Pop_Subregions_Thailand %>%
      add_title("City and rest-of-Region Population") %>%
      add_units("thousand persons") %>%
      add_comments("computed off-line") %>%
      add_precursors("breakout/Subregions_Thailand_pop") ->
      X201.Pop_Subregions_Thailand

    X201.BaseGDP_Subregions_Thailand %>%
      add_title("City and rest-of-Region base GDP") %>%
      add_units("million 1990 USD") %>%
      add_comments("population and GDP of these sub-regions are computed off-line") %>%
      add_precursors("breakout/Subregions_Thailand_pop","breakout/Subregions_Thailand_pcgdp") ->
      X201.BaseGDP_Subregions_Thailand

    X201.LaborForceFillout_Subregions_Thailand %>%
      add_title("City and rest-of-Region labor force participation rate") %>%
      add_units("unitless") %>%
      add_comments("default value") %>%
      add_precursors("breakout/Subregions_Thailand_pcgdp") ->
      X201.LaborForceFillout_Subregions_Thailand

    X201.LaborProductivity_Subregions_Thailand %>%
      add_title("City and rest-of-Region labor productivity growth rate") %>%
      add_units("unitless") %>%
      add_comments("calculated from per-capita GDP growth rate") %>%
      add_precursors("breakout/Subregions_Thailand_pop","breakout/Subregions_Thailand_pcgdp") ->
      X201.LaborProductivity_Subregions_Thailand

    X201.GDP_Subregions_Thailand %>%
      add_title("City and rest-of-Region total GDP") %>%
      add_units("million 1990 USD") %>%
      add_comments("calculated from per-capita GDP and population") %>%
      add_precursors("breakout/Subregions_Thailand_pop", "breakout/Subregions_Thailand_pcgdp") ->
      X201.GDP_Subregions_Thailand


    return_data(X201.Pop_Subregions_Thailand,
                X201.BaseGDP_Subregions_Thailand,
                X201.LaborForceFillout_Subregions_Thailand,
                X201.LaborProductivity_Subregions_Thailand,
                X201.GDP_Subregions_Thailand)
  } else {
    stop("Unknown command")
  }
}
