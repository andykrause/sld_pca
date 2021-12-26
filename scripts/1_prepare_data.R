#***************************************************************************************************
#
#  Script for reproducing SLD_PCA Results
#
#***************************************************************************************************

### Setup ------------------------------------------------------------------------------------------

  # Load Libraries
  require(kingCoData) #devtools::install_github('anonymousREAuthor/kingCoData')
  require(hpiR)
  require(tidyverse)
  require(sp)
  require(spdep)
  require(tidycensus)
  require(sf)

  # Create directory for results if it doesn't exist
  dir.create(file.path(getwd(), 'results'))

  # Load data from packages
  data(kingco_sales, package = 'kingCoData')

  # Load local data
  submarket_df <- readRDS(file.path(getwd(), 'data', 'submarkets.RDS'))
  
  # Set Census API
  tidycensus::census_api_key("eba9fd9d7bd7f7164ed95b4c7f8874cf37586d42", install=TRUE,
                             overwrite = TRUE)

### Data and Model Prep ----------------------------------------------------------------------------

  ## SLD Data
  sld_df <- readRDS(file.path(getwd(), 'raw_data', 'sld.RDS'))

  ## Census Block Group Data
  bg_sf <-  tidycensus::get_acs(geography = 'block group',
                                state = 'WA',
                                year = 2018,
                                variables = 'B02001_001',
                                geometry = T)

  ## Sales Data

  # Add submarket designations to data
  raw_df <- kingco_sales %>%
    dplyr::left_join(., submarket_df, by = 'area')

  # Combine bathrooms and sum up views, create wfnt and townhomes
  raw_df <- raw_df %>%
    dplyr::mutate(baths = bath_full + (.75 * bath_3qtr) + (.5 * bath_half),
                  view_score = view_rainier + view_olympics + view_cascades + view_territorial +
                    view_skyline + view_sound + view_lakewash + view_lakesamm,
                    wfnt = ifelse(wfnt == 0, 0, 1),
                    townhome = ifelse(present_use == 29, 1, 0))

  # Remove obvious bad homes
  sales_df <- raw_df %>%
    dplyr::filter(sqft > 400 &
                  baths > 0 &
                  beds > 0 &
                  beds < 33 &
                  sqft_1 <= sqft_lot &
                  year_built <= lubridate::year(sale_date))

  # Add Trans Period
  sales_df <- hpiR::dateToPeriod(trans_df = sales_df,
                           date = 'sale_date',
                           periodicity = 'monthly')

### Join Spatial data ------------------------------------------------------------------------------

  # Sales to spatial
  sales_sf <- sf::st_as_sf(sales_df,
                           coords = c("longitude", "latitude"),
                           crs = 4269,
                           remove = FALSE)

  # Add Block Group Designations to sales
  sales_sf <- sf::st_join(sales_sf, bg_sf %>%
                            dplyr::select(GEOID))

  # Add PCA data

  sales_sf <- sales_sf %>%
    dplyr::mutate(GEOID = as.character(GEOID)) %>%
    dplyr::left_join(., sld_df %>%
                       dplyr::select(GEOID = GEOID10, SLD_1 = Factor_1_CSA,
                                     SLD_2 = Factor_2_CSA, SLD_3 = Factor_3_CSA,
                                     SLD_4 = Factor_4_CSA, SLD_5 = Factor_5_CSA) %>%
                       dplyr::mutate(GEOID = as.character(GEOID)),
                     by = 'GEOID') %>%
    dplyr::filter(!is.na(SLD_1))

  sales_sf$geometry <- NULL

  # Create normalized factor
  sales_sf <- sales_sf %>%
    dplyr::mutate_at(.vars = c('SLD_1', 'SLD_2', 'SLD_3', "SLD_4", "SLD_5"),
                     .funs = scale)
  
  # Add Border designation
  sales_df <- assignBorder(as.data.frame(sales_sf), k = 5)
  
### Save Data --------------------------------------------------------------------------------------
  
  saveRDS(sales_df, file.path(getwd(), 'data', paste0('all_sales.rds')))

#***************************************************************************************************
#***************************************************************************************************
  