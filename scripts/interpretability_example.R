#***************************************************************************************************
#
#  Example of using Random Forests with Partial Dependency plots to extract marginal effects
#
#***************************************************************************************************

  # Load Libraries
  library(kingCoData)   #devtools::install_github('andykrause/kingCoData')
  library(hpiR)         #devtools::install_github('andykrause/hpiR')
  library(tidyverse)
  library(ranger)
  library(pdp)

  # Load sample data
  data(kingco_sales, package = 'kingCoData')

### Data Prep --------------------------------------------------------------------------------------

  # Filter to recent sales
  raw_df <- kingco_sales %>%
    dplyr::filter(sale_date >= as.Date('2018-01-01'))

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

  # Add Transaction Period
  sales_df <- hpiR::dateToPeriod(trans_df = sales_df,
                                 date = 'sale_date',
                                 periodicity = 'monthly')

### Modeling ---------------------------------------------------------------------------------------

  # Set non-linear model specification (random forest)
  rf_spec <- as.formula(log(sale_price) ~ year_built + sqft + grade + condition + beds + baths +
                          sqft_lot + view_score + wfnt + trans_period + latitude + longitude)

  # Run Model
  mod_rf <- ranger::ranger(rf_spec, data = sales_df, importance = 'impurity')

  # Plot Feature Importance
  ggplot(stack(mod_rf$variable.importance), aes(ind, values)) +
    geom_col() +
    xlab('Variable\n') +
    ylab('\nRelative Measure of Importance') +
    coord_flip()

  # Extraction Predictions
  rf_pred <- mod_rf$predictions

  ## Gather Partial Dependency Plots

  # Set up simulation data.  Here we take just 100 random sales from data
  sim_df <- hpiR::rfSimDf(rf_df = sales_df,
                          seed = 1,
                          sim_count = 100)

  # Calculate Partial dependency (or how prediction changes if only that variable is changed)
  # For Year Built.  You can set range in the last parameter, here i'm using 1800 to 2019.
  yearbuilt_pdp <- pdp::partial(object = mod_rf,
                                train = sim_df,
                                pred.var = "year_built",
                                pred.grid = data.frame(year_built = 1880:2019))

  # Plot how the predicted home value of the median home in our 100 home sample would change based
  # on different hypothetical year builts

  ggplot(yearbuilt_pdp, aes(x = year_built, y = exp(yhat))) +
    geom_line() +
    ylab('Median Sale Price') +
    xlab('\n Year Built') +
    ggtitle('Partial Dependence Plot')

  # Square Foot example
  sqft_pdp <- pdp::partial(object = mod_rf,
                           train = sim_df,
                           pred.var = "sqft",
                           pred.grid = data.frame(sqft = seq(800, 5000, by = 25)))

  ggplot(sqft_pdp, aes(x = sqft, y = exp(yhat))) +
    geom_line() +
    ylab('Median Sale Price\n') +
    xlab('\n Home Size (Sq Ft)') +
    ggtitle('Partial Dependence Plot')

#***************************************************************************************************
#***************************************************************************************************


