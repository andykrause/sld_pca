#***************************************************************************************************
#
#  Script for reproducing SLD_PCA Results
#
#***************************************************************************************************

### Setup ------------------------------------------------------------------------------------------

  # Load Libraries
  library(hpiR)
  library(tidyverse)
  require(sf)
  library(sp)
  library(magrittr)

  # Read in Data
  sales_df <- readRDS(file.path(getwd(), paste0('all_sales.rds')))

  # Trim to recent years
  x_df <- sales_df %>%
    dplyr::filter(sale_date >= as.Date('2017-01-01')) %>%
    dplyr::rename(price = sale_price)

  # Split to Train and Validate
  val_periods <- 2
  train_df <- x_df %>%
    dplyr::filter(trans_period <= max(x_df$trans_period) - val_periods)

  validate_df <- x_df %>%  
    dplyr::filter(trans_period > max(x_df$trans_period) - val_periods) %>%
    dplyr::mutate(trans_period = max(train_df$trans_period))

### Linear Modeling --------------------------------------------------------------------------------

## Set the Base Specification

  lm_spec <- as.formula(log(price) ~ year_built + sqft + grade + condition + beds + baths +
                          log(sqft_lot) + view_score + wfnt + as.factor(trans_period))

  # Base Model
  base_lm <- lm(lm_spec, data = train_df)
  validate_df <- validate_df %>%
    dplyr::mutate(base_lm = exp(predict(base_lm, .)),
                  base_lm_error = round(log(base_lm) - log(price), 3))

  # Add Submarkets
  subm_lm <- lm(update(lm_spec, . ~ . + as.factor(submarket)), data = train_df)
  validate_df <- validate_df %>%
    dplyr::mutate(subm_lm = exp(predict(subm_lm, .)),
                  subm_lm_error = round(log(subm_lm) - log(price), 3))

  # Add all SLD
  sld_lm <- lm(update(lm_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5), data = train_df)
  #sld_lm <- lm(update(lm_spec, . ~ . + SLD_1 + SLD_2), data = train_df)
  
  validate_df <- validate_df %>%
    dplyr::mutate(sld_lm = exp(predict(sld_lm, .)),
                  sld_lm_error = round(log(sld_lm) - log(price), 3))

### Non-linear models ------------------------------------------------------------------------------

  # Set non-linear model specification (random forest)
  rf_spec <- as.formula(log(price) ~ year_built + sqft + grade + condition + beds + baths +
                          sqft_lot + view_score + wfnt + trans_period)

  # Hyperparameters
  ntrees <- 250
  min_node_size <- 10

  # Base Model
  base_rf <- ranger::ranger(rf_spec,
                            data = train_df,
                            num.trees = ntrees,
                            min.node.size = min_node_size)
  validate_df <- validate_df %>%
    dplyr::mutate(base_rf = exp(predict(base_rf, .)$predictions),
                  base_rf_error = round(log(base_rf) - log(price), 3))

  # Add Space
  subm_rf <- ranger::ranger(update(rf_spec, . ~ . + submarket),
                            data = train_df,
                            num.trees = ntrees,
                            min.node.size = min_node_size)
  validate_df <- validate_df %>%
    dplyr::mutate(subm_rf = exp(predict(subm_rf, .)$predictions),
                  subm_rf_error = round(log(subm_rf) - log(price), 3))

  sld_rf <- ranger::ranger(update(rf_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5),
                           data = train_df,
                           num.trees = ntrees,
                           min.node.size = min_node_size)
  
  validate_df <- validate_df %>%
    dplyr::mutate(sld_rf = exp(predict(sld_rf, .)$predictions),
                  sld_rf_error = round(log(sld_rf) - log(price), 3))

### Spatial Autocorrelation Tests ---------

  sp_df <- SpatialPointsDataFrame(
    SpatialPoints(cbind(validate_df$longitude, validate_df$latitude)),
    validate_df)
  nbl <- sp_df %>%
    spdep::knearneigh(., k = 5, longlat = NULL, RANN=TRUE) %>%
    spdep::knn2nb(.) %>%
    spdep::nb2listw(.)

  #
  err <- grep('error', names(validate_df))
  spac_ <- list()

  for(i in 1:length(err)){
    x <- validate_df[, err[i]]
    spac_[[i]] <- spdep::moran.test(unlist(x), nbl)
  }
  names(spac_) <- names(validate_df)[err]

### Summary ----

summary_df = data.frame(model = c(rep('linear', 3), rep('rf', 3)),
                        spec = rep(c('base', 'subm', 'sld')),
                        fit = c(summary(base_lm)$r.squared, summary(subm_lm)$r.squared,
                                summary(sld_lm)$r.squared,
                                base_rf$r.squared, subm_rf$r.squared,
                                sld_rf$r.squared),
                        mdape = c(median(abs(validate_df$base_lm_error)),
                                  median(abs(validate_df$subm_lm_error)),
                                  median(abs(validate_df$sld_lm_error)),
                                  median(abs(validate_df$base_rf_error)),
                                  median(abs(validate_df$subm_rf_error)),
                                  median(abs(validate_df$sld_rf_error))),
                        mdpe = c(median(validate_df$base_lm_error),
                                 median(validate_df$subm_lm_error),
                                 median(validate_df$sld_lm_error),
                                 median(validate_df$base_rf_error),
                                 median(validate_df$subm_rf_error),
                                 median(validate_df$sld_rf_error)),
                        spac = c(spac_$base_lm_error$statistic,
                                 spac_$subm_lm_error$statistic,
                                 spac_$sld_lm_error$statistic,
                                 spac_$base_rf_error$statistic,
                                 spac_$subm_rf_error$statistic,
                                 spac_$sld_rf_error$statistic))

### Interpretatability -----

## Linear Models
lmsld_coefs <- sld_lm$coefficients[grep('SLD', names(sld_lm$coefficients))]

lmint_df <- data.frame(var_stds = seq(-3, 3, by = .25)) %>%
  dplyr::mutate(sld1 = var_stds * lmsld_coefs[1],
                sld2 = var_stds * lmsld_coefs[2],
                sld3 = var_stds * lmsld_coefs[3],
                sld4 = var_stds * lmsld_coefs[4],
                sld5 = var_stds * lmsld_coefs[5],
                model = 'lm')
# 
# Set up simulation data.  Here we take just 100 random sales from data
sim_df <- hpiR::rfSimDf(rf_df = validate_df,
                        seed = 1,
                        sim_count = 25)
sim_df$SLD_1 <- as.numeric(sim_df$SLD_1)
sim_df$SLD_2 <- as.numeric(sim_df$SLD_2)
sim_df$SLD_3 <- as.numeric(sim_df$SLD_3)
sim_df$SLD_4 <- as.numeric(sim_df$SLD_4)
sim_df$SLD_5 <- as.numeric(sim_df$SLD_5)

# Calculate Partial dependency
sld1_pdp <- pdp::partial(object = sld_rf,
                         train = sim_df,
                         pred.var = "SLD_1",
                         pred.grid = data.frame(SLD_1 = lmint_df$var_stds))
sld2_pdp <- pdp::partial(object = sld_rf,
                         train = sim_df,
                         pred.var = "SLD_2",
                         pred.grid = data.frame(SLD_2 = lmint_df$var_stds))
sld3_pdp <- pdp::partial(object = sld_rf,
                         train = sim_df,
                         pred.var = "SLD_3",
                         pred.grid = data.frame(SLD_3 = lmint_df$var_stds))
sld4_pdp <- pdp::partial(object = sld_rf,
                         train = sim_df,
                         pred.var = "SLD_4",
                         pred.grid = data.frame(SLD_4 = lmint_df$var_stds))
sld5_pdp <- pdp::partial(object = sld_rf,
                         train = sim_df,
                         pred.var = "SLD_5",
                         pred.grid = data.frame(SLD_5 = lmint_df$var_stds))

rfint_df <- data.frame(var_stds = seq(-3, 3, by = .25)) %>%
  dplyr::mutate(sld1 = sld1_pdp$yhat - mean(sld1_pdp$yhat),
                sld2 = sld2_pdp$yhat - mean(sld2_pdp$yhat),
                sld3 = sld3_pdp$yhat - mean(sld3_pdp$yhat),
                sld4 = sld4_pdp$yhat - mean(sld4_pdp$yhat),
                sld5 = sld5_pdp$yhat - mean(sld5_pdp$yhat),
                model = 'rf')

int_df <- dplyr::bind_rows(lmint_df, rfint_df) %>%
  tidyr::pivot_longer(., cols = tidyselect::starts_with('sld')) %>%
  dplyr::mutate(sld_var = gsub('x', '', name)) %>%
  dplyr::select(-name)
# 
ggplot(int_df, aes(x = var_stds, y= value, color = model)) +
  facet_wrap(~sld_var) +
  geom_line() +
  scale_color_manual(name = 'Model Type',
                     values = c('blue', 'orange'),
                     labels = c('Linear', 'Random Forest')) +
  xlab('\nSLD Variable Value (Normalized)') +
  ylab('Impact on Value (%)\n') +
  theme(legend.position = 'bottom')

ggplot(train_df, aes(x = SLD_1)) + geom_density() + coord_cartesian(xlim = c(-3, 3))
ggplot(train_df, aes(x = SLD_2)) + geom_density() + coord_cartesian(xlim = c(-3, 3))


ggplot(int_df %>% dplyr::filter(sld_var == 'sld1'), 
       aes(x = var_stds, y= value, color = model)) +
  #facet_wrap(~sld_var) +
  geom_line(size = 1.5) +
  scale_color_manual(name = 'Model Type',
                     values = c('blue', 'orange'),
                     labels = c('Linear', 'Random Forest')) +
  xlab('\nSLD Variable Value (Normalized)') +
  ylab('Impact on Value (%)\n') +
  ggtitle('Impact of SLD_1 on Home Values')+ 
  theme(legend.position = 'bottom')


ggplot(int_df %>% dplyr::filter(sld_var == 'sld2'), 
       aes(x = var_stds, y= value, color = model)) +
  #facet_wrap(~sld_var) +
  geom_line(size = 1.5) +
  scale_color_manual(name = 'Model Type',
                     values = c('blue', 'orange'),
                     labels = c('Linear', 'Random Forest')) +
  xlab('\nSLD Variable Value (Normalized)') +
  ylab('Impact on Value (%)\n') +
  ggtitle('Impact of SLD_2 on Home Values')+ 
  theme(legend.position = 'bottom')

# 
# ### Save Data ---------
# 
# saveRDS(summary_df, file.path(getwd(), paste0('summary.rds')))
# saveRDS(int_df, file.path(getwd(), paste0('interpretability.rds')))
# 
# ### Functions ----
# 
# sldLMEval <- function(train_df,
#                       validate_df,
#                       model_spec,
#                       spec_name,
#                       ...){
#   
#   cat('Building Model: ', spec_name, '\n')
#   lm_obj <- lm(model_spec, data = train_df)
#   
#   cat('Calculating Predictive Ability: ', spec_name, '\n')
#   validate_df <- validate_df %>%
#     dplyr::mutate(pred = exp(predict(lm_obj, .)),
#                   error = round(log(lm_obj) - log(price), 3))
#   
#   cat('Calculating Spatial Autocorrelation: ', spec_name, '\n')
#   sp_df <- SpatialPointsDataFrame(
#     SpatialPoints(cbind(validate_df$longitude, validate_df$latitude)),
#     validate_df)
#   
#   nbl <- sp_df %>%
#     spdep::knearneigh(., k = 5, longlat = NULL, RANN=TRUE) %>%
#     spdep::knn2nb(.) %>%
#     spdep::nb2listw(.)
#   spac <- spdep::moran.test(unlist(x), nbl)
#   
#   list(summary = data.frame(model = 'linear',
#                             spec = spec_name,
#                             fit = summary(lm_obj)$r.squared,
#                             mdape = median(abs(validate_df$error)),
#                             border_mdape = median(abs(validate_df$error[validate_df$border])),
#                             spac = spac$statistic),
#        int = lm_obj$coefficients)
# }
# 
# ### Summary of Data ------------
# 
# 
# 
