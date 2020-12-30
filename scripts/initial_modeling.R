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

   # Read in Data
   sales_df <- readRDS(file.path(getwd(), paste0('all_sales.rds')))

   # Trim to recent years
   x_df <- sales_df %>%
     dplyr::filter(sale_date >= as.Date('2017-01-01')) %>%
     dplyr::rename(price = sale_price)

   # Split to Train and Validate
   val_periods <- 1
   train_df <- x_df %>%
     dplyr::filter(trans_period <= max(x_df$trans_period) - val_periods)
   validate_df <- x_df %>%
     dplyr::filter(trans_period > max(x_df$trans_period) - val_periods) %>%
     dplyr::mutate(trans_period = max(train_df$trans_period))

assignBorder <- function(validate_df,
                         k = 5){

   # Identify "Border" properties
   coords <- cbind(validate_df$longitude, validate_df$latitude)
   sp_df = SpatialPointsDataFrame(SpatialPoints(coords), validate_df)
   nbl <- sp_df %>%
     spdep::knearneigh(., k = 5, longlat = NULL, RANN=TRUE) %>%
     spdep::knn2nb(.)

   validate_df$border <- FALSE
   for (i in 1:nrow(validate_df)){
     nbls <- validate_df$submarket[nbl[[i]]]
     if (length(which(nbls != validate_df$submarket[i])) > 0){
       validate_df$border[i] <- TRUE
     }
   }
  validate_df
}

  train_df <- assignBorder(train_df, k = 5)
  validate_df <- assignBorder(validate_df, k = 5)

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

  # Add Lat/Long
  xy_lm <- lm(update(lm_spec, . ~ . + latitude + longitude + latitude:longitude), data = train_df)
  validate_df <- validate_df %>%
    dplyr::mutate(xy_lm = exp(predict(xy_lm, .)),
                  xy_lm_error = round(log(xy_lm) - log(price), 3))


  # Add SLD
  sld1_lm <- lm(update(lm_spec, . ~ . + SLD_1), data = train_df)
  validate_df <- validate_df %>%
    dplyr::mutate(sld1_lm = exp(predict(sld1_lm, .)),
                  sld1_lm_error = round(log(sld1_lm) - log(price), 3))

  # Add all SLD
  sld_lm <- lm(update(lm_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5), data = train_df)
  validate_df <- validate_df %>%
    dplyr::mutate(sld_lm = exp(predict(sld_lm, .)),
                  sld_lm_error = round(log(sld_lm) - log(price), 3))

  # SLD + Subm
  slds_lm <- lm(update(lm_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5 +
                         as.factor(submarket)), data = train_df)
  validate_df <- validate_df %>%
    dplyr::mutate(slds_lm = exp(predict(slds_lm, .)),
                  slds_lm_error = round(log(slds_lm) - log(price), 3))


  # SLD + Subm
  sldxy_lm <- lm(update(lm_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5 +
                          + latitude + longitude + latitude:longitude), data = train_df)
  validate_df <- validate_df %>%
    dplyr::mutate(sldxy_lm = exp(predict(sldxy_lm, .)),
                  sldxy_lm_error = round(log(sldxy_lm) - log(price), 3))


  # All
  all_lm <- lm(update(lm_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5 +
                          + latitude + longitude + latitude:longitude + as.factor(submarket)),
               data = train_df)
  validate_df <- validate_df %>%
    dplyr::mutate(all_lm = exp(predict(all_lm, .)),
                  all_lm_error = round(log(all_lm) - log(price), 3))

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


  # Add Space
  xy_rf <- ranger::ranger(update(rf_spec, . ~ . + latitude + longitude),
                          data = train_df,
                          num.trees = ntrees,
                          min.node.size = min_node_size)
  validate_df <- validate_df %>%
    dplyr::mutate(xy_rf = exp(predict(xy_rf, .)$predictions),
                  xy_rf_error = round(log(xy_rf) - log(price), 3))

  sld_rf <- ranger::ranger(update(rf_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5),
                            data = train_df,
                            num.trees = ntrees,
                            min.node.size = min_node_size)
  validate_df <- validate_df %>%
    dplyr::mutate(sld_rf = exp(predict(sld_rf, .)$predictions),
                  sld_rf_error = round(log(sld_rf) - log(price), 3))

  # SLD + Subm
  slds_rf <- ranger::ranger(update(rf_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5 +
                            submarket),
                            data = train_df,
                            num.trees = ntrees,
                            min.node.size = min_node_size)
  validate_df <- validate_df %>%
    dplyr::mutate(slds_rf = exp(predict(slds_rf, .)$predictions),
                  slds_rf_error = round(log(slds_rf) - log(price), 3))


  # SLD + LL
  sldxy_rf <- ranger::ranger(update(rf_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5 +
                                     latitude + longitude),
                             data = train_df,
                             num.trees = ntrees,
                             min.node.size = min_node_size)
  validate_df <- validate_df %>%
    dplyr::mutate(sldxy_rf = exp(predict(sldxy_rf, .)$predictions),
                  sldxy_rf_error = round(log(sldxy_rf) - log(price), 3))

  # SLD + LL
  all_rf <- ranger::ranger(update(rf_spec, . ~ . + SLD_1 + SLD_2 + SLD_3 + SLD_4 + SLD_5 +
                                      latitude + longitude + submarket),
                             data = train_df,
                             num.trees = ntrees,
                             min.node.size = min_node_size)
  validate_df <- validate_df %>%
    dplyr::mutate(all_rf = exp(predict(all_rf, .)$predictions),
                  all_rf_error = round(log(all_rf) - log(price), 3))


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

  b_idx <- which(validate_df$border)

  summary_df = data.frame(model = c(rep('linear', 7), rep('rf', 7)),
                          spec = rep(c('base', 'subm', 'xy', 'sld', 'sld+subm', 'sld+xy',
                                       'sld + xy + subm')),
                          fit = c(summary(base_lm)$r.squared, summary(subm_lm)$r.squared,
                                  summary(xy_lm)$r.squared, summary(sld_lm)$r.squared,
                                  summary(slds_lm)$r.squared, summary(sldxy_lm)$r.squared,
                                  summary(all_lm)$r.squared,
                                  base_rf$r.squared, subm_rf$r.squared,
                                  xy_rf$r.squared, sld_rf$r.squared,
                                  slds_rf$r.squared, sldxy_rf$r.squared,
                                  all_rf$r.squared),
                          mdape = c(median(abs(validate_df$base_lm_error)),
                                    median(abs(validate_df$subm_lm_error)),
                                    median(abs(validate_df$xy_lm_error)),
                                    median(abs(validate_df$sld_lm_error)),
                                    median(abs(validate_df$slds_lm_error)),
                                    median(abs(validate_df$sldxy_lm_error)),
                                    median(abs(validate_df$all_lm_error)),
                                    median(abs(validate_df$base_rf_error)),
                                    median(abs(validate_df$subm_rf_error)),
                                    median(abs(validate_df$xy_rf_error)),
                                    median(abs(validate_df$sld_rf_error)),
                                    median(abs(validate_df$slds_rf_error)),
                                    median(abs(validate_df$sldxy_rf_error)),
                                    median(abs(validate_df$all_rf_error))),
                          spac = c(spac_$base_lm_error$statistic,
                                   spac_$subm_lm_error$statistic,
                                   spac_$xy_lm_error$statistic,
                                   spac_$sld_lm_error$statistic,
                                   spac_$slds_lm_error$statistic,
                                   spac_$sldxy_lm_error$statistic,
                                   spac_$all_lm_error$statistic,
                                   spac_$base_rf_error$statistic,
                                   spac_$subm_rf_error$statistic,
                                   spac_$xy_rf_error$statistic,
                                   spac_$sld_rf_error$statistic,
                                   spac_$slds_rf_error$statistic,
                                   spac_$sldxy_rf_error$statistic,
                                   spac_$all_rf_error$statistic),
                          border_mdape = c(median(abs(validate_df$base_lm_error[b_idx])),
                                    median(abs(validate_df$subm_lm_error[b_idx])),
                                    median(abs(validate_df$xy_lm_error[b_idx])),
                                    median(abs(validate_df$sld_lm_error[b_idx])),
                                    median(abs(validate_df$slds_lm_error[b_idx])),
                                    median(abs(validate_df$sldxy_lm_error[b_idx])),
                                    median(abs(validate_df$all_lm_error[b_idx])),
                                    median(abs(validate_df$base_rf_error[b_idx])),
                                    median(abs(validate_df$subm_rf_error[b_idx])),
                                    median(abs(validate_df$xy_rf_error[b_idx])),
                                    median(abs(validate_df$sld_rf_error[b_idx])),
                                    median(abs(validate_df$slds_rf_error[b_idx])),
                                    median(abs(validate_df$sldxy_rf_error[b_idx])),
                                    median(abs(validate_df$all_rf_error[b_idx]))))

### Interpretatability -----

 ## Linear Models
  lmsld_coefs <- sld_lm$coefficients[grep('SLD', names(sld_lm$coefficients))]
  lmall_coefs <- all_lm$coefficients[grep('SLD', names(all_lm$coefficients))]

  lmint_df <- data.frame(var_stds = seq(-2, 2, by = .25)) %>%
    dplyr::mutate(sld1 = var_stds * lmsld_coefs[1],
                  sld2 = var_stds * lmsld_coefs[2],
                  sld3 = var_stds * lmsld_coefs[3],
                  sld4 = var_stds * lmsld_coefs[4],
                  sld5 = var_stds * lmsld_coefs[5],
                  sldx1 = var_stds * lmall_coefs[1],
                  sldx2 = var_stds * lmall_coefs[2],
                  sldx3 = var_stds * lmall_coefs[3],
                  sldx4 = var_stds * lmall_coefs[4],
                  sldx5 = var_stds * lmall_coefs[5],
                  model = 'lm')

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
  sldx1_pdp <- pdp::partial(object = all_rf,
                            train = sim_df,
                            pred.var = "SLD_1",
                            pred.grid = data.frame(SLD_1 = lmint_df$var_stds))
  sld2_pdp <- pdp::partial(object = sld_rf,
                           train = sim_df,
                           pred.var = "SLD_2",
                           pred.grid = data.frame(SLD_2 = lmint_df$var_stds))
  sldx2_pdp <- pdp::partial(object = all_rf,
                            train = sim_df,
                            pred.var = "SLD_2",
                            pred.grid = data.frame(SLD_2 = lmint_df$var_stds))
  sld3_pdp <- pdp::partial(object = sld_rf,
                           train = sim_df,
                           pred.var = "SLD_3",
                           pred.grid = data.frame(SLD_3 = lmint_df$var_stds))
  sldx3_pdp <- pdp::partial(object = all_rf,
                            train = sim_df,
                            pred.var = "SLD_3",
                            pred.grid = data.frame(SLD_3 = lmint_df$var_stds))
  sld4_pdp <- pdp::partial(object = sld_rf,
                           train = sim_df,
                           pred.var = "SLD_4",
                           pred.grid = data.frame(SLD_4 = lmint_df$var_stds))
  sldx4_pdp <- pdp::partial(object = all_rf,
                            train = sim_df,
                            pred.var = "SLD_4",
                            pred.grid = data.frame(SLD_4 = lmint_df$var_stds))
  sld5_pdp <- pdp::partial(object = sld_rf,
                           train = sim_df,
                           pred.var = "SLD_5",
                           pred.grid = data.frame(SLD_5 = lmint_df$var_stds))
  sldx5_pdp <- pdp::partial(object = all_rf,
                            train = sim_df,
                            pred.var = "SLD_5",
                            pred.grid = data.frame(SLD_5 = lmint_df$var_stds))




  rfint_df <- data.frame(var_stds = seq(-2, 2, by = .25)) %>%
    dplyr::mutate(sld1 = sld1_pdp$yhat - mean(sld1_pdp$yhat),
                  sld2 = sld2_pdp$yhat - mean(sld2_pdp$yhat),
                  sld3 = sld3_pdp$yhat - mean(sld3_pdp$yhat),
                  sld4 = sld4_pdp$yhat - mean(sld4_pdp$yhat),
                  sld5 = sld5_pdp$yhat - mean(sld5_pdp$yhat),
                  sldx1 = sldx1_pdp$yhat - mean(sldx1_pdp$yhat),
                  sldx2 = sldx2_pdp$yhat - mean(sldx2_pdp$yhat),
                  sldx3 = sldx3_pdp$yhat - mean(sldx3_pdp$yhat),
                  sldx4 = sldx4_pdp$yhat - mean(sldx4_pdp$yhat),
                  sldx5 = sldx5_pdp$yhat - mean(sldx5_pdp$yhat),
                  model = 'rf')

  int_df <- dplyr::bind_rows(lmint_df, rfint_df) %>%
    tidyr::pivot_longer(., cols = tidyselect::starts_with('sld')) %>%
    dplyr::mutate(spec = ifelse(grepl('x', name), 'all spatial', 'sld-only'),
                  sld_var = gsub('x', '', name)) %>%
    dplyr::select(-name)

  ggplot(int_df, aes(x = var_stds, y= value, color = model, linetype = spec)) +
    facet_wrap(~sld_var) +
    geom_line() +
    scale_color_manual(name = 'Model Type',
                       values = c('blue', 'orange'),
                       labels = c('Linear', 'Random Forest')) +
    scale_linetype_manual(name = 'Model Specification',
                          values = c(2,1),
                          labels = c('All Spatial', 'SLD Only')) +
    xlab('\nSLD Variable Value (Normalized)') +
    ylab('Impact on Value (%)\n') +
    theme(legend.position = 'bottom')

### Save Data ---------

  saveRDS(summary_df, file.path(getwd(), paste0('summary.rds')))
  saveRDS(int_df, file.path(getwd(), paste0('interpretability.rds')))

### Functions ----

sldLMEval <- function(train_df,
                      validate_df,
                      model_spec,
                      spec_name,
                      ...){

  cat('Building Model: ', spec_name, '\n')
  lm_obj <- lm(model_spec, data = train_df)

  cat('Calculating Predictive Ability: ', spec_name, '\n')
  validate_df <- validate_df %>%
    dplyr::mutate(pred = exp(predict(lm_obj, .)),
                  error = round(log(lm_obj) - log(price), 3))

  cat('Calculating Spatial Autocorrelation: ', spec_name, '\n')
  sp_df <- SpatialPointsDataFrame(
    SpatialPoints(cbind(validate_df$longitude, validate_df$latitude)),
    validate_df)

  nbl <- sp_df %>%
    spdep::knearneigh(., k = 5, longlat = NULL, RANN=TRUE) %>%
    spdep::knn2nb(.) %>%
    spdep::nb2listw(.)
  spac <- spdep::moran.test(unlist(x), nbl)

  list(summary = data.frame(model = 'linear',
                            spec = spec_name,
                            fit = summary(lm_obj)$r.squared,
                            mdape = median(abs(validate_df$error)),
                            border_mdape = median(abs(validate_df$error[validate_df$border])),
                            spac = spac$statistic),
       int = lm_obj$coefficients)
}
