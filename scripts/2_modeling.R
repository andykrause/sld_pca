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
  require(pdp)
  library(ggpubr)
  theme_set(theme_pubr())

  # Read in Data
  sales_df <- readRDS(file.path(getwd(), 'data', paste0('all_sales.rds')))

  # Trim to recent years
  sales_df <- sales_df %>%
    dplyr::filter(sale_date >= as.Date('2015-01-01')) %>%
    dplyr::rename(price = sale_price)

### Model Fitting ----------------------------------------------------------------------------------  
  
  ## Set the Base Specifications
  
  lm_spec <- as.formula(log(price) ~ year_built + sqft + grade + condition + beds + baths +
                          log(sqft_lot) + view_score + wfnt + as.factor(trans_period))
  
  sales_df$log_sqft_lot <- log(sales_df$sqft_lot)
  rf_spec <- as.formula(log(price) ~ year_built + sqft + grade + condition + beds + baths +
                           log_sqft_lot + view_score + wfnt + trans_period)
  
  # Hyperparameters
  ntrees <- 250
  min_node_size <- 10
  
  ## Fit linear models
  base_lm <- lm(lm_spec, 
                data = sales_df)
  subm_lm <- lm(update(lm_spec, . ~ . + as.factor(submarket)), 
                data = sales_df)
  xy_lm <- lm(update(lm_spec, . ~ . + latitude + longitude + latitude:longitude +
                       I(latitude ^ 2)+ I(longitude ^2)), 
              data = sales_df)
  sld_lm <- lm(update(lm_spec, . ~ . + SLD_1 + SLD_2), 
                data = sales_df)
  sldsub_lm <- lm(update(as.formula(subm_lm), . ~ . + SLD_1 + SLD_2), 
                data = sales_df)
  sldxy_lm <- lm(update(as.formula(xy_lm), . ~ . + SLD_1 + SLD_2), 
                 data = sales_df)
  all_lm <- lm(update(as.formula(sldxy_lm), . ~ . + as.factor(submarket)),
               data = sales_df)
  
  ## Fit random forest models
  base_rf <- ranger::ranger(rf_spec,
                            data = sales_df,
                            num.trees = ntrees,
                            min.node.size = min_node_size)
  
  subm_rf <- ranger::ranger(update(rf_spec, . ~ . + submarket),
                            data = sales_df,
                            num.trees = ntrees,
                            min.node.size = min_node_size)
  
  xy_rf <- ranger::ranger(update(rf_spec, . ~ . + latitude + longitude),
                            data = sales_df,
                            num.trees = ntrees,
                            min.node.size = min_node_size)
  
  sld_rf <- ranger::ranger(update(rf_spec, . ~ . + SLD_1 + SLD_2),
                          data = sales_df,
                          num.trees = ntrees,
                          min.node.size = min_node_size)
  
  sldsub_rf <- ranger::ranger(update(rf_spec, . ~ . + SLD_1 + SLD_2 + submarket),
                              data = sales_df,
                              num.trees = ntrees,
                              min.node.size = min_node_size)
  
  sldxy_rf <- ranger::ranger(update(rf_spec, . ~ . + SLD_1 + SLD_2 + latitude + longitude),
                             data = sales_df,
                             num.trees = ntrees,
                             min.node.size = min_node_size)
  
  all_rf <- ranger::ranger(update(rf_spec, . ~ . + SLD_1 + SLD_2 + latitude + longitude + 
                                    submarket),
                           data = sales_df,
                           num.trees = ntrees,
                           min.node.size = min_node_size)
  
### Analyze Fit ------------------------------------------------------------------------------------
  
  ## Basic Model fit
  fitsumm_df = data.frame(model = c(rep('linear', 7), rep('rf', 7)),
                          spec = rep(c('base', 'subm', 'sld', 'xy', 'subm+sld',
                                       'xy+sld', 'all')),
                          fit = c(summary(base_lm)$r.squared, summary(subm_lm)$r.squared,
                                  summary(sld_lm)$r.squared, summary(xy_lm)$r.squared,
                                  summary(sldsub_lm)$r.squared, summary(sldxy_lm)$r.squared,
                                  summary(all_lm)$r.squared,
                                  base_rf$r.squared, subm_rf$r.squared, sld_rf$r.squared,
                                  xy_rf$r.squared, sldsub_rf$r.squared, sldxy_rf$r.squared,
                                  all_rf$r.squared))
                          
  
### Plot Impact ------------------------------------------------------------------------------------
  
  ## Linear Models
  lmsld_coefs <- sld_lm$coefficients[grep('SLD', names(sld_lm$coefficients))]
  
  lmint_df <- data.frame(var_stds = seq(-3, 3, by = .25)) %>%
    dplyr::mutate(sld1 = var_stds * lmsld_coefs[1],
                  sld2 = var_stds * lmsld_coefs[2],
                  model = 'lm')
  ## Random Forest   
  # Set up simulation data.  Here we take just 100 random sales from data
  sim_df <- hpiR::rfSimDf(rf_df = sales_df,
                          seed = 1,
                          sim_count = 100)
  sim_df$SLD_1 <- as.numeric(sim_df$SLD_1)
  sim_df$SLD_2 <- as.numeric(sim_df$SLD_2)
  
  # Calculate Partial dependency
  sld1_pdp <- pdp::partial(object = sld_rf,
                           train = sim_df,
                           pred.var = "SLD_1",
                           pred.grid = data.frame(SLD_1 = lmint_df$var_stds))
  sld2_pdp <- pdp::partial(object = sld_rf,
                           train = sim_df,
                           pred.var = "SLD_2",
                           pred.grid = data.frame(SLD_2 = lmint_df$var_stds))
 
  
  rfint_df <- data.frame(var_stds = seq(-3, 3, by = .25)) %>%
    dplyr::mutate(sld1 = sld1_pdp$yhat - mean(sld1_pdp$yhat),
                  sld2 = sld2_pdp$yhat - mean(sld2_pdp$yhat),
                  model = 'rf')
  
  ## Create Density measures
  dens1_df <- sales_df %>% 
    dplyr::select(SLD_1, SLD_2) %>%
    dplyr::mutate(sld1 = cut(SLD_1, rfint_df$var_stds)) %>%
    dplyr::group_by(sld1) %>%
    dplyr::summarize(sld1_count = n()) %>%
    dplyr::mutate(start = stringr::str_locate(sld1, ',')[,1] + 1,
                  end = stringr::str_locate(sld1, ']')[,1] - 1,
                  sld1_value = as.numeric(substr(sld1, start, end)),
                  sld1_ratio = sld1_count / nrow(sales_df),
                  sld_var = 'sld1')

  dens2_df <- sales_df %>% 
    dplyr::select(SLD_1, SLD_2) %>%
    dplyr::mutate(sld2 = cut(SLD_2, rfint_df$var_stds)) %>%
    dplyr::group_by(sld2) %>%
    dplyr::summarize(sld2_count = n()) %>%
    dplyr::mutate(start = stringr::str_locate(sld2, ',')[,1] + 1,
                  end = stringr::str_locate(sld2, ']')[,1] - 1,
                  sld2_value = as.numeric(substr(sld2, start, end)),
                  sld2_ratio = sld2_count / nrow(sales_df),
                  sld_var = 'sld2')
  
  int_df <- dplyr::bind_rows(lmint_df, rfint_df) %>%
    tidyr::pivot_longer(., cols = tidyselect::starts_with('sld')) %>%
    dplyr::mutate(sld_var = gsub('x', '', name)) %>%
    dplyr::select(-name) %>%
    dplyr::left_join(., dens1_df %>%
                       dplyr::select(sld_var, var_stds = sld1_value, sld1_ratio),
                     by = c('sld_var', 'var_stds')) %>%
    dplyr::mutate(sld1_ratio = ifelse(is.na(sld1_ratio), 0, sld1_ratio)) %>%
    dplyr::left_join(., dens2_df %>%
                       dplyr::select(sld_var, var_stds = sld2_value, sld2_ratio),
                     by = c('sld_var', 'var_stds')) %>%
    dplyr::mutate(sld2_ratio = ifelse(is.na(sld2_ratio), 0, sld2_ratio)) %>%
    dplyr::mutate(sld_ratio = ifelse(sld1_ratio > sld2_ratio, sld1_ratio, sld2_ratio))
  
  # 
  ggplot(int_df, aes(x = var_stds, y= value, color = model)) +
    facet_wrap(~sld_var) +
    geom_line() +
    scale_color_manual(name = 'Model Type',
                       values = c('blue', 'orange'),
                       labels = c('Linear', 'Random Forest')) +
    xlab('\nSLD Variable Value (Normalized)') +
    ylab('Impact on Value (%)\n') +
    theme(legend.position = 'bottom') ->
  impact_plot
  impact_plot
  
  ggplot(int_df, aes(x = var_stds, y= sld_ratio)) +
    facet_wrap(~sld_var) +
    geom_line() +
    xlab('\nSLD Variable Value (Normalized)') +
    ylab('SLD Factor Density (% of Obs)\n') +
    theme(legend.position = 'bottom') ->
  dens_plot
  dens_plot
  
  gridExtra::grid.arrange(impact_plot, dens_plot, nrow=2) ->
    combined_plot
  
### 
  
  
  
  # Split to Train and Validate
  val_periods <- 2
  train_df <- x_df %>%
    dplyr::filter(trans_period <= max(x_df$trans_period) - val_periods)

  validate_df <- x_df %>%  
    dplyr::filter(trans_period > max(x_df$trans_period) - val_periods) %>%
    dplyr::mutate(trans_period = max(train_df$trans_period))

  
### Predictive Impact --------------------------------------------------------------------------------

  periods <- 205:252
  
  baselm_ <- purrr::map(.x = periods,
                        .f = ootPredict,
                        sales_df = sales_df,
                        mod_spec = as.formula(base_lm),
                        model_type = 'linear',
                        train_window = 12,
                        pred_window = 1)
  baselm_df <- baselm_ %>%
    dplyr::bind_rows()
  median(abs(baselm_df$error))
  
  submlm_ <- purrr::map(.x = periods,
                        .f = ootPredict,
                        sales_df = sales_df,
                        mod_spec = as.formula(subm_lm),
                        model_type = 'linear',
                        train_window = 12,
                        pred_window = 1)
  submlm_df <- submlm_ %>%
    dplyr::bind_rows()
  median(abs(submlm_df$error))
  
  sldlm_ <- purrr::map(.x = periods,
                        .f = ootPredict,
                        sales_df = sales_df,
                        mod_spec = as.formula(sld_lm),
                        model_type = 'linear',
                        train_window = 12,
                        pred_window = 1)
  sldlm_df <- sldlm_ %>%
    dplyr::bind_rows()
  median(abs(sldlm_df$error))
  
  sslm_ <- purrr::map(.x = periods,
                       .f = ootPredict,
                       sales_df = sales_df,
                       mod_spec = as.formula(sldsub_lm),
                       model_type = 'linear',
                       train_window = 12,
                       pred_window = 1)
  sslm_df <- sslm_ %>%
    dplyr::bind_rows()
  median(abs(sslm_df$error))
  
  baserf_ <- purrr::map(.x = periods,
                        .f = ootPredict,
                        sales_df = sales_df,
                        mod_spec = rf_spec,
                        model_type = 'randomforest',
                        train_window = 12,
                        pred_window = 1)
  baserf_df <- baserf_ %>%
    dplyr::bind_rows()
  median(abs(baserf_df$error))
  
  submrf_ <- purrr::map(.x = periods,
                        .f = ootPredict,
                        sales_df = sales_df,
                        mod_spec = update(rf_spec, . ~ . + submarket),
                        model_type = 'randomforest',
                        train_window = 12,
                        pred_window = 1)
  submrf_df <- submrf_ %>%
    dplyr::bind_rows()
  median(abs(submrf_df$error))
  
  sldrf_ <- purrr::map(.x = periods,
                        .f = ootPredict,
                        sales_df = sales_df,
                        mod_spec = update(rf_spec, . ~ . + SLD_1 + SLD_2),
                        model_type = 'randomforest',
                        train_window = 12,
                        pred_window = 1)
  sldrf_df <- sldrf_ %>%
    dplyr::bind_rows()
  median(abs(sldrf_df$error))
  
  ssrf_ <- purrr::map(.x = periods,
                       .f = ootPredict,
                       sales_df = sales_df,
                       mod_spec = update(rf_spec, . ~ . + SLD_1 + SLD_2 + submarket),
                       model_type = 'randomforest',
                       train_window = 12,
                       pred_window = 1)
  ssrf_df <- ssrf_ %>%
    dplyr::bind_rows()
  median(abs(ssrf_df$error))
  
### Out of Space  
  
  submlmoos_ <- purrr::map(.x = periods,
                        .f = ootPredict,
                        sales_df = sales_df,
                        mod_spec = as.formula(subm_lm),
                        model_type = 'linear',
                        train_window = 12,
                        pred_window = 1,
                        subm_hide = .9,
                        subm_labels = c('B', 'I', 'K', 'Q'))
  submlmoos_df <- submlmoos_ %>%
    dplyr::bind_rows()
  median(abs(submlmoos_df$error))
  submlmoos_df %>%
    dplyr::group_by(submarket) %>%
    dplyr::summarize(count = n(),
                     mdape = median(abs(error)))
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
