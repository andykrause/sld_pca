#***************************************************************************************************
#
#  Functions used in SLD_PCA analysis
#
#***************************************************************************************************

#'
#' Create border flag
#'
#' Creates a binary flag indicating if a sale is near the boundary/border of the fixed effect submarkets
#'
#' @param x_df data.frame of sales
#' @param k [5] number of neighbors to search. If ANY of k are different submarket, then it is
#' labeled a border property
#' @return original data frame (`x_df` object) with a new field ('border') indicating that the
#' property is near the border of submarket boundary
#' @importFrom sp SpatialPointsDataFrame SpatialPoints
#' @importFrom spdep knearneigh knn2b
#' @export

assignBorderFlag <- function(x_df,
                            k = 5){
  
  # Identify "Border" properties
  coords <- cbind(x_df$longitude, x_df$latitude)
  sp_df = SpatialPointsDataFrame(SpatialPoints(coords), x_df)
  nbl <- sp_df %>%
    spdep::knearneigh(., k = k, longlat = NULL, RANN=TRUE) %>%
    spdep::knn2nb(.)
  
  x_df$border <- FALSE
  for (i in 1:nrow(validate_df)){
    nbls <- x_df$submarket[nbl[[i]]]
    if (length(which(nbls != x_df$submarket[i])) > 0){
      x_df$border[i] <- TRUE
    }
  }
  x_df
}




ootPredict <- function(t0,
                       sales_df, 
                       mod_spec,
                       model_type = 'linear',
                       train_window,
                       pred_window,
                       subm_hide = 0,
                       subm_labels = NULL,
                       ...){
  
  cat('Predicting at period:', t0, '\n')
  
  ## Prep data
  oot_df <- sales_df %>% dplyr::filter(trans_period >= t0 - train_window & 
                                         trans_period < t0 + pred_window)
  train_df <- oot_df %>%
    dplyr::filter(trans_period < t0)
  test_df <- oot_df %>%
    dplyr::filter(trans_period >= t0) %>%
    dplyr::mutate(hidden = FALSE) %>%
    dplyr::mutate(trans_period = max(train_df$trans_period))
  
  if (subm_hide > 0){
    subm_df <- train_df %>%
      dplyr::filter(!submarket %in% subm_labels)
    hidden_df <- train_df %>%
      dplyr::filter(submarket %in% subm_labels) %>%
      dplyr::sample_frac(., 1 - subm_hide)
    train_df <- dplyr::bind_rows(subm_df, hidden_df)
    test_df$hidden = ifelse(test_df$submarket %in% subm_labels, TRUE, FALSE)
  }
  
  if (model_type == 'linear'){
    train_mod <- lm(mod_spec, data = train_df)
    mod_preds <- exp(predict(train_mod, test_df))
  } else {
    train_mod <- ranger::ranger(mod_spec,
                                data = train_df,
                                ...)
    mod_preds <- exp(predict(train_mod, test_df)$predictions)
  }
  
  test_df$error = round(log(mod_preds) - log(test_df$price), 3)
  test_df %>% dplyr::select(sale_id, pinx, sale_date, price, submarket, 
                            border, hidden, error)
}