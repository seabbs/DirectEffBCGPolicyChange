## All cleaning is done via the tbinenglanddataclean package see help files for details
## See the package website: http://www.samabbott.co.uk/tbinenglanddataclean for details of data sources
## Raw data needs to be stored in the data-raw folder
library(tidyverse)
library(tbinenglanddataclean)
library(prettypublisher)
library(scales)
library(h2o)

safe_ets_clean <- purrr::safely(clean_munge_ets_2016)

ets <- safe_ets_clean(data_path = "tb_data/ETS/ETS_2016_extract/SAbbott_BCG dataset_version2_final140217.dta",
                     return = TRUE,
                     save = TRUE,
                     save_name = "clean_ets_2016",
                     save_path = "tb_data/tbinenglanddataclean",
                     save_format = c("rds", "csv"),
                     verbose = FALSE)$result

if (!is.null(ets)) {
  ## Evaluate missing data
  ets %>% 
    filter(year %in% 2000:2015) %>% 
    select(ukborn, year, age) %>% 
    map_dbl(~sum(is.na(.))/length(.))
  
  ## Missing data is only an obvious issue for UK birth status with around 7% missing.
  ## Does the missing data change over time
  
  ## Look at missing data by year
  ets %>% 
    count(ukborn, year) %>% 
    add_count(year, wt = n) %>% 
    mutate(prop = n / nn) %>% 
    ggplot(aes(x = year, y = prop, col = ukborn)) +
    scale_y_continuous(labels = percent, limits = c(0,NA), breaks = seq(0, 1, 0.1)) +
    geom_point(size = 1.2) +
    geom_line(alpha = 0.8, size = 1.1) +
    theme_minimal() +
    theme(legend.position = "top") +
    facet_wrap(~ukborn, nrow =1, scales = "free_y")
  
  ## Yes missing data goes down over time, with increasing cases
  ## in both the non-UK born and UK born over this period.
  ## Therefore we need to impute - do this using a nonlinear algorithm including variables reasonably
  ## related to missingness
  
  ## Split data into complete UKborn status and not complete groups
  complete_ets <- ets %>% 
    drop_na(ukborn)
  
  incomplete_ets <- ets %>% 
    filter(is.na(ukborn))
  
  ## Launch h2o
  h2o.init(min_mem_size = "14G",
           nthreads = -1)
  
  ## Split complete_ets into training, calibration and testing sets (80%, 5% and 15%)
  
  splits <- complete_ets %>% 
    as.h2o %>% 
    h2o.splitFrame(c(0.8, 0.05))
  
  train <- splits[[1]]
  calib = splits[[2]]
  test <- splits[[3]]
  
  ## Check the training data
  h2o.describe(train)
  
  ## Identify target and features
  target <- "ukborn"
  features <- c("year", "sex", "age", "phec", "occat", "ethgrp", "natquintile", "riskfactorcount")
  
  
  ## Train GBM - default parameters apart from specifying early stopping and an increased number of trees
  ukborn_model <- h2o.gbm(y = target,
                          x = features,
                          learn_rate = 0.1,
                          learn_rate_annealing = 0.99,
                          training_frame = train,
                          calibration_frame = calib,
                          calibrate_model = TRUE,
                          validation_frame = test,
                          ntrees = 10000,
                          stopping_rounds = 10,
                          stopping_tolerance = 1e-5, 
                          seed = 12345)
  
  ##Summary of model
  summary(ukborn_model)
  
  ## Plot variable importance
  h2o.varimp_plot(ukborn_model)
  
  ## Get model performance on the test set
  h2o.performance(ukborn_model, valid = TRUE)
  
  ## Logloss
  logloss <- h2o.logloss(ukborn_model, valid = TRUE)
  
  ## AUC
  auc <- h2o.auc(ukborn_model, valid = TRUE)
  
  ## Predict UK born status for incomplete data
  ukborn_predictions <- incomplete_ets %>% 
    select(-ukborn) %>% 
    as.h2o %>% 
    {h2o.predict(ukborn_model, .)} %>% 
    as_tibble %>% 
    select(predict)
  
  ## Add back into ets data - add a single dummy missing row for incidence estimation function hard coding
  impute_ets <- complete_ets %>% 
    mutate(ukborn_with_miss = ukborn,
           ukborn_status = "complete") %>% 
    bind_rows(
      incomplete_ets %>% 
        rename(ukborn_with_miss = ukborn) %>% 
        bind_cols(ukborn_predictions) %>% 
        rename(ukborn = predict) %>% 
        mutate(ukborn = factor(ukborn),
               ukborn_status = "incomplete")
    ) %>% 
    mutate(ukborn_status = factor(ukborn_status)) 
  
  ## Evaluate
  dist_ukborn_cases <- impute_ets %>% 
    count(ukborn, ukborn_status) %>% 
    add_count(ukborn_status, wt = n) %>% 
    mutate(prop = n / nn) 
  
  ## Get variable importance as a tibble
  var_importance <- ukborn_model %>% 
    h2o.varimp %>% 
    as_tibble
  
  ## Imputation model evaluation metrics
  imputation_evaluation <- list(logloss = logloss,
                                auc = auc,
                                dist_ukborn_cases = dist_ukborn_cases,
                                var_importance = var_importance)
  
  ## Save metrics to evaluate imputation
  saveRDS(imputation_evaluation, "imputation_evaluation.rds")
  
  ## Save imputed_ets data to disk
  saveRDS(impute_ets, "tb_data/tbinenglanddataclean/directeffectsbcgpolicychange/impute_ets.rds")
  
  ## No. of cases in the study period - birth status
  StudyCasesSum <- ets %>% 
    filter(year %in% 2000:2015) %>% 
    count(ukborn) %>% 
    mutate(per = pretty_round(n/sum(n) * 100, digits = 0))
  
  
  devtools::use_data(StudyCasesSum, overwrite = TRUE)
  readr::write_csv(StudyCasesSum, path = "StudyCasesSum.csv")
}else{
  warning("ETS extract was not found and so aggregated data has not been refreshed.
Unless changes have been made to ETS cleaning this will not impact results")
}


