#' Summarise model fit results  
#'
#' @param ModelsWithFits Dataframe containg fitted model objects (stored as a Fit variable).
#' @param cores Numeric, the number of cores to use for parralisation (used by loo). Default
#' is 1.
#' @return A datafrmae containing summarised model fit statistics.
#' @export
#' @importFrom dplyr bind_cols do rowwise mutate_each select mutate_all slice first nth ungroup mutate
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom brms LOO WAIC
#' @examples
#' 
summarise_model_fit = function(ModelsWithFits, Cores = 1) {
  
  ## Summarise model results
  Models <- ModelsWithFits %>% 
    bind_cols(ModelsWithFits  %>% 
                rowwise %>% 
                do(
                   TidySum = .$Fit %>% 
                     tidy(conf.int = TRUE, conf.level = 0.95) %>% 
                     mutate_at(.vars = vars(estimate, lower, upper), .funs = funs(exp(.))), 
                   Effect = .$Fit %>% 
                     tidy(conf.int = TRUE, conf.level = 0.95) %>% 
                     select(estimate, lower, upper) %>%
                     mutate_all(funs(exp(.))) %>% 
                     slice(2) %>% 
                     unlist(., use.names = FALSE) %>%
                     paste(collapse = '/'),
                   WAIC_ob = .$Fit %>% WAIC,
                   LOOIC_ob = .$Fit %>% LOO(reloo = FALSE, cores = Cores))
    ) %>% 
    mutate(LOOIC = LOOIC_ob %>% map(~.$estimates[3, 1]) %>% unlist,
           LOOICSE = LOOIC_ob %>% map(~.$estimates[3, 2]) %>% unlist,
           WAIC = WAIC_ob %>% map(~.$estimates[3, 1]) %>% unlist,
           WAICSE = WAIC_ob %>% map(~.$estimates[3, 2]) %>% unlist
           
    )
  
  
  ## Add computed log pointwise predictive density
  ## Also add estimated effective number of parameters
  Models <- Models %>% 
    mutate(elpd = map_dbl(WAIC_ob, ~.$estimates[1, 1]),
           edf = map_dbl(WAIC_ob, ~.$estimates[2, 1]),
           edf_se = map_dbl(WAIC_ob, ~.$estimates[2, 2]),
           lpd = elpd + edf) %>% 
    select(-elpd, -WAIC_ob, -LOOIC_ob)
  
  ## Deal with bad variable storage
  Models <- Models %>% 
    mutate(WAIC = WAIC %>% as_vector) %>%
    mutate(WAICSE = WAICSE %>% as_vector) %>%
    mutate(LOOIC = LOOIC %>% as_vector) %>%
    mutate(LOOICSE = LOOICSE %>% as_vector)
  
  ## Add model degrees of freedom - dropping lp from tidysum
  Models <- Models %>% 
    rowwise %>% 
    mutate(df = TidySum %>% nrow, df = df - 1) %>% 
    ungroup
  
  return(Models)
}