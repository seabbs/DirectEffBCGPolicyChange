#' Return Model Fit for Plotting
#'
#' @description Extracts a given model and refits it, returning the fitted model object.
#' @param df A dataframe of fitted model results
#' @param ModelBestFit A character string identifying the model to refit.
#' @param Scheme A character string identifying which scheme to refit the model for.
#' @param Pop A character string identifying the population to refit the model for.
#' @inheritParams fit_stan_models
#' @seealso Base_Model
#' @return Fitted model object
#' @export
#' @importFrom dplyr filter do select
#' @importFrom purrr flatten
#' @examples
#' 
best_model_fit = function(df = NULL, ModelBestFit = NULL, 
                          Scheme = NULL, Pop = NULL, Cores = NULL, 
                          Iterations = 10000, BurnIn = 5000, 
                          Alpha = 0.99, Treedepth = 10) {
  ## Filter data for this model
  ModelFit <- df %>% 
    filter(VacScheme %in% Scheme, CoB %in% Pop, Model %in% ModelBestFit) %>% 
    do(
      
      Fit = Base_Model(.$Predictors %>% first,
                       ModelFamily = .$ModelFamily %>% first, 
                       Data = .,
                       Silent = TRUE,  SetIter = Iterations, SetBurn =BurnIn,
                       SetCores = Cores, Chains = 4,
                       AdaptDelta  = Alpha, MaxTree = Treedepth
      )
    ) %>% 
    select(Fit) %>% 
    flatten 
  return(ModelFit)
}