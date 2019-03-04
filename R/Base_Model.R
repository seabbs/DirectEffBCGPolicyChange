## Model function to be used across results
## Set MCMC options across models as function defaults
#' Generic BRM model with fitting defaults set
#'
#' @description A generic model used to define default \code{brms} settings to be used across all
#' models investigated.
#' @param Predictors A character vector of variables to use as features
#' @param ModelFamily A character string containing the model family to fit.
#' @param Data A dataframe of the data to fit to.
#' @param SetIter Numeric, define the number of iterations
#' @param SetBurn Numeric, define the number of burn in iterations
#' @param SetCores Numeric, set the number of cores to use.
#' @param Chains Numeric, set the number of chains to use for MCMC
#' @param SetThin Nuemeric, set the number of iterations to thin over.
#' @param Silent Logical, defaults to \code{TRUE}. Should the results be returned without progress messages.
#' @param SaveModel Logical, defaults to \code{TRUE}. Should the model output be saved
#' @param AdaptDelta Numeric, adaptive delta to use
#' @param MaxTree Numeric, the maximum number of trees.
#'
#' @return A \code{brm} fitted model.
#' @export
#' @seealso ModelConstruct
#' @importFrom brms brm
#' @import rstan
#' @import Rcpp
#' @importFrom parallel detectCores
#' @examples
#' 
Base_Model = function(Predictors = NULL, ModelFamily, Data = NULL, SetIter = NULL, SetBurn = NULL,
                      SetCores = NULL, Chains = 4, SetThin = 1,  Silent = TRUE, SaveModel = TRUE,
                      AdaptDelta  = NULL, MaxTree = NULL) {
  
  # Load Rstan
  # set no predictor option
  if (is.null(Predictors)) {
    Predictors <- '1'
  }
  
  if (is.null(SetCores)) {
    SetCores <- parallel::detectCores() - 1 
  }
  ## Stop if no data
  if (is.null(Data)) {
    stop('No data has been specified')
  }
  #Specify model
  Model <- Predictors %>% ModelConstruct
  ## Fit model
  ModelFit <- brm(as.formula(Model), family = ModelFamily, data = Data, chains = Chains,
                  cores = SetCores, iter = SetIter, warmup = SetBurn, thin = SetThin, 
                  refresh = ifelse(Silent, 0, 1), save_dso = SaveModel, 
                  control = list(adapt_delta = AdaptDelta, max_treedepth = MaxTree))
  
  return(ModelFit)
}