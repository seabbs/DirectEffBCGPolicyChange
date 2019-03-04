#' Fit and summarise models
#'
#' @param ModelReadyDataSpec A dataframe of data ready for modelling
#' @param PreCompile Logical, defaults to \code{TRUE}. Should a subset of models be precompiled
#' (specified with \code{BaseLineModelNames}) for the first vaccination scheme and population?
#' @param BaseLineModelsNames A character string of baseline model names.
#' @param FileName A character string to use as the file name for the results.
#' Defaults to ModelResults.
#' @param SubFiles Numeric, defaults to 4. The number of subfiles to split the results into. Results are
#' saved as \code{ModelResults1} etc. This must be smaller or equal to the number of models to be fitted.
#' @param Folder Character string to use as the folder name for the results. 
#' Defaults to "results"
#' @param Path Character string specifying the path to the results folder. 
#' Defaults to ".".
#' @param ReRunAnalysis Logical, default to \code{FALSE}. Should the analysis be rerun.
#' @param verbose Logical defaults to \code{FALSE}. Should progress messages be printed.
#' @param Cores Numeric, defaults to the number of cores available minus 1.
#'  How many cores to use for the analysis.
#' @param Iterations Numeric, the number of iterations to use for the MCMC sampler. Defaults to 
#' 10000.
#' @param BurnIn Numeric, the number of iterations to use for the burn in. Defaults to 5000.
#' @param Alpha Numeric, set the adaptive alpha value for the MCMC. Defaults to 0.99
#' @param Treedepth Numeric, set the maximum tree depth. Defaults to 10.
#' @param UpdateAlpha Numeric, set an updated alpha for models that have failed to fit.
#' Defaults to 0.99999.
#' @param HandleDLLs Logical, defaults to \code{FALSE}. Enables tracking and unloading of Stan DLL 
#' objects. If not reachng R's DLL cap then this is not required.
#' @param UpdateTreedepth Numeric, set the updated maximum tree depth for models that have failed to fit.
#' @seealso clear_user_DLL Base_Model summarise_model_fit
#' @return A dataframe of summarised results for the models specified to be fitted in the input dataframe.
#' @export
#' @importFrom parallel detectCores
#' @importFrom dplyr select mutate filter do inner_join ungroup rename bind_rows bind_cols rowwise
#' @importFrom tidyr nest
#' @importFrom purrr map2 map_dfr
#' @examples
#' 
fit_stan_models = function(ModelReadyDataSpec, PreCompile = TRUE, BaseLineModelsNames = NULL, 
                           FileName = 'ModelResults', SubFiles = 4, Folder = 'results',
                           Path = '.', ReRunAnalysis = FALSE, verbose = TRUE,
                           Cores = NULL, Iterations = 10000, BurnIn = 5000, 
                           Alpha = 0.99, Treedepth = 10, UpdateAlpha = 0.99999,
                           HandleDLLs = FALSE, UpdateTreedepth = 20) {

  #Compile file path
  ## Make multiple files
  FileNames <- paste0(FileName, 1:SubFiles, ".rds")
  ResultsPaths <- file.path(Path, Folder, FileNames) 
  
  if (is.null(Cores)) {
    Cores <- parallel::detectCores() - 1
  }
  
  ##Load previous analysis if it exists and not requested to rerun results
  if (!ReRunAnalysis & all(file.exists(ResultsPaths))) {
    Models <- map_dfr(ResultsPaths, readRDS)
  } else {

    if (HandleDLLs) {
      ## Clear previously used stan models
      clear_user_DLL(verbose = FALSE)
    }

    ## Add Flagging variable for random effects
    ModelReadyDataSpec <- ModelReadyDataSpec %>% 
      mutate(RanEffect = ifelse(`Random effects` %in% '', 'No', 'Yes'))
    
    if (PreCompile) {
      
      ## Compile baseline models 
      BaseLineModels <-  ModelReadyDataSpec %>% 
        filter(VacScheme %in% levels(VacScheme)[1], 
               CoB %in% levels(CoB)[1], 
               Model %in% BaseLineModelsNames) 
      
      BaseLineModels <- BaseLineModels %>% 
        inner_join(BaseLineModels %>% 
                     do(
                       Fit = Base_Model(.$Predictors %>% first, 
                                        ModelFamily = .$ModelFamily %>% first,
                                        Data = ., Silent = TRUE, SetCores = 1, 
                                        Chains = 4, SetIter = 200, SetBurn = 100)
                     ) %>% 
                     select(Model, Fit)
        ) %>%
        ungroup %>% 
        select(ModelFamily, RanEffect, Model, Fit) %>% 
        rename(BaseModel = Model, BaseFit = Fit) %>% 
        unique
      
      if (verbose) { 
        message("Completed compilation of baseline models, the nest step is to fit all unique models.")
      }
      
      if (HandleDLLs) {
        ## Check DLL status
        BaseLineDLLs <- capture_stan_DLLs(verbose)
      }
      
      ## Add base compiled models into modelling data
      ModelReadyDataSpec <- ModelReadyDataSpec %>% 
        inner_join(BaseLineModels)
      
      ## Compile models - first VacScheme, Cob etc 
      CompiledModels <- ModelReadyDataSpec %>% 
        filter(VacScheme %in% levels(VacScheme)[1], CoB %in% levels(CoB)[1]) %>% 
        do(
          Fit = update(object = .$BaseFit %>% first, 
                       formula = .$Predictors %>% first %>% ModelConstruct %>% as.formula, 
                       newdata = .,
                       refresh = 0, 
                       cores = Cores,
                       iter = Iterations, 
                       warmup = BurnIn,
                       control = list(adapt_delta = DeltaAdapt, max_treedepth = TreeMax)
          )
        )
      
      
      if (verbose) {
        message("Completed compilation of all models (first vaccination and country of birth).
                The next step is to fit all remaining models.")
      }
      
      if (HandleDLLs) {
        ## Check DLL status
        AllModelCompDLLs <- capture_stan_DLLs(verbose)
      }
      
      ## Link models into data frame
      ModelReadyDataSpecComModels <- ModelReadyDataSpec %>% 
        inner_join(CompiledModels  %>% 
                     select(Model, Fit) %>% 
                     rename(CompiledModel = Fit), key = 'Model')
      
      ## Update models with new data (other vaccination schemes, Countries of birth etc)
      ModelsWithFits <- ModelReadyDataSpecComModels %>%
        filter(!(VacScheme %in% levels(VacScheme)[1] && CoB %in% levels(CoB)[1])) %>%
        do(
          Fit = update(object = .$CompiledModel %>% first, 
                       newdata = ., 
                       refresh = 0, 
                       cores = Cores,
                       control = list(adapt_delta = DeltaAdapt, max_treedepth = TreeMax)
          )
        )
      
      ## Bind all together
      ModelsWithFits <- CompiledModels %>% 
        bind_rows(ModelsWithFits)
      
    }else{
      
      ModelsWithFits <- ModelReadyDataSpec %>% 
        do(
          Fit = Base_Model(.$Predictors %>% first, 
                           ModelFamily = .$ModelFamily %>% first,
                           Data = ., Silent = TRUE, SetCores = Cores, 
                           Chains = 4, SetIter = Iterations, SetBurn = BurnIn,
                           AdaptDelta = DeltaAdapt, MaxTree = TreeMax)
        )
    }
    
    
    if (verbose) {
      message("All models have been fitted to baseline accuracy.")
    }
    
    if (HandleDLLs) {
      ## Check DLL status
      FitDLLs <- capture_stan_DLLs(verbose)
    }
    
    ## Add Model warnings for convergance issues
    ModelsWithFits <- ModelsWithFits %>% 
      ungroup %>%
      mutate(Warnings = Fit %>% store_warnings) %>% 
      mutate(DivTrans = ifelse(grepl('divergent', Warnings), TRUE, FALSE))
    
    if (verbose) {
      message("Summarising converged models...")
    }
    
    ## Summarise models that have converged with no divergent transitions
    Models <- ModelsWithFits %>%
      filter(!DivTrans) %>% 
      summarise_model_fit(Cores)
    
      ##Print message with the number of models that will be rerun due to divergant transitions - if to many models have divergent transitions the recompilation of models will cause R to reach the DLL buffer, a solution to this is to increase the adapt delta value for the initial modelling run.
      NoModelsDivTrans <- ModelsWithFits %>% filter(DivTrans) %>% nrow
      
    if (NoModelsDivTrans > 0) {
      if (verbose) {
        message(NoModelsDivTrans,' models will be refitted due to divergent transitions, the number of divergent transitions for each model can be viewed in the final output dataframe \n')
        message("These are; ")
        print(ModelsWithFits %>% filter(DivTrans))
        } 
      
      ## Identify models that have divergant transitions and rerun with a smaller alpha step size, update warnings for new fits
      ModelsWithFits <- ModelsWithFits %>% 
        filter(DivTrans) %>%
        select(-Fit) %>% 
        bind_cols(ModelsWithFits %>% 
                    filter(DivTrans) %>% 
                    select(Fit) %>% 
                    rowwise %>% 
                    do(
                      Fit = update(object = .$Fit, 
                                   refresh = 0, 
                                   cores = Cores, 
                                   control = list(adapt_delta = UpdateAlpha, max_treedepth = UpdateTreedepth), 
                                   save_dso  = FALSE)
                    )
        ) %>% 
        ungroup %>% 
        mutate(Warnings = Fit %>% store_warnings)
      
      if (verbose) {
        message("Models that did not converge have been refitted. Summarising updated models...")
      }
      
      ##Update model summary with newly fitted models
      Models <- Models %>% 
        bind_rows(ModelsWithFits %>%
                    summarise_model_fit(Cores)
        )
    }
      
      ## Drop model fits
      Models <- Models %>% 
        select(-Fit)
    
    if (HandleDLLs) {
      ## Check DLLs
      RefitDLLs <- capture_stan_DLLs(verbose)
      
      ## Unload used user DLLs
      UnloadUserDLLs(RefitDLLs, verbose)
    }

    ## Save summarised output
    Models %>% 
      mutate(SubFiles = sample(1:SubFiles, n(), replace = TRUE)) %>% 
      group_by(SubFiles) %>% 
      nest() %>% 
      mutate(save = map2(data, SubFiles, ~saveRDS(.x, file = ResultsPaths[[.y]])))
  }
  
  ##Return summarised models
  return(Models)
}





