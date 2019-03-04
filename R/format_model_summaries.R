#' Summarise models
#'
#' @description Summarise fitted models and bind to descriptive information.
#' @param Models A dataframe of fitted models with summary information
#' @param ModelSpec A dataframe containing descriptive information about each model
#' @param sep A character vector indicating the seperator used between the upper and lower credible intervals.
#'  The default is ', '.
#' @return A dataframe of formatted model summaries, including updated descriptive information.
#' @export
#'
#' @importFrom dplyr mutate ungroup rowwise inner_join select
#' @importFrom prettypublisher pretty_ci
#' @examples
#' 
format_model_summaries = function(Models, ModelSpec, sep = ", ") {
  
  ## Assess Rhat convergance
  Models <- Models %>% 
    rowwise %>% 
    mutate(Convergance = ifelse(length(Warnings) == 0, 'Yes', 'No')) %>% 
    ungroup

  ## Update model descriptions if updated since last fit
  Models <- Models %>% 
    inner_join(ModelSpec, by = c("Model"))
  
  ## Clean up effect sizes
  Models <- Models %>%
    rowwise %>% 
    mutate(Effect = Effect %>% 
             strsplit(split = '/') %>% 
             unlist %>% 
             pretty_ci(string = TRUE, sep = sep)) %>% 
    ungroup
  
  
  ## Remove spurious effects for models that do not include the change in policy
  Models <- Models %>% 
    mutate(Effect = Effect %>% replace(!grepl('PolicyChange', paste0(Predictors)), pretty_ci(0, 0, 0, sep = sep)))
}

