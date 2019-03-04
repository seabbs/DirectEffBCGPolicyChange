#' Function to generate results table
#'
#' @param df A dataframe of model results
#' @param Scheme A character string specifying the vaccination scheme to make the 
#' table for.
#' @param Pop A character string indicating which population to make the results table for.
#' @param model_to_select Numeric, defaults to 1. Manually select which model to extract. Useful if several models
#' are nearly comparable.
#' @seealso model_comparision
#' @return A list containing the best fitting model name, the model caption, the tidied effect size 
#' output and the summarised model for inspection
#' @export
#'
#' @inheritParams format_model_summaries
#' @importFrom dplyr first filter select mutate rowwise rename
#' @importFrom purrr flatten
#' @importFrom tibble as_data_frame
#' @importFrom stringr str_replace
#' @examples
#' 
best_fit_results_table = function(df, Scheme, Pop,
                                  sep = ", ",
                                  model_to_select = 1) {
  ## Run model comparision
  ModelCom <- model_comparison(df, Scheme, Pop)
  
  ## Best fitting model
  BestFitModel <- ModelCom %>% first %>% nth(model_to_select)
  
  ## Filter data for this model
  df <- df %>% 
    filter(VacScheme %in% Scheme, CoB %in% Pop, Model %in% BestFitModel)
  
  ## Save caption
  Caption <- df %>% select(Description) %>% unlist

  ## Save Tidied effect size output
  TidySum <- df %>% 
    select(TidySum) %>%
    flatten %>%
    flatten %>% 
    as_data_frame %>% 
    mutate(term = str_replace(term, '_', ' ')) %>% 
    mutate(term = str_replace(term, 'b ', '')) %>% 
    mutate(term = str_replace(term, 'r ', '')) %>%
    mutate(term = str_replace(term, '__', ' ')) %>%
    rowwise %>% 
    mutate(estimate = c(estimate, lower, upper) %>% 
             as.character %>%
             pretty_ci(string = TRUE, sep = sep)) %>%
    select(-std.error, -lower, -upper)

  
  return(list(BestFitModel, Caption, TidySum))   
}


