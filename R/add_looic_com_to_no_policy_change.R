#' Add LOOIC Comparision for a Model Without the Effect of the Change in Policy
#'
#' @param model A model object as produced by \code{best_fit_model}
#' @param reloo Logical, defaults to \code{TRUE}. Should problematic 
#' observations be refit.
#' @param cores Numeric, defaults to 1. The number of cores to use.
#'
#' @return A list containing the fitted model object and a tibble containing the 
#' comparision LOOIC scores and standard error
#' @export
#' @importFrom tibble tibble
#' @importFrom brms LOO
#' @importFrom prettypublisher pretty_round
#' @examples
#' 
add_looic_com_to_no_policy_change <- function(model, reloo = TRUE, cores = 1) {
  
  model_no_policy_change <- update(model$Fit, . ~ . - PolicyChange, cores = cores)
  
  loo_ic <- LOO(model$Fit, model_no_policy_change, reloo = reloo, cores = cores)
  
  out <- tibble(diff_looic = loo_ic$ic_diffs__[1],
                diff_looic_se = loo_ic$ic_diffs__[2]) %>% 
    mutate(
      pretty_diff_looic = paste0(pretty_round(diff_looic, 2), 
                                 " (SE ",
                                 pretty_round(diff_looic_se, 2),
                                 ")")
    )
  
  model$diff_drop_policy_change <- out
   
  return(model)            
}