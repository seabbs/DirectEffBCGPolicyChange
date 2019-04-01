
#' Make Model Comparison, Filters by Scheme, Country of Birth, and Convergance
#'
#' @param DF A dataframe of summarised model results.
#' @param Scheme A character string specifying the scheme to select models for.
#' @param Pop A character string speccifying the population to select models for.
#' @param Digits Numeric, the number of digits to round to.
#'
#' @seealso format_est_sd
#' @return A summarised table comparing model results
#' @export
#' @importFrom dplyr filter mutate select rename arrange
#' @importFrom prettypublisher pretty_round
#' @importFrom purrr map_lgl
#' @examples
#' 
model_comparison = function(DF, Scheme, Pop, Digits = 3) {
 
  ##Sumarise model components
  ComTable <- DF %>% 
    filter(VacScheme %in% Scheme) %>% 
    filter(CoB %in% Pop) %>%
    filter(Convergance %in% 'Yes') %>%
    mutate(`Policy Change` = ifelse(grepl('PolicyChange', Predictors), 'Yes', 'No')) %>% 
    mutate(`Age` = ifelse(grepl('YrsFollowUp', Predictors), 'Yes', 'No')) %>%
    mutate(`Year of study entry` = ifelse(grepl('YearEligib', Predictors), 'Yes', 'No')) %>%
    mutate(`UK born rates` = ifelse((grepl('UKRate', Predictors, fixed = TRUE) &  !grepl('NonUKRate', Predictors, fixed = TRUE)) |
                                      map_lgl(Predictors, ~sum(grepl('UKRate', ., fixed = TRUE)) == 2) |
                                    (CoB %in% 'UK born' & grepl('TotalRate', Predictors)), 'Yes', 'No')) %>% 
    mutate(`Non-UK born rates` = ifelse(grepl('NonUKRate', Predictors) | (CoB %in% 'Non-UK born' & grepl('TotalRate', Predictors)), 'Yes', 'No'))
  
  ## Arrange and order by fit stats 
  ComTable <-  ComTable %>% 
    select(Model, Effect, LOOIC, LOOICSE, WAIC, WAICSE, `Policy Change`, `Age`,  `UK born rates`, `Non-UK born rates`, `Year of study entry`, df, lpd) %>%
    mutate(`LPD` = lpd %>% pretty_round(0)) %>% 
    select(-lpd) %>% 
    rename(`IRR (CI 95%)*` = Effect) %>%
    rename(`DoF**` = df) %>% 
    rowwise %>% 
    mutate(LOOIC = LOOIC %>% round(0)) %>% 
    mutate(LOOICSE = LOOICSE %>% round(0)) %>% 
    mutate(`LOOIC (se)` = LOOIC %>% format_est_sd(LOOICSE, Digits)) %>%
    mutate(`WAIC (se)` = WAIC %>% format_est_sd(WAICSE, Digits)) %>%
    arrange(LOOIC, `DoF**`) %>% 
    select(-WAIC, -WAICSE, -LOOIC, -LOOICSE, -`WAIC (se)`)
  
  return(ComTable)
}