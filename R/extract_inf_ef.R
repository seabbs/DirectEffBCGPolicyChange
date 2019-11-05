#' Extract incidence estimates
#'
#' @param df A dataframe containing a Model and IRR variables.
#' @param model A Character string identifying the model to extract
#'
#' @return Returns a vector of formatted IRRs formatted for publication
#' @export
#'
#'
#' @importFrom prettypublisher pretty_inline_ci
#' @importFrom dplyr filter select
#' @examples
#' 
extract_inc_ef = function(df, model) {
  df %>% 
    filter(Model %in% model) %>% 
    select(`IRR (CI 95%)*`) %>%
    unlist %>% pretty_inline_ci(note = "95%CrI ")
}