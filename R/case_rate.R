#' Function to estimate the case rate for a given variable
#'
#' @description Estimates the case rate for a given variable, stratified by given variables,
#'  dependant on age, year, agegrp2, and uk birth status. Missing data for the rate variable
#'   is dropped.
#'
#' @param df A dataframe
#' @param rate_for A character string indicating the variables to estiamte the rate for.
#' @param strat_by A character string indicating the variables to stratify by.
#' @param age_split A character string, defaults to \code{NULL}. Specifies the age group to
#' stratify for.
#' @param CoB_split Logical, defaults to \code{TRUE}. Should the case rate be grouped by the country of
#' birth.
#' @param Year_strat Logical, defaults to \code{TRUE}. Should the case rate be grouped by year.
#'
#' @seealso extract_case_counts
#' @return A dataframe of case rates with upper and lower bounds
#' @export
#'
#'
#' @importFrom dplyr filter_at group_by summarise ungroup group_by rowwise mutate
#' @examples
#' 
case_rate = function(df, rate_for, strat_by = NULL, age_split = NULL,
                     CoB_split = TRUE, Year_strat = TRUE) {
  CaseCountVar <- c('age', 'year', 'ukborn')
  GroupByCase <- c()
  GroupByTotCase <- c()

  if (!is.null(strat_by)) {

    CaseCountVar <- c(CaseCountVar, strat_by)
    GroupByCase <- c(GroupByCase, strat_by)
    GroupByTotCase <- c(GroupByTotCase, strat_by)
  }

  if (!is.null(rate_for)) {

    CaseCountVar <- c(CaseCountVar, rate_for)
    GroupByCase <- c(GroupByCase, rate_for)
  }

  if (!is.null(age_split)) {
    GroupByCase <- c(GroupByCase, age_split)
    GroupByTotCase <- c(GroupByTotCase, age_split)
  }

  if (CoB_split) {
    GroupByCase <- c(GroupByCase, 'CoB')
    GroupByTotCase <- c(GroupByTotCase, 'CoB')
  }

  if (Year_strat) {
    GroupByCase <- c(GroupByCase, 'Year')
    GroupByTotCase <- c(GroupByTotCase, 'Year')
  }


  df %>%
    filter_at(.vars = rate_for, .vars_predicate = all_vars(!is.na(.))) %>%
    extract_case_counts(strat_var = CaseCountVar) %>%
    group_by(.dots = GroupByCase) %>%
    summarise(Cases = sum(Cases, na.rm = TRUE)) %>%
    ungroup %>%
    group_by(.dots = GroupByTotCase) %>%
    mutate(`Total cases` = sum(Cases, na.rm = TRUE)) %>%
    ungroup() %>%
    rowwise %>%
    mutate(`Case rate` = prop.test(Cases, `Total cases`)$estimate * 100,
           LowRate = prop.test(Cases, `Total cases`)$conf.int[1] * 100,
           HiRate = prop.test(Cases, `Total cases`)$conf.int[2] * 100)
}
