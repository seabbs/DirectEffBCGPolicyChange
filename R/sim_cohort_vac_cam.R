
#' Identify those that are eligible for a vaccination program and track age specific incidence rates.
#'
#' @description This function identifies those that are eligible for vaccination programs and tracks their age
#' specific incidence over years since vaccination. It does this by,
#' - Produces a data set with year of eligibility as a variable
#' - Does this for both the universal and targeted schemes
#' - Adds a variables for years since entry to scheme - both universal and targetted
#' - Adds a flag for before or after policy change (i.e 2005 switch)
#'
#' @param df A dataframe
#' @param eligib_vector Numeric year identifying eligibility.
#' @param campaign A character string identifying the name of the campaign.
#' @param policy_change_yr Numeric year identifying the year of the policy change.
#'
#' @return A simulate cohort for a given vaccination program correctly formatted as a
#' retrospective cohort.
#' @export
#' @importFrom dplyr mutate filter
#' @examples
#'
sim_cohort_vac_cam = function(df, eligib_vector, campaign, policy_change_yr = 2005) {

 tmp <-  df %>%
    filter(!(Age %in% c('All cases (crude)', 'All cases (adj)')), !(Year %in% '2016')) %>%
    mutate(YrsFollowUp = Age %>%
             as.character %>%
             replace(Age %in% '90+', '90') %>%
             as.numeric) %>%
    mutate(YrsFollowUp = YrsFollowUp - eligib_vector) %>%
    mutate(YrsFollowUp = ifelse(YrsFollowUp < 0, NA, YrsFollowUp)) %>%
    mutate(YearEligib = as.numeric(Year) - YrsFollowUp) %>%
    mutate(YearEligib = ifelse(YearEligib < min(as.numeric(Year)), NA, YearEligib)) %>%
    mutate(PolicyChange = ifelse(YearEligib < policy_change_yr, 'Pre change', 'Post change') %>% as.factor) %>%
    mutate(VacScheme = campaign) %>%
    filter(!is.na(YrsFollowUp), !(is.na(YearEligib)))

  return(tmp)
}
