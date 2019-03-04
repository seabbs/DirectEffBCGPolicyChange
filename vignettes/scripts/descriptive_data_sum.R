
TotalStudyCases <- StudyCasesSum$n %>% sum

## Percentage of cases with complete UK birth status
CompleteBirthStatus <- StudyCasesSum %>% 
  filter(!is.na(ukborn)) %>%
  select(n) %>%
  sum
CompleteBirthStatusPer <- CompleteBirthStatus %>% 
  pretty_percentage(TotalStudyCases, digits = 0)

##Cases that were UK born
CasesUKborn <- StudyCasesSum %>% 
  filter(ukborn %in% 'UK Born') %>%
  select(n) %>% 
  sum
CasesUKbornPer <- CasesUKborn %>% 
  pretty_percentage(CompleteBirthStatus, digits = 0)

##Eligible For schemes
eligible_filter <-  function(df, cob_filter = NULL) {
  df %>% 
    filter(CoB %in% cob_filter) %>%
      group_by(VacScheme, PolicyChange) %>% 
      summarise(Cases = sum(Cases), 
                Population = sum(Population), 
                Incidence = epi_conf(Cases, Population)$est * RateScale,
                Inc_LCI = epi_conf(Cases, Population)$lower * RateScale,
                Inc_UCI = epi_conf(Cases, Population)$upper * RateScale,
                PrettyInc = pretty_ci(Incidence, 
                                      lci = Inc_LCI, 
                                      uci = Inc_UCI, 
                                      inline = TRUE,
                                      note = "per 100,000 (95%CI ",
                                      replace_bracket = FALSE)) %>% 
      ungroup
    
}

EligibForSchemes <- RestCohortVacInc %>%
  eligible_filter("UK born")

CasesUniPrePost <- EligibForSchemes %>% 
  filter(VacScheme %in% 'Universal school-age (14)') %>%
  pull(Cases) %>%
  purrr::set_names(c("Pre change", "Post change"))

CasesUni <- CasesUniPrePost %>% 
  sum

CasesUniPer <- CasesUni %>% 
  pretty_percentage(CasesUKborn, digits = 0)

IncUni <- EligibForSchemes %>% 
            filter(VacScheme %in% 'Universal school-age (14)') %>% 
              select(PolicyChange, PrettyInc)

IncUniPreChange <- IncUni %>% 
  filter(PolicyChange %in% 'Pre change') %>%
  select(PrettyInc) %>% 
  unlist
IncUniPostChange <- IncUni %>% 
  filter(PolicyChange %in% 'Post change') %>% 
  select(PrettyInc) %>% 
  unlist

##Eligible Targeted Scheme
CasesTarPrePost <- EligibForSchemes %>% 
  filter(VacScheme %in% 'Targeted high-risk neonates (0)') %>% 
  pull(Cases) %>%
  purrr::set_names(c("Pre change", "Post change"))

CasesTar <- CasesTarPrePost %>% 
  sum

CasesTarPer <- CasesTar %>% 
  pretty_percentage(CasesUKborn, digits = 0)

IncTar <- EligibForSchemes %>% 
  filter(VacScheme %in% 'Targeted high-risk neonates (0)') %>% 
  select(PolicyChange, PrettyInc)

IncTarPreChange <- IncTar %>% 
  filter(PolicyChange %in% 'Pre change') %>% 
  select(PrettyInc) %>% 
  unlist

IncTarPostChange <- IncTar %>%
  filter(PolicyChange %in% 'Post change') %>%
  select(PrettyInc) %>%
  unlist

## Non-UK born summary stats
NUKEligib <- RestCohortVacInc %>%
  eligible_filter("Non-UK born") %>% 
  count(VacScheme, wt = Cases) 

NUKCasesTar <- NUKEligib[[1, 2]]
NUKCasesUni <- NUKEligib[[2, 2]]
  

## Summary stats on incidence directly effected populations
extract_pretty_inc_est = function(df, Year, AgeGroup) {
  df %>%
    filter(`Year eligible for vaccination` %in% Year ) %>% 
    select_at(.vars = AgeGroup) %>% 
    unlist %>% 
    pretty_inline_ci(note = "per 100,000 (95%CI ", replace_bracket = FALSE)
}

Years <- 2000:2015 %>% 
  as.character

UKbornAllCaseInc <- Years %>% 
  map_chr(~extract_pretty_inc_est(UKbornStudySpecIncRates, 
                                  ., 
                                  AgeGroup = 'Age group-All cases')) %>% 
  setNames(nm = Years)

NonUKbornAllCaseInc <- Years %>%
  map_chr(~extract_pretty_inc_est(NonUKbornStudySpecIncRates,
                                  ., 
                                  AgeGroup = 'Age group-All cases')) %>% 
  setNames(nm = Years)
        
UKborn05Inc <- Years %>% 
  map_chr(~extract_pretty_inc_est(UKbornStudySpecIncRates,
                                  ., 
                                  AgeGroup = 'Age group-0-5')) %>% 
  setNames(nm = Years)

NonUKborn05Inc <- Years %>% 
  map_chr(~extract_pretty_inc_est(NonUKbornStudySpecIncRates,
                                  .,
                                  AgeGroup = 'Age group-0-5')) %>% 
  setNames(nm = Years)

UKborn1419Inc <- Years %>% 
  map_chr(~extract_pretty_inc_est(UKbornStudySpecIncRates,
                                  ., 
                                  AgeGroup = 'Age group-14-19')) %>% 
  setNames(nm = Years)

NonUKborn1419Inc <- Years %>% 
  map_chr(~extract_pretty_inc_est(NonUKbornStudySpecIncRates,
                                  .,
                                  AgeGroup = 'Age group-14-19')) %>% 
  setNames(nm = Years)

## Minimum/Maximum incidence in 14-19 cohort
find_incidence = function(df, WhichLoc) {
  Pos <- df %>%
    map(~str_split(., pattern = ' ')) %>%
    flatten %>% 
    map_chr(~.[1]) %>%
    as.numeric %>%
    WhichLoc
  
  df[Pos]
}

UKborn1419MinInc <- UKborn1419Inc %>% find_incidence(which.min)
UKborn1419MaxInc <- UKborn1419Inc %>% find_incidence(which.max)                            
NonUKborn1419MinInc <- NonUKborn1419Inc %>% find_incidence(which.min)
NonUKborn1419MaxInc <- NonUKborn1419Inc %>% find_incidence(which.max)