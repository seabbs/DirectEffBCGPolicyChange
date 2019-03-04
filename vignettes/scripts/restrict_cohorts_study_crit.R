## Restrict data to those years with 5 complete years of follow up (i.e post 2010) and standardised results for 5 years of follow up
RestCohortVacInc <- CohortVacInc %>% 
  filter(YearEligib <= 2010) %>%
  filter(YrsFollowUp <= 5)

## Estimate total population values
cases_demo_incidence_total <- cases_demo_incidence %>% 
  group_by(Year, CoB) %>% 
  summarise(TotalCases = sum(Cases, na.rm = TRUE), 
            TotalPopulation = sum(Population, na.rm = TRUE)) 


## Add total cases and total population for both Non-UK born and UK-born
cases_demo_incidence_uk_strat <- cases_demo_incidence_total %>%
  spread(CoB, TotalCases) %>% 
  select(Year, `Non-UK born`, `UK born`) %>%
  rename(NonUKCases = `Non-UK born`, UKCases = `UK born`) %>% 
  group_by(Year) %>% 
  summarise(NonUKCases = sum(NonUKCases, na.rm = TRUE), 
            UKCases = sum(UKCases, na.rm = TRUE)) %>% 
  full_join(cases_demo_incidence_total %>%
              spread(CoB, TotalPopulation) %>% 
              select(Year, `Non-UK born`, `UK born`) %>% 
              rename(NonUKPop = `Non-UK born`, UKPop = `UK born`) %>% 
              group_by(Year) %>% 
              summarise(NonUKPop = sum(NonUKPop, na.rm = TRUE), 
                        UKPop = sum(UKPop, na.rm = TRUE)), 
            key = Year
  )

## Bind UK born and non UK born rates back into study populations
## Exclude cases and population in the study population of interest
RestCohortVacInc <- RestCohortVacInc %>% 
  left_join(cases_demo_incidence_uk_strat, key = c(Year)) %>% 
  filter(!is.na(VacScheme)) %>% 
  mutate(UKCases = case_when(CoB %in% "UK born" ~ UKCases - Cases,
                             TRUE ~ UKCases),
         UKPop = case_when(CoB %in% "UK born" ~ UKPop - Population,
                           TRUE ~ UKPop),
         NonUKCases = case_when(CoB %in% "Non-UK born" ~ NonUKCases - Cases,
                                TRUE ~ NonUKCases),
         NonUKPop = case_when(CoB %in% "Non-UK born" ~ NonUKPop - Population,
                           TRUE ~ NonUKPop)
         ) %>% 
  mutate(UKRate = UKCases / UKPop, NonUKRate = NonUKCases / NonUKPop)

## Scale rates
RestCohortVacInc <- RestCohortVacInc %>% 
  mutate_at(.vars = vars(UKRate, NonUKRate), .funs = ~.*RateScale)

## identify rates for study popilations only
StudySpecIncRates <- cases_demo_incidence  %>%
  mutate(
    `Age group` = Age %>%
      as.character %>%
      replace(Age %in% as.character(0:5), '0-5') %>%
      replace(Age %in% as.character(14:19), '14-19')
  ) %>% 
    full_join(cases_demo_incidence_total %>% 
                  ungroup %>% 
                    rename(Cases = TotalCases, Population = TotalPopulation) %>% 
                      mutate(`Age group` = 'All cases'),
              key = c('Year', 'CoB', 'Age group')
              ) %>% 
        mutate(`Age group` = `Age group` %>% factor(levels = c('All cases', '0-5', '14-19')))

StudySpecIncRates <- StudySpecIncRates %>%
                                filter(`Age group` %in% c('0-5', '14-19', 'All cases')) %>%
                                  group_by(`Age group`, CoB, Year) %>%                 
                                    summarise(
                                      Cases = sum(Cases),
                                      Incidence = epi_conf(Cases, Population)$est * RateScale,
                                      Inc_LCI = epi_conf(Cases, Population)$lower * RateScale,
                                      Inc_UCI = epi_conf(Cases, Population)$upper * RateScale,
                                      PrettyInc = pretty_ci(Incidence, lci = Inc_LCI, uci = Inc_UCI, sep = ", ")
                                    )  %>%
                                      ungroup %>% 
                                        filter(CoB %in% c('UK born', 'Non-UK born'), Year < 2016) %>%
                                          select(`Age group`, Year, CoB, Cases, Incidence, Inc_LCI, Inc_UCI, PrettyInc) %>% 
                                            na.omit

##Prettify rates for table
UKbornStudySpecIncRates <- StudySpecIncRates %>% 
                              filter(CoB %in% 'UK born') %>% 
                                select(`Age group`, Year, PrettyInc) %>% 
                                  rename(Incidence = PrettyInc, `Year eligible for vaccination` = Year) %>% 
                                    spread(key = `Age group`, value = Incidence, sep = '-')

NonUKbornStudySpecIncRates <- StudySpecIncRates %>% 
                                filter(CoB %in% 'Non-UK born') %>% 
                                  select(`Age group`, Year, PrettyInc) %>% 
                                    rename(Incidence = PrettyInc, `Year eligible for vaccination` = Year) %>% 
                                      spread(key = `Age group`, value = Incidence, sep = '-')
