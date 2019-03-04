## Look at incidence rates over time in the general population, and those effected directly by the policy change
GraphStudySpecIncRates <- StudySpecIncRates %>%
  mutate(`Policy change (2005)` = ifelse(Year < 2005, 'Pre change', 'Post change') %>% 
           factor(levels = c('Pre change', 'Post change'))) %>% 
  mutate(CoB = CoB %>% relevel("UK born")) %>% 
  ggplot(aes(
    x = Year,
    y = Incidence,
    colour = `Policy change (2005)`,
    fill = `Policy change (2005)`
  )) +
  geom_pointrange(aes(ymin = Inc_LCI, ymax = Inc_UCI), fatten = 1.1, size = 1) +
  geom_line(aes(group = `Policy change (2005)`), size = 0.8, alpha = 0.6) +
  geom_vline(xintercept = 2004) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Incidence rate (per 100,000)") +
  PaperTheme() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(CoB ~ `Age group`, scale = 'free_y') +
  theme(legend.position = "top")

##  Look at average in cohorts between those Pre and post policy change
##  Look at average in cohorts between those Pre and post policy change
## define the scale of incidence rates
AvgRestCohortVacInc <- RestCohortVacInc %>%
  group_by(VacScheme, PolicyChange, Age, CoB) %>%                                                       
  summarise(
    Incidence = epi_conf(Cases, Population)$est * RateScale,
    LCI = epi_conf(Cases, Population)$lower * RateScale,
    UCI = epi_conf(Cases, Population)$upper * RateScale
  )  %>%
  ungroup %>% 
  mutate(Cohort = case_when(VacScheme %in% "Universal school-age (14)" 
                            & CoB %in% "UK born" &
                              PolicyChange %in% "Pre change" ~ "UNCUK14",
                            VacScheme %in% "Universal school-age (14)" 
                            & CoB %in% "UK born" &
                              PolicyChange %in% "Post change" ~ "UCUK14",
                            VacScheme %in% "Targeted high-risk neonates (0)" 
                            & CoB %in% "UK born" &
                              PolicyChange %in% "Pre change" ~ "TNCUKBirth",
                            VacScheme %in% "Targeted high-risk neonates (0)" 
                            & CoB %in% "UK born" &
                              PolicyChange %in% "Post change" ~ "TCUKBirth",
                            VacScheme %in% "Universal school-age (14)" 
                            & CoB %in% "Non-UK born" &
                              PolicyChange %in% "Pre change" ~ "UNCNUK14",
                            VacScheme %in% "Universal school-age (14)" 
                            & CoB %in% "Non-UK born" &
                              PolicyChange %in% "Post change" ~ "UCNUK14",
                            VacScheme %in% "Targeted high-risk neonates (0)" 
                            & CoB %in% "Non-UK born" &
                            PolicyChange %in% "Pre change" ~ "TNCNUKBirth",
                            VacScheme %in% "Targeted high-risk neonates (0)" 
                            & CoB %in% "Non-UK born" &
                              PolicyChange %in% "Post change" ~ "TCNUKBirth"
                            ) %>% 
           factor
  )

## Exploratory plots
## Plot of average incidence in each cohort, for both Universal and targetted vaccination
## Rates shown are for the appropriate vaccinated popupalation
## Non-UK born are shown for comparision of trend only as they may not have been exposed to the scheme
## Year of eligibility is converted into a factor so that each year is treated independantly
GraphSumPolicyChangeChange <- AvgRestCohortVacInc %>%
  filter(CoB %in% c('UK born', 'Non-UK born')) %>%
  mutate(CoB = CoB %>% relevel("UK born"),
         VacScheme = VacScheme %>% 
           relevel("Universal school-age (14)")) %>% 
  mutate(`Policy Change (2005)` = PolicyChange) %>%
  drop_na(Incidence) %>% 
  ggplot(
    aes(
      x = Age,
      y = Incidence,
      shape = `Policy Change (2005)`,
      colour = `Policy Change (2005)`,
      group = `Policy Change (2005)`
    )
  ) +
  geom_pointrange(aes(min = LCI, max = UCI),
                  fatten = 2,
                  size = 1.05,
                  position = position_dodge(width = 0.3)) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.6) +
  facet_grid(facets = CoB ~ VacScheme, scales = 'free') +
  scale_color_brewer(palette = "Dark2") +
  PaperTheme() +
  labs(y = "Incidence rate (per 100,000)") +
  guides(shape = guide_legend(nrow = 2)) +
  theme(legend.position = "top")

## Facetted by year for targeted vaccination of neonates
GraphTargetedPolicyChangeChange <- RestCohortVacInc %>%
  filter(CoB %in% c('UK born', 'Non-UK born'),
         VacScheme %in% 'Targeted high-risk neonates (0)') %>%
  mutate(`Policy Change (2005)` = PolicyChange) %>%
  ggplot(
    aes(
      x = Age,
      y = Incidence,
      colour = `Policy Change (2005)`,
      group = `Policy Change (2005)`
    )
  ) +
  geom_pointrange(aes(min = Inc_LCI, max = Inc_UCI),
                  fatten = 1.5,
                  size = 1.05,
                  position = position_dodge(width = 0.4)) +
  geom_line(position = position_dodge(width = 0.4), alpha = 0.6) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(facets =  CoB ~ YearEligib, scales = 'free_y') +
  PaperTheme() +
  labs(y = "Incidence rate (per 100,000)") +
  theme(legend.position = 'top')

## Facetted by year for universal vaccination of those at school-age
GraphUniversalPolicyChangeChange <- RestCohortVacInc %>%
  filter(CoB %in% c('UK born', 'Non-UK born'),
         VacScheme %in% 'Universal school-age (14)') %>%
  mutate(`Policy Change (2005)` = PolicyChange) %>%
  ggplot(
    aes(
      x = Age,
      y = Incidence,
      colour = `Policy Change (2005)`,
      group = `Policy Change (2005)`
    )
  ) +
  geom_pointrange(aes(min = Inc_LCI, max = Inc_UCI),
                  fatten = 1.5,
                  size = 1.05,
                  position = position_dodge(width = 0.4)) +
  geom_line(position = position_dodge(width = 0.4), alpha = 0.6) +
  facet_grid(facets =  CoB ~ YearEligib, scales = 'free_y') +
  scale_color_brewer(palette = "Dark2") +
  PaperTheme() +
  labs(y = "Incidence rate (per 100,000)") +
  theme(legend.position = 'top',
        axis.text.x = element_text(angle = 90, hjust = 1))
