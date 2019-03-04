## All cleaning is done via the tbinenglanddataclean package see help files for details
## See the package website: http://www.samabbott.co.uk/tbinenglanddataclean for details of data sources
## Raw data needs to be stored in the data-raw folder
library(tidyverse)
library(tbinenglanddataclean)

if (dir.exists("tb_data")) {
  clean_munge_ets_2016(data_path = "tb_data/ETS/ETS_2016_extract/SAbbott_BCG dataset_version2_final140217.dta",
                       return = FALSE,
                       save = TRUE,
                       save_name = "clean_ets_2016",
                       save_path = "tb_data/tbinenglanddataclean",
                       save_format = c("rds", "csv"),
                       verbose = FALSE)
  
  
  ## Set up demographic data if required (or run tbinengladdataclean vignette on your local computer)
  ## ONS
  clean_demographics_uk(data_path = "tb_data/UK_demographics",
                        demo_2000 = "UK_2000_age.csv",
                        demo_2001_2015 = "UK_2001_2015_age.csv",
                        countries = c("E"),
                        return = FALSE,
                        save = TRUE,
                        save_name = "E_demo_2000_2015",
                        save_path = "tb_data/tbinenglanddataclean",
                        save_format = c("rds", "csv"),
                        verbose = FALSE)
  
  
  
  ## LFS
  clean_labour_force_survey(data_path = "tb_data/LFS",
                            return = FALSE,
                            save = TRUE,
                            save_name = "formatted_LFS_2000_2016",
                            save_path = "tb_data/tbinenglanddataclean",
                            save_format = c("rds", "csv"),
                            verbose = FALSE)
  
  
  
  ## Combined ONS, LFS data
  combine_ons_with_lfs(data_path = "tb_data/tbinenglanddataclean",
                       ons_name = "E_demo_2000_2015.rds",
                       lfs_name = "formatted_LFS_2000_2016.rds",
                       countries = "England",
                       return = FALSE,
                       save = TRUE,
                       save_name = "E_ons_lfs_2000_2016",
                       save_path = "tb_data/tbinenglanddataclean",
                       save_format = c("rds", "csv"),
                       verbose = FALSE) 
  
  
  
  ## Calculate incidence and store in the repository folder - note only checks for incidence data
  ## removing other data files will cause an error
  incidence_measures <- calculate_incidence_ets_lfs_ons(data_path = "tb_data/tbinenglanddataclean",
                                                        ets_name = "directeffectsbcgpolicychange/impute_ets.rds",
                                                        demo_name = "E_ons_lfs_2000_2016.rds",
                                                        return = TRUE,
                                                        save = FALSE,
                                                        incidence_name = "incidence" ,
                                                        grouped_incidence_name = "age_grouped_incidence",
                                                        condensed_grouped_incidence_name = "condensed_age_group_incidence",
                                                        cases_demo_incidence_name = "cases_demo_incidence",
                                                        verbose = TRUE)
  
  
  
  ## Incidence data
  incidence <- incidence_measures[[1]]
  
  devtools::use_data(incidence, overwrite = TRUE)
  readr::write_csv(incidence, path = "incidence.csv")
  
  ## Age grouped incidence rates
  age_grouped_incidence <-  incidence_measures[[2]]
  
  
  devtools::use_data(age_grouped_incidence, overwrite = TRUE)
  readr::write_csv(age_grouped_incidence, path = "age_grouped_incidence.csv")
  
  ## Condensed age grouped incidence rates
  condensed_age_group_incidence <-  incidence_measures[[3]]
  
  condensed_age_group_incidence <- condensed_age_group_incidence %>% 
    mutate(CoB = CoB %>% 
             factor(level = c("Total", "Total (LFS)", 
                              "UK born", "Non-UK born"))) 
  
  devtools::use_data(condensed_age_group_incidence, overwrite = TRUE)
  readr::write_csv(condensed_age_group_incidence, path = "condensed_age_group_incidence.csv")
  
  ## Data combinded with cases and populations
  cases_demo_incidence <-  incidence_measures[[4]]
  
  devtools::use_data(cases_demo_incidence, overwrite = TRUE)
  readr::write_csv(cases_demo_incidence, path = "cases_demo_incidence.csv")
  
}else{
  message("TB data folder (tb_data) was not found and so incidence rates have not been refreshed.
Attach the folder containing the required data files is this is desired.")
}
