
#install.packages("devtools")
library(devtools)

# init stuffs
use_data_raw()
use_cran_badge()
use_mit_license()
use_readme_rmd()
use_testthat()
use_roxygen_md()


#Suggest
use_package("pkgdown", type = "Suggests")
use_package("devtools", type = "Suggests")

#Import


#Vignettes
use_vignette("paper")

##Build site, and make pkgdown
devtools::document()
devtools::build_vignettes()
devtools::build()
pkgdown::build_site()
