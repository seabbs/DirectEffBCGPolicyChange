
Estimating the effect of the 2005 change in BCG policy in England: A retrospective cohort study
===============================================================================================

[![Documentation](https://img.shields.io/badge/Documentation-click%20here!-lightgrey.svg?style=flat)](https://www.samabbott.co.uk/DirectEffBCGPolicyChange) [![badge](https://img.shields.io/badge/Launch-Analysis-lightblue.svg)](https://mybinder.org/v2/gh/seabbs/DirectEffBCGPolicyChange/master?urlpath=rstudio) [![DOI](https://zenodo.org/badge/173767331.svg)](https://zenodo.org/badge/latestdoi/173767331)

Abstract
--------

### Background

In 2005, England changed from universal Bacillus Calmette–Guérin (BCG) vaccination of school-age children to targeted BCG vaccination of high-risk children at birth.

#### Methods

We combined notification data from the Enhanced Tuberculosis Surveillance system, with demographic data from the Labour Force Survey to construct retrospective cohorts of individuals in England relevant to both the universal, and targeted vaccination programmes between Jan 1, 2000 and Dec 31, 2010. For each cohort, we estimated incidence rates over a 5 year follow-up period and used Poisson and Negative Binomial regression models in order to estimate the impact of the change in policy on TB.

### Results

In the non-UK born, we found evidence for an association between a reduction in incidence rates and the change in BCG policy (school-age IRR: 0.74 (95%CI 0.61, 0.88), neonatal IRR: 0.62 (95%CI 0.44, 0.88)). We found some evidence that the change in BCG policy was associated with a increase in incidence rates in the UK born school-age population (IRR: 1.08 (95%CI 0.97, 1.19)) and weaker evidence of an association with a reduction in incidence rates in UK born neonates (IRR: 0.96 (95%CI 0.82, 1.14)). Overall, we found that the change in BCG policy was associated with directly preventing 385 (95% CI -105, 881) TB cases.

### Conclusions

Withdrawing universal vaccination at school-age and targeting BCG vaccination towards high-risk neonates was associated with reduced incidence of TB in England. This was largely driven by reductions in the non-UK born. There was a slight increase in UK born school-age cases.

Reproducibility
---------------

### Repository structure

The repository is structured as an R package. It has the following structure:

-   **data-raw:** Raw data processing.
-   \*\*<data:**> Processed data.
-   **R:** Supporting R functions
-   **vignettes:** Analysis paper and results.

### Manual install

-   Install R (analysis run with `3.5.2`) and Rstudio.

-   Download the analysis folder from <https://github.com/seabbs/DirectEffBCGPolicyChange/archive/master.zip> or use `git clone`, as follows, in the command line (not the R terminal).

``` bash
git clone https://github.com/seabbs/DirectEffBCGPolicyChange.git
```

-   Once this has been downloaded click on the project file (`DirectEffBCGPolicyChange.Rproj`).

-   Install the analysis dependencies and build the package using the following. To enable more robust reproducibility consider using the [`checkpoint`](https://cran.r-project.org/web/packages/checkpoint/index.html) package versioned locked to R `3.5.2`.

``` r

#install.packages("devtools")

# To build locally
devtools::install_dev_deps(dependencies = TRUE)
devtools::install()

# Alternatively to remote install
devtools::install_github("seabbs/DirectEffBCGPolicyChange", dependencies = TRUE)
```

-   Load the analysis results by running `vignettes/paper.Rmd`. The complete analysis is not reproducible as the raw data cannot be published, however all interim results are stored in the repository (`data`) and these can be explored.

-   Alternatively the complete analysis (along with documentation) can be reconstructed using `make` in the project root directory.

### Docker

This analysis was developed in a docker container based on the tidyverse docker image. To run the docker image run:

``` bash
docker run -d -p 8787:8787 --name DirectEffBCGPolicyChange -e USER=DirectEffBCGPolicyChange -e PASSWORD=DirectEffBCGPolicyChange seabbs/directeffbcgpolicychange
```

The rstudio client can be found on port :8787 at your local machines ip. The default username:password is DirectEffBCGPolicyChange:DirectEffBCGPolicyChange, set the user with -e USER=username, and the password with - e PASSWORD=newpasswordhere. The default is to save the analysis files into the user directory.

If you have access to the required underlying raw data then the entire analysis can be reproduced from scratch by adding the following to the `docker run` command, with the data saved into `data/tb_data`. The data requirements, and structure, can be found [here](https://www.samabbott.co.uk/tbinenglanddataclean/).

``` bash
--mount type=bind,source=$(pwd)/data/tb_data,target=/home/DirectEffBCGPolicyChange/data/tb_data
```

Alternatively the analysis environment can be accessed via [binder](https://mybinder.org/v2/gh/seabbs/DirectEffBCGPolicyChange/master?urlpath=rstudio).
