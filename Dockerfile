## Start with the tidyverse docker image
FROM rocker/tidyverse:3.5.2

MAINTAINER "Sam Abbott" contact@samabbott.co.uk


## Get JAVA and deb deps
RUN apt-get update -qq \
  && apt-get -y --no-install-recommends install \
    default-jdk \
    default-jre \
    libmagick++-dev \
  && R CMD javareconf
  
ADD . /home/rstudio/DirectEffBCGPolicyChange

WORKDIR /home/rstudio/DirectEffBCGPolicyChange

RUN Rscript -e 'devtools::install_dev_deps(".", dependencies = TRUE)'

RUN Rscript -e 'devtools::install(".")'





