FROM rocker/binder:3.5.2

USER root

## Get JAVA and deb deps
RUN apt-get update -qq \
&& apt-get -y --no-install-recommends install \
default-jdk \
default-jre \
libmagick++-dev \
&& R CMD javareconf

COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}

## Become normal user again
USER ${NB_USER}

RUN R -e "devtools::install_dev_deps(); devtools::install()"