# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:3.6.3

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    gfortran \ 
    libgdal-dev \
    gdal-bin\
    libgsl0-dev\
    libudunits2-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Add necessary files

## renv
ENV RENV_VERSION 0.12.0
RUN Rscript -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN mkdir SK-Ana
COPY SK-Ana/.Rprofile /SK-Ana/
COPY SK-Ana/renv/activate.R /SK-Ana/renv/activate.R
COPY SK-Ana/renv.lock /SK-Ana/renv.lock
WORKDIR /SK-Ana
RUN R -e 'renv::consent(TRUE)' \
 && R -e 'renv::restore()'

## app files
COPY SK-Ana/global.R /SK-Ana/
COPY SK-Ana/ui.R /SK-Ana/
COPY SK-Ana/server.R /SK-Ana/
COPY SK-Ana/reportTemplate.Rmd /SK-Ana/
COPY SK-Ana/ui_files /SK-Ana/ui_files/
COPY SK-Ana/server_files /SK-Ana/server_files/
COPY SK-Ana/data /SK-Ana/data/
COPY SK-Ana/outputDir /SK-Ana/outputDir/

# expose port
EXPOSE 3840

# run app on container start
#CMD ["R", "-e", "renv::status();renv:::renv_paths_cache()"]
CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 3840)"]
