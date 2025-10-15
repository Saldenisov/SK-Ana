# Use Rocker Shiny with R 4.4.x
FROM rocker/shiny:4.4.1

# Workdir for the app
WORKDIR /SK-Ana

# Install R packages needed by the app
RUN R -e "install.packages(c('shiny','shinythemes','shinycssloaders','DT','outliers','nnls','Iso','httpuv','Rsolnp','fields','NMFN','rgenoud','mvtnorm','deSolve','msm','xtable','magrittr','callr','processx','RColorBrewer','viridisLite','changepoint'), repos='https://cloud.r-project.org')"
# Ensure shinyBS is available (CRAN is archived sometimes); try CRAN then fallback to GitHub
RUN R -e "if (!requireNamespace('shinyBS', quietly = TRUE)) { try(install.packages('shinyBS', repos='https://cloud.r-project.org'), silent=TRUE); if (!requireNamespace('shinyBS', quietly=TRUE)) { install.packages('remotes', repos='https://cloud.r-project.org'); remotes::install_github('ebailey78/shinyBS'); } }"

# Copy app files
COPY global.R /SK-Ana/
COPY ui.R /SK-Ana/
COPY server.R /SK-Ana/
COPY reportTemplate.Rmd /SK-Ana/
COPY ui_files /SK-Ana/ui_files/
COPY server_files /SK-Ana/server_files/
COPY data /SK-Ana/data/

# Create output directory inside the container
RUN mkdir -p /SK-Ana/outputDir

# Expose port and run app
# Use PORT from environment
CMD ["R", "-e", "port <- as.integer(Sys.getenv('PORT', '3000')); host <- '0.0.0.0'; message(paste('Starting app on', host, 'port', port)); shiny::runApp('.', host = host, port = port)"]

