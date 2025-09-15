FROM rocker/shiny:4.3.2

# Sys deps
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev \
    libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev \
    libtiff5-dev libjpeg-dev && \
    rm -rf /var/lib/apt/lists/*

# Base R helpers
RUN R -e "install.packages(c('remotes','BiocManager'), repos='https://cloud.r-project.org')"

WORKDIR /srv/shiny-server
# Layer cache: install from DESCRIPTION first
COPY DESCRIPTION ./
# COPY NAMESPACE ./    # uncomment if present

RUN R -e "options(BioC_mirror='https://bioconductor.org'); \
          remotes::install_deps('.', dependencies=TRUE, upgrade='never')"

# Explicitly install Shiny-side deps NOT in your DESCRIPTION
RUN R -e "install.packages(c('shiny','shinyBS','colourpicker','shinyalert', \
                            'DT','shinycssloaders','shinythemes','future','promises'), \
                          repos='https://cloud.r-project.org')"

RUN R -e "must_bioc <- c('msa','Biostrings','edgeR'); \
          still_missing <- setdiff(must_bioc, rownames(installed.packages())); \
          if (length(still_missing)) BiocManager::install(still_missing, update = FALSE, ask = FALSE)"

# App code
COPY . .

# Make files writable by 'shiny'
RUN chown -R shiny:shiny /srv/shiny-server && chmod -R a+rX /srv/shiny-server

#run inside the container to solve permission issue: "chown -R shiny:shiny /srv/shiny-server && chmod -R a+rX /srv/shiny-server"

# Expose default Shiny Server port
EXPOSE 3838

# More transparent errors in logs
RUN printf 'run_as shiny;\nserver {\n  listen 3838;\n  sanitize_errors off;\n  location / { app_dir /srv/shiny-server; directory_index on; }\n}\n' \
    > /etc/shiny-server/shiny-server.conf

# This is the line that fixes the error
RUN sed -i '2i\\tlog_dir /var/log/shiny-server;' /etc/shiny-server/shiny-server.conf

HEALTHCHECK --interval=30s --timeout=5s --start-period=15s --retries=5 \
  CMD wget -qO- http://localhost:3838/ || exit 1

CMD ["/usr/bin/shiny-server"]