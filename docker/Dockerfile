FROM openanalytics/r-base

MAINTAINER Nathalie Gerstner "nathalie_gerstner@psych.mpg.de"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    texlive-latex-recommended \
    texlive-latex-extra \
    language-pack-de \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the app
#  RUN apt-get update && apt-get install -y \
#      libmpfr-dev \
#      && rm -rf /var/lib/apt/lists/*

# Docker inheritance
FROM bioconductor/bioconductor_docker:devel  

RUN apt-get update

RUN R -e 'BiocManager::install(ask = F)' && R -e 'BiocManager::install(c("org.Mm.eg.db", \
    ask = F))'

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'markdown', 'shinythemes', 'shinyWidgets', 'shinycssloaders'), \
                           repos = 'https://cloud.r-project.org/', \
                           Ncpus = parallel::detectCores())"

# install dependencies of the diffbrainnet app
RUN R -e "install.packages(c('tidyverse', 'plotly', 'DT', 'data.table', 'scales', 'visNetwork', 'igraph'), \
                           repos='https://cloud.r-project.org/', \
                           Ncpus = parallel::detectCores())"


# copy the app to the image
RUN mkdir /root/diffbrainnet
COPY diffbrainnet /root/diffbrainnet

#COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/diffbrainnet', host = '0.0.0.0', port = 3838)"]

#CMD ["R", "-e", "shiny::runApp('/root/diffbrainnet')"]
