# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:3.6.1

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
    libjpeg-dev \
    poppler-utils \
    libpoppler-cpp-dev \
    texlive-full \
    texlive-latex-recommended \
    texlive-fonts-recommended
    
    # cran/poppler
  

# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('devtools', dependencies=TRUE)"
RUN R -e "install.packages('remotes')"
RUN R -e "install.packages('BiocManager', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('R.matlab', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggrepel', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('foreach', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('doParallel', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('parallel', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('splines', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github(\"r-lib/later\")"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lifecycle', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sortable', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyBS', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyalert', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjqui', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rmarkdown', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('zip', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('d3heatmap', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('pdftools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tinytex', repos='http://cran.rstudio.com/')"
RUN R -e "tinytex::install_tinytex()"
RUN R -e "install.packages('pdftools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinybusy', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('GGally', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github(\"sidchop/brainconn\")"
RUN R -e "install.packages('igraph', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('WGCNA', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dendextend', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gtools', repos='http://cran.rstudio.com/')"

# installing github packages
RUN R -e "devtools::install_github('jzhangc/git_R_STATS_KBS/package/rbioplot')"
RUN R -e "BiocManager::install(c('limma','edgeR'))"
RUN touch /home/.Renviron
RUN echo GITHUB_PAT='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' > /home/.Renviron
RUN R -e "devtools::install_github('jzhangc/git_RBioArray/RBioArray', repos = BiocManager::repositories(), ref = 'beta', auth_token = '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')"
RUN R -e "devtools::install_github('jzhangc/git_RBioFS/RBioFS', repos = BiocManager::repositories(), ref = 'beta')"
RUN R -e "install.packages('kableExtra', repos='http://cran.rstudio.com/')"

# copy the app to the image
COPY app.R /srv/shiny-server/
COPY ./utils /srv/shiny-server/utils
COPY ./backend /srv/shiny-server/backend
COPY results.Rmd /srv/shiny-server/
COPY reg_results.Rmd /srv/shiny-server/
COPY predResults.Rmd /srv/shiny-server/

# select port
EXPOSE 3838

# allow permissions
RUN sudo chown -R shiny:shiny /srv/shiny-server
RUN sudo chmod -R +rw /srv/shiny-server
RUN sudo chmod -R +rw /tmp

# run app
CMD ["/usr/bin/shiny-server.sh"]