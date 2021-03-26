FROM  rocker/shiny:4.0.0
MAINTAINER RodrigoZepeda

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev \
    libxml2-dev \
    libpq-dev \
    libssh2-1-dev

RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('r-spatial/mapview@develop')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leafpop', repos='http://cran.rstudio.com/')"

# select port
EXPOSE 3838


# run app
ADD mapcovid /srv/shiny-server

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server


# CMD ["/usr/bin/shiny-server.sh"]