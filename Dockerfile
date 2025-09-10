# Plain R base; we control everything explicitly
FROM rocker/r-ver:4.3.2

# System libraries needed to compile/install CRAN packages cleanly
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    build-essential pkg-config \
    libsodium-dev \
    libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Use Posit Package Manager for **binary** packages on Ubuntu 22.04 (jammy)
ENV CRAN=https://packagemanager.posit.co/cran/__linux__/jammy/latest

# Install R packages (binary where possible)
RUN R -q -e "options(repos=c(CRAN=Sys.getenv('CRAN'))); install.packages(c('plumber','nflreadr','dplyr','stringr'))"

# App files
WORKDIR /app
COPY api.R plumber.R /app/

# Render sets $PORT
EXPOSE 8000

# Start the API (boring & reliable)
CMD ["Rscript", "/app/plumber.R"]
