# Stable base with plain R; we'll control everything explicitly
FROM rocker/r-ver:4.3.2

# System deps for CRAN packages
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -q -e "install.packages(c('plumber','nflreadr','dplyr','stringr'), repos='https://cloud.r-project.org')"

# Copy API files
WORKDIR /app
COPY api.R plumber.R /app/

# Expose port (Render sets $PORT)
EXPOSE 8000

# Start the API explicitly (no special entrypoint magic)
CMD ["Rscript", "/app/plumber.R"]
